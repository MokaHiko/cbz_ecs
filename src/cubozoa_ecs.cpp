#include <cubozoa_ecs/cubozoa_ecs.h>

#include <spdlog/sinks/stdout_color_sinks.h>
#include <spdlog/spdlog.h>

static std::shared_ptr<spdlog::logger> sLogger;

namespace cbz::ecs {

ArchetypeContainer::Iterator::Iterator(uint8_t *dataPtr,
                                       ArchetypeContainer *container)
    : mPtr(dataPtr), mContainer(container) {}

void ArchetypeContainer::init() {}

void ArchetypeContainer::addArchetypeType(ComponentId componentId,
                                          uint32_t size) {
  mTypeSizes[componentId] = size;
  mTypeOffsets[componentId] = mArchetypeWholeSize;

  mArchetypeWholeSize += size;

  mID.set(componentId);
}

void ArchetypeContainer::fini() {
  // TODO : Check Alignment
}

ArchetypeContainer::Iterator ArchetypeContainer::begin() {
  return Iterator(mArchetypeBuffer.data(), this);
}

ArchetypeContainer::Iterator ArchetypeContainer::end() {
  return Iterator(mArchetypeBuffer.data() + (mArchetypeWholeSize * mCount),
                  this);
}

uint32_t ArchetypeContainer::getArchetypeSize() const {
  return mArchetypeWholeSize;
}

uint32_t ArchetypeContainer::getTypeSize(ComponentId id) const {
  const auto it = mTypeSizes.find(id);
  if (it != mTypeSizes.end()) {
    return it->second;
  }

  return 0;
}

uint32_t ArchetypeContainer::getTypeOffset(ComponentId id) const {
  const auto it = mTypeOffsets.find(id);
  if (it != mTypeOffsets.end()) {
    return it->second;
  }

  return 0;
}

void *ArchetypeContainer::getComponent(EntityId eId, ComponentId componentId) {
  const size_t offset = (getEntityArchetypeIndex(eId) * mArchetypeWholeSize) +
                        mTypeOffsets[componentId];
  return static_cast<uint8_t *>(mArchetypeBuffer.data()) + offset;
}

void *ArchetypeContainer::pushComponent(EntityId eId, ComponentId componentId,
                                        void *data) {
  // Increment only if does not exist
  if (mEntityIndex.find(eId) == mEntityIndex.end()) {
    mEntityIndex[eId] = mCount++;
  }

  const size_t offset =
      (mEntityIndex[eId] * mArchetypeWholeSize) + mTypeOffsets[componentId];

  if (offset >= mArchetypeBuffer.size()) {
    // Guarantee free slot at end for pop swap
    mArchetypeBuffer.resize(
        (((mEntityIndex[eId] + 1) * mArchetypeWholeSize) * 2) +
        mArchetypeWholeSize);
  }

  void *out = mArchetypeBuffer.data() + offset;
  memcpy(out, data, mTypeSizes[componentId]);
  return out;
};

void *ArchetypeContainer::popArchetype(EntityId eId) {
  if (mCount == 0) {
    sLogger->error("Attempting to remove component; Entity has no component!");
    return nullptr;
  }

  const size_t offsetToFree =
      (getEntityArchetypeIndex(eId) * mArchetypeWholeSize);
  const size_t tempOffset = (mCount * mArchetypeWholeSize);

  // Store in archetype free 'count' offset
  memcpy(mArchetypeBuffer.data() + tempOffset,
         mArchetypeBuffer.data() + offsetToFree, mArchetypeWholeSize);

  // Copy end to newly freed offset
  const size_t endOffset = (mCount - 1) * mArchetypeWholeSize;
  if (offsetToFree != endOffset) {
    memcpy(mArchetypeBuffer.data() + offsetToFree,
           mArchetypeBuffer.data() + endOffset, mArchetypeWholeSize);
  }

  // Decrement count
  --mCount;

  // Return archetype at tmp offset
  return mArchetypeBuffer.data() + tempOffset;
};

uint32_t ArchetypeContainer::getEntityArchetypeIndex(EntityId eId) const {
  auto it = mEntityIndex.find(eId);
  if (it == mEntityIndex.end()) {
    sLogger->error("Entity has no such component!");
    return -1;
  }

  return it->second;
}

class World : public cbz::ecs::IWorld {
public:
  EntityId create(const char *name) override {
    EntityId id = mEntityDescs.size();
    mEntityDescs.push_back({});

    if (name) {
      mEntityDescs.back().name = name;
    }

    return id;
  }

  // template <typename... Ts, typename Fn> void query(Fn &&fn) {
  //   ArchetypeMask archetypeId = hashAll<Ts...>();
  //
  //   auto archetypeIt = mArchetypes.find(archetypeId);
  //   if (archetypeIt == mArchetypes.end())
  //     return;
  //
  //   ArchetypeContainer &container = archetypeIt->second;
  //
  //   for (auto it = container.begin(); it != container.end(); ++it) {
  //     fn(it.template as<Ts>()...);
  //   }
  // }

  // TODO: Impl
  void destroy(EntityId _) override {};

  const char *getName(EntityId eId) const override {
    return mEntityDescs[eId].name.c_str();
  };

protected:
  void *addComponent(EntityId eId, ComponentId componentId, void *data,
                     uint32_t len) override {
    EntityDesc *desc = &mEntityDescs[eId];

    // Cache component size
    if (mComponentDescs.find(componentId) == mComponentDescs.end()) {
      spdlog::trace("Component define with size {} bytes!", len);
      mComponentDescs[componentId].size = len;
    }

    const ComponentBitset prevArchetypeId = desc->componentMask;
    ComponentBitset newArchetypeId = desc->componentMask;
    newArchetypeId.set(componentId);

    // Check if new archetype
    if (mArchetypes.find(newArchetypeId) == mArchetypes.end()) {
      ArchetypeContainer &container = mArchetypes[newArchetypeId];
      // Add to entity components size
      desc->componentsIds.push_back(componentId);

      container.init();

      for (ComponentId id : desc->componentsIds) {
        container.addArchetypeType(id, mComponentDescs[id].size);
      }

      container.fini();
    } else {
      auto it = std::find(desc->componentsIds.begin(),
                          desc->componentsIds.end(), componentId);

      // Check if entity already has component
      if (it != desc->componentsIds.end()) {
        spdlog::warn("Entity already has component!");
        return getComponent(eId, componentId);
      }

      // Add to entity components size
      desc->componentsIds.push_back(componentId);
    }

    ArchetypeContainer &newContainer = mArchetypes[newArchetypeId];

    if (newArchetypeId != prevArchetypeId) {
      // Remove from previous archetype container
      if (mArchetypes.find(prevArchetypeId) != mArchetypes.end()) {
        ArchetypeContainer &prevContainer = mArchetypes[prevArchetypeId];
        void *prevData = prevContainer.popArchetype(eId);

        // Copy previous type data to new archetype container
        for (ComponentId componentId : desc->componentsIds) {
          newContainer.pushComponent(
              eId, componentId,
              static_cast<uint8_t *>(prevData) +
                  prevContainer.getTypeOffset(componentId));
        }
      }

      desc->componentMask = newArchetypeId;
      return newContainer.pushComponent(eId, componentId, data);
    }

    return newContainer.getComponent(eId, componentId);
  }

  void *getComponent(EntityId eId, ComponentId componentID) override {
    EntityDesc &desc = mEntityDescs[eId];
    return mArchetypes[desc.componentMask].getComponent(eId, componentID);
  }

  void removeComponent(EntityId e, ComponentId componentID) override {
    uint32_t _ = e = componentID;
  }

private:
  struct EntityDesc {
    std::string name;
    ComponentBitset componentMask;
    std::vector<ComponentId> componentsIds;
  };

  std::vector<EntityDesc> mEntityDescs;

  struct ComponentDesc {
    std::string name;
    uint32_t size;
  };

  std::unordered_map<ComponentId, ComponentDesc> mComponentDescs;

  std::vector<cbz::ecs::ISystem> mSystems;
};

cbz::ecs::IWorld *sWorld = nullptr;
cbz::ecs::IWorld *InitWorld() {
  sLogger = spdlog::stdout_color_mt("cbz");
  sLogger->set_level(spdlog::level::trace);
  sLogger->set_pattern("[%^%l%$][CBZ] %v");

  sWorld = new cbz::ecs::World();
  return sWorld;
};

} // namespace cbz::ecs
