#include <cbz_ecs/cbz_ecs.h>
#include <cbz_ecs/cbz_ecs_types.h>

#include <spdlog/sinks/stdout_color_sinks.h>
#include <spdlog/spdlog.h>

#include <algorithm>

void Transform::setParent(cbz::ecs::Entity parentId) {
  cbz::ecs::Entity parent(parentId);

  if (!parent.hasComponent<Transform>()) {
    spdlog::error(
        "Attempting to parent to entity without transfrom component!");
  }

  mDepth = parent.getComponent<Transform>().mDepth + 1;
  mParent = parentId;
}

namespace cbz::ecs {

uint8_t GenerateNextComponentId() {
  static uint8_t sComponentCount = 0;
  return sComponentCount++;
}

ArchetypeContainer::Iterator::Iterator(uint8_t *dataPtr,
                                       ArchetypeContainer *container)
    : mPtr(dataPtr), mContainer(container) {}

void ArchetypeContainer::init(IWorld *world) {
  mWorld = world;
  // Prepend archetype with entityID
  addArchetypeType(GetComponentId<Entity>(), sizeof(Entity), alignof(Entity));
}

void ArchetypeContainer::addArchetypeType(ComponentId componentId,
                                          uint32_t size, uint32_t alignment) {
  // Ensure next offset fits with allignment
  mArchetypeWholeSize =
      (mArchetypeWholeSize + alignment - 1) & ~(alignment - 1);

  mTypeSizes[componentId] = size;
  mTypeOffsets[componentId] = mArchetypeWholeSize;

  mArchetypeWholeSize += size;
  mComponentMask.set(componentId);
}

void ArchetypeContainer::fini() {
  // Enforce 8 byte allignment
  mArchetypeWholeSize = (mArchetypeWholeSize + 7) & ~7;
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

uint32_t ArchetypeContainer::getComponentCount() const {
  return static_cast<uint32_t>(mTypeSizes.size());
}

void *ArchetypeContainer::getComponent(EntityId eId, ComponentId componentId) {
  const size_t offset = (getEntityArchetypeIndex(eId) * mArchetypeWholeSize) +
                        mTypeOffsets[componentId];
  return static_cast<uint8_t *>(mArchetypeBuffer.data()) + offset;
}

void *ArchetypeContainer::pushComponent(cbz::ecs::EntityId eId,
                                        ComponentId componentId, void *data) {
  // Increment only if does not exist
  if (mEntityIndex.find(eId) == mEntityIndex.end()) {
    mEntityIndex[eId] = mCount++;
  }

  const size_t globalComponentOffset =
      (mEntityIndex[eId] * mArchetypeWholeSize) + mTypeOffsets[componentId];

  if (globalComponentOffset >= mArchetypeBuffer.size()) {
    // Guarantee free 'temp' slot at end for pop swap
    mArchetypeBuffer.resize(
        (((mEntityIndex[eId] + 1) * mArchetypeWholeSize) * 2) +
        mArchetypeWholeSize);
  }

  // Preprend entity
  // Entity is always the first component in the archetype
  Entity e(eId, mWorld);
  memcpy(mArchetypeBuffer.data() + (mEntityIndex[eId] * mArchetypeWholeSize),
         &e, sizeof(cbz::ecs::Entity));

  // Copy component data
  void *out = mArchetypeBuffer.data() + globalComponentOffset;
  memcpy(out, data, mTypeSizes[componentId]);

  return out;
};

void *ArchetypeContainer::popArchetype(EntityId eId) {
  if (mCount == 0) {
    spdlog::error("Attempting to remove component; Entity has no component!");
    return nullptr;
  }

  const size_t offsetToFree =
      (getEntityArchetypeIndex(eId) * mArchetypeWholeSize);
  const size_t tempOffset = (mCount * mArchetypeWholeSize);

  // Guarantee required size
  const size_t requiredSize = (mCount + 1) * mArchetypeWholeSize;
  if (mArchetypeBuffer.size() < requiredSize) {
    mArchetypeBuffer.resize(requiredSize);
  }

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
    spdlog::error("Entity has no such component!");
    return -1;
  }

  return it->second;
}

class World : public cbz::ecs::IWorld {
public:
  World() = default;
  ~World() = default;

  Entity instantiate(const char *name) override {
    if (mEntityDescs.size() >= MAX_ENTITIES) {
      spdlog::error("Exceeded max entities");
      return {INVALID_ENTITY_ID, this};
    }

    EntityId id = static_cast<uint32_t>(mEntityDescs.size());
    mEntityDescs.push_back({});

    if (name) {
      mEntityDescs.back().name = name;
    }

    return {id, this};
  }

  void step([[maybe_unused]] double _) override {
    for (auto &systemFn : mySystems) {
      systemFn();
    }
  };

  void destroy(EntityId eId) override {
    if (mArchetypes.find(mEntityDescs[eId].componentMask) ==
        mArchetypes.end()) {
      return;
    }

    mArchetypes[mEntityDescs[eId].componentMask].popArchetype(eId);
  };

  const char *getName(EntityId eId) const override {
    if (eId >= mEntityDescs.size() || eId == INVALID_ENTITY_ID) {
      return "<Uknown/Invalid Entity>!";
    }

    return mEntityDescs[eId].name.c_str();
  };

  void setName(EntityId eId, const char *name) override {
    if (eId >= mEntityDescs.size() || eId == INVALID_ENTITY_ID) {
      spdlog::error("Cannot rename invalid entity!");
      return;
    }

    mEntityDescs[eId].name = name;
  };

protected:
  void *addComponent(EntityId eId, ComponentId componentId, void *data,
                     uint32_t len, uint32_t alignment) override {
    EntityDesc *desc = &mEntityDescs[eId];

    // Cache component size
    if (mComponentDescs.find(componentId) == mComponentDescs.end()) {
      spdlog::trace("Component defined with {} bytes!", len);
      mComponentDescs[componentId].size = len;
      mComponentDescs[componentId].alignment = alignment;
    }

    const ComponentBitset prevArchetypeId = desc->componentMask;
    ComponentBitset newArchetypeId = desc->componentMask;
    newArchetypeId.set(componentId);

    // Check if new archetype
    if (mArchetypes.find(newArchetypeId) == mArchetypes.end()) {
      ArchetypeContainer &newContainer = mArchetypes[newArchetypeId];
      newContainer.init(this);

      // Previous components
      for (ComponentId id : desc->componentsIds) {
        newContainer.addArchetypeType(id, mComponentDescs[id].size,
                                      mComponentDescs[id].alignment);
      }

      // New component
      newContainer.addArchetypeType(componentId,
                                    mComponentDescs[componentId].size,
                                    mComponentDescs[componentId].alignment);

      newContainer.fini();
    } else {
      auto it = std::find(desc->componentsIds.begin(),
                          desc->componentsIds.end(), componentId);

      // Check if entity already has component
      if (it != desc->componentsIds.end()) {
        spdlog::warn("Entity already has component!");
        return getComponent(eId, componentId);
      }
    }

    ArchetypeContainer &newContainer = mArchetypes[newArchetypeId];

    // Check if switched archetype
    if (newArchetypeId != prevArchetypeId) {
      // Remove from previous archetype container
      if (mArchetypes.find(prevArchetypeId) != mArchetypes.end()) {
        ArchetypeContainer &prevContainer = mArchetypes[prevArchetypeId];
        void *prevData = prevContainer.popArchetype(eId);

        // Copy overlapping components to new archetype container
        for (ComponentId componentId : desc->componentsIds) {
          newContainer.pushComponent(
              eId, componentId,
              static_cast<uint8_t *>(prevData) +
                  prevContainer.getTypeOffset(componentId));
        }
      }

      // Update entity description
      desc->componentsIds.push_back(componentId);
      desc->componentMask = newArchetypeId;
      return newContainer.pushComponent(eId, componentId, data);
    }

    return newContainer.getComponent(eId, componentId);
  }

  void *getComponent(EntityId eId, ComponentId componentID) override {
    EntityDesc &desc = mEntityDescs[eId];

    if (!hasComponent(eId, componentID)) {
      spdlog::error("Entity has no component #{}", componentID);
      return NULL;
    }

    return mArchetypes[desc.componentMask].getComponent(eId, componentID);
  }

  bool hasComponent(EntityId eId, ComponentId componentID) const override {
    const EntityDesc &desc = mEntityDescs[eId];

    ComponentBitset componentMask = {};
    componentMask.set(componentID);
    if ((desc.componentMask & componentMask) != componentMask) {
      return false;
    }

    return true;
  }

  void forEachEntity(std::function<void(EntityId)> fn) override {
    for (size_t i = 0; i < mEntityDescs.size(); i++) {
      fn(EntityId{static_cast<uint32_t>(i)});
    }
  };

  void removeComponent(EntityId e, ComponentId componentID) override {
    uint32_t _ = e = componentID;
  }

private:
  struct EntityDesc {
    std::string name;
    ComponentBitset componentMask;

    // TODO: Remove only use bitmask
    std::vector<ComponentId> componentsIds;
  };

  std::vector<EntityDesc> mEntityDescs;

  struct ComponentDesc {
    std::string name;
    uint32_t size;
    uint32_t alignment;
  };

  std::unordered_map<ComponentId, ComponentDesc> mComponentDescs;
};

cbz::ecs::IWorld *InitWorld() { return new cbz::ecs::World(); };

} // namespace cbz::ecs
