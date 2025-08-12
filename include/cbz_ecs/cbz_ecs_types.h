#ifndef CBZ_ECS_TYPES_H_
#define CBZ_ECS_TYPES_H_

#include <stdint.h>

namespace cbz::ecs {

static const uint32_t MAX_ENTITIES = UINT32_MAX - 1;

typedef uint32_t EntityId;
static const EntityId INVALID_ENTITY_ID = UINT32_MAX;

constexpr uint64_t MAX_COMPONENTS = 128;
typedef EntityId ComponentId;

} // namespace cbz::ecs

#include <bitset>
#include <cstdint>
#include <functional>
#include <stdint.h>
#include <utility>

#include <unordered_map>
#include <vector>

#include <spdlog/spdlog.h>

namespace cbz::ecs {

// Increments the component ctr and returns an id
uint8_t GenerateNextComponentId();
template <typename T> ComponentId GetComponentId() {
  static const ComponentId id = []() {
    ComponentId newId = GenerateNextComponentId();
#ifdef __clang__
    spdlog::trace("Component<{}>: registered as {}", typeid(T).name(), newId);
#else
    spdlog::trace("Component<{}>: registered as {}", typeid(T).name(), newId);
#endif
    return newId;
  }();
  return id;
}

using ComponentBitset = std::bitset<MAX_COMPONENTS>;
template <typename... Ts> ComponentBitset GetSignature() {
  ComponentBitset bitset;
  (bitset.set(GetComponentId<Ts>()), ...);
  return bitset;
}

struct BitsetHash {
  size_t operator()(const ComponentBitset &bitset) const {
    return std::hash<std::string>()(bitset.to_string());
  }
};

class IWorld;
class ArchetypeContainer {
public:
  void init(IWorld *world);
  void addArchetypeType(ComponentId componentId, uint32_t size,
                        uint32_t alignmnent);
  void fini();

  [[nodiscard]] inline const ComponentBitset &getId() const {
    return mComponentMask;
  }

  [[nodiscard]] uint32_t getTypeSize(ComponentId id) const;
  [[nodiscard]] uint32_t getTypeOffset(ComponentId id) const;
  [[nodiscard]] uint32_t getComponentCount() const;
  [[nodiscard]] uint32_t getArchetypeSize() const;
  [[nodiscard]] uint32_t getEntityArchetypeIndex(EntityId eId) const;

  [[nodiscard]] bool hasComponents(const ComponentBitset &componentMask) const {
    return (mComponentMask & componentMask) == componentMask;
  };

  [[nodiscard]] void *getComponent(EntityId eId, ComponentId componentId);

  void *pushComponent(EntityId e, ComponentId componentId, void *data);
  void *popArchetype(EntityId eId);

  class Iterator {
  public:
    Iterator(uint8_t *dataPtr, ArchetypeContainer *container);

    Iterator &operator++() {
      mPtr += mContainer->mArchetypeWholeSize;
      return *this;
    }

    template <typename T> inline T &as() {
      uint32_t offset = mContainer->mTypeOffsets[GetComponentId<T>()];
      return *(reinterpret_cast<T *>(mPtr + offset));
    }

    bool operator!=(const Iterator &other) const { return mPtr != other.mPtr; }

  private:
    uint8_t *mPtr;
    ArchetypeContainer *mContainer;
  };

  Iterator begin();
  Iterator end();

private:
  std::unordered_map<ComponentId, uint32_t> mTypeSizes;
  std::unordered_map<ComponentId, uint32_t> mTypeOffsets;

  ComponentBitset mComponentMask; // component types hash
  std::unordered_map<EntityId, uint32_t> mEntityIndex;

  uint32_t mArchetypeWholeSize;

  std::vector<uint8_t> mArchetypeBuffer;
  uint32_t mCount;

  IWorld *mWorld;
};

class Entity;
class IWorld {
public:
  IWorld() = default;
  virtual ~IWorld() = default;

  [[nodiscard]] virtual Entity instantiate(const char *name = nullptr) = 0;
  virtual void destroy(EntityId e) = 0;

  virtual const char *getName(EntityId eId) const = 0;
  virtual void setName(EntityId eId, const char *name) = 0;

  [[nodiscard]] virtual void *addComponent(EntityId e, ComponentId componentID,
                                           void *data, uint32_t len,
                                           uint32_t alignment) = 0;

  [[nodiscard]] virtual void *getComponent(EntityId e,
                                           ComponentId componentID) = 0;

  [[nodiscard]] virtual bool hasComponent(EntityId e,
                                          ComponentId componentID) const = 0;

  virtual void forEachEntity(std::function<void(EntityId)>) = 0;

  virtual void removeComponent(EntityId e, ComponentId componentID) = 0;

  virtual void step(double deltaTime) = 0;

  template <typename... Ts, typename Fn> void query(Fn &&fn) {
    static_assert((std::is_standard_layout_v<Ts> && ...),
                  "All Ts must be standard layout");
    static_assert((std::is_trivially_copyable_v<Ts> && ...),
                  "All Ts must be trivially copyable");

    // Query cache
    static std::vector<ArchetypeContainer *> sCachedContainers;
    static ComponentBitset sSignature = GetSignature<Ts...>();

    // Check if query already cached
    if (!sCachedContainers.empty()) {
      for (ArchetypeContainer *container : sCachedContainers) {
        for (auto it = container->begin(); it != container->end(); ++it) {
          fn(it.template as<Ts>()...);
        }
      }

    } else { // First query
      for (auto &archetypeIt : mArchetypes) {
        ArchetypeContainer *container = &archetypeIt.second;

        if (!container->hasComponents(sSignature)) {
          continue;
        }

        // Cache query containers
        sCachedContainers.push_back(container);
        for (auto it = container->begin(); it != container->end(); ++it) {
          fn(it.template as<Ts>()...);
        }
      }
    }
  }

  void system(std::function<void(ecs::IWorld *)> &&oncePerStepFn) {
    mySystems.push_back([fn = std::move(oncePerStepFn), this]() { fn(this); });
  }

  template <typename... Ts, typename Fn> void system_for_each(Fn &&forEachFn) {
    mySystems.push_back(
        {[this, fn = std::forward<Fn>(forEachFn)]() { query<Ts...>(fn); }});
  }

protected:
  std::vector<std::function<void()>> mySystems;
  std::unordered_map<ComponentBitset, ArchetypeContainer> mArchetypes;
};

// TODO: Make entity hold reference to world
extern cbz::ecs::IWorld *InitWorld();

class Entity {
public:
  Entity(EntityId id, IWorld *world) : mId(id), mWorld(world) {};
  Entity() = default;
  ~Entity() = default;

  inline EntityId getId() const { return mId; }

  operator EntityId() const { return mId; }
  operator bool() const { return mWorld && mId != cbz::ecs::INVALID_ENTITY_ID; }

  const char *getName() const { return mWorld->getName(mId); };
  void setName(const char *name) { return mWorld->setName(mId, name); };

  void destroy() {
    mWorld->destroy(mId);
    mId = INVALID_ENTITY_ID;
  }

  template <typename T, typename... Args> T &addComponent(Args &&...args) {
    static_assert(std::is_standard_layout<T>::value &&
                      std::is_trivially_copyable<T>::value,
                  "Component<T> must be trivial standard data type!");

    T temp{std::forward<Args>(args)...};
    return *static_cast<T *>(mWorld->addComponent(
        getId(), GetComponentId<T>(), &temp, sizeof(T), alignof(T)));
  }

  template <typename T> [[nodiscard]] T &getComponent() {
    return *static_cast<T *>(
        mWorld->getComponent(getId(), GetComponentId<T>()));
  }

  template <typename T> [[nodiscard]] bool hasComponent() const {
    return mWorld->hasComponent(getId(), GetComponentId<T>());
  }

  template <typename T> void removeComponent() {
    return *static_cast<T *>(
        mWorld->removeComponent(getId(), GetComponentId<T>()));
  }

private:
  EntityId mId;
  IWorld *mWorld;
};

}; // namespace cbz::ecs

// --- ECS Common ---
#include <glm/glm.hpp>
#include <glm/gtc/type_ptr.hpp>

struct Position {
  float x = 0;
  float y = 0;
  float z = 0;

  operator glm::vec3() const { return glm::vec3(x, y, z); }
};

struct Scale {
  float x = 1.0f;
  float y = 1.0f;
  float z = 1.0f;

  operator glm::vec3() const { return glm::vec3(x, y, z); }
};

// A quaternion rotation
struct Rotation {
  float w = 1.0f;

  float x = 0.0f;
  float y = 0.0f;
  float z = 0.0f;

  operator glm::quat() const { return glm::quat(w, x, y, z); }

  // Sets the rotation given euler angles in RADIANS.
  void setEuler(float xRadians, float yRadians, float zRadians) {
    glm::quat rotX = glm::angleAxis(xRadians, glm::vec3(1, 0, 0));
    glm::quat rotY = glm::angleAxis(yRadians, glm::vec3(0, 1, 0));
    glm::quat rotZ = glm::angleAxis(zRadians, glm::vec3(0, 0, 1));

    glm::quat rotation = rotZ * rotY * rotX; // ZYX order

    // Update this object
    w = rotation.w;
    x = rotation.x;
    y = rotation.y;
    z = rotation.z;
  }
};

class Transform {
public:
  inline cbz::ecs::Entity getParent() const { return mParent; }
  void setParent(cbz::ecs::Entity parentId);

  // @returns the transform matrix
  glm::mat4 get() const { return mMatrix; }
  void set(const glm::mat4 &mat) { mMatrix = mat; }

  // Returns the depth the transform in the  global heierarchy
  inline uint32_t getDepth() const { return mDepth; }

private:
  glm::mat4 mMatrix = glm::mat4(1.0f);
  cbz::ecs::Entity mParent = {cbz::ecs::INVALID_ENTITY_ID, nullptr};
  uint32_t mDepth = 0;
};

#endif // CBZ_ECS_TYPES_H_
