#ifndef CBZ_ECS_H_
#define CBZ_ECS_H_

#include <bitset>
#include <cstdint>
#include <stdint.h>
#include <utility>

#include <unordered_map>
#include <vector>

#include <string_view>

template <typename T> constexpr std::string_view GetTypeName() {
#ifdef __clang__
  constexpr std::string_view p = __PRETTY_FUNCTION__;
  constexpr std::string_view prefix = "std::string_view GetTypeName() [T = ";
  constexpr std::string_view suffix = "]";
#elif defined(__GNUC__)
  constexpr std::string_view p = __PRETTY_FUNCTION__;
  constexpr std::string_view prefix =
      "constexpr std::string_view GetTypeName() [with T = ";
  constexpr std::string_view suffix = "]";
#elif defined(_MSC_VER)
  constexpr std::string_view p = __FUNCSIG__;
  constexpr std::string_view prefix = "std::string_view __cdecl GetTypeName<";
  constexpr std::string_view suffix = ">(void)";
#else
#error Unsupported compiler
#endif

  const auto start = p.find(prefix) + prefix.size();
  const auto end = p.rfind(suffix);
  return p.substr(start, end - start);
}

namespace cbz::ecs {

static const uint32_t MAX_ENTITIES = UINT32_MAX - 1;

typedef uint64_t EntityId;
static const EntityId INVALID_ENTITY_ID = UINT64_MAX;

typedef EntityId ComponentId;

static uint8_t sComponentCount = 0;
template <typename T> ComponentId GetComponentId() {
  static const ComponentId id = sComponentCount++;
  return id;
}

constexpr size_t MAX_COMPONENTS = 128; // Adjust based on your needs
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

class ISystem {
public:
private:
};

template <typename T, typename... Args> class System : ISystem {
public:
private:
};

class ArchetypeContainer {
public:
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

  void init();

  void addArchetypeType(ComponentId componentId, uint32_t size);

  void fini();

  Iterator begin();
  Iterator end();

  inline const ComponentBitset &getId() const { return mID; }

  uint32_t getArchetypeSize() const;

  uint32_t getTypeSize(ComponentId id) const;

  uint32_t getTypeOffset(ComponentId id) const;

  bool hasComponents(const ComponentBitset &componentMask) const {
    return (mID & componentMask) == componentMask;
  };

  void *getComponent(EntityId eId, ComponentId componentId);
  void *pushComponent(EntityId eId, ComponentId componentId, void *data);

  void *popArchetype(EntityId eId);

private:
  uint32_t getEntityArchetypeIndex(EntityId eId) const;

private:
  std::unordered_map<ComponentId, uint32_t> mTypeSizes;
  std::unordered_map<ComponentId, uint32_t> mTypeOffsets;

  ComponentBitset mID; // component types hash

  std::unordered_map<EntityId, uint32_t> mEntityIndex;

  uint32_t mArchetypeWholeSize;

  std::vector<uint8_t> mArchetypeBuffer;
  uint32_t mCount;
};

class IWorld {
public:
  [[nodiscard]] virtual EntityId create(const char *name) = 0;
  virtual void destroy(EntityId e) = 0;

  virtual const char *getName(EntityId eId) const = 0;

  [[nodiscard]] virtual void *addComponent(EntityId e, ComponentId componentID,
                                           void *data, uint32_t len) = 0;

  [[nodiscard]] virtual void *getComponent(EntityId e,
                                           ComponentId componentID) = 0;

  virtual void removeComponent(EntityId e, ComponentId componentID) = 0;

  template <typename... Ts, typename Fn> void query(Fn &&fn) {
    for (auto &archetypeIt : mArchetypes) {
      ArchetypeContainer &container = archetypeIt.second;

      if (!container.hasComponents(GetSignature<Ts...>())) {
        continue;
      }

      for (auto it = container.begin(); it != container.end(); ++it) {
        fn(it.template as<Ts>()...);
      }
    }
  }

protected:
  std::unordered_map<ComponentBitset, ArchetypeContainer> mArchetypes;
};

extern cbz::ecs::IWorld *sWorld;
extern cbz::ecs::IWorld *InitWorld();

class Entity {
public:
  Entity(EntityId id) : mId(id) {};
  Entity() = default;
  ~Entity() = default;

  inline EntityId getId() const { return mId; }

  operator EntityId() const { return mId; }

  const char *getName() const { return sWorld->getName(mId); };

  template <typename T, typename... Args> T &addComponent(Args &&...args) {
    static_assert(std::is_standard_layout<T>::value &&
                      std::is_trivially_copyable<T>::value,
                  "Component<T> must be trivial standard data type!");

    T temp{std::forward<Args>(args)...};
    return *static_cast<T *>(
        sWorld->addComponent(getId(), GetComponentId<T>(), &temp, sizeof(T)));
  }

  template <typename T> T &getComponent() {
    return *static_cast<T *>(
        sWorld->getComponent(getId(), GetComponentId<T>()));
  }

  template <typename T> void removeComponent() {
    return *static_cast<T *>(
        sWorld->removeComponent(getId(), GetComponentId<T>()));
  }

private:
  EntityId mId;
};

}; // namespace cbz::ecs

#endif
