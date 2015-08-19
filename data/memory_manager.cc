#include "memory_manager.h"
#include <cstdio>
#include <vector>

namespace cfrp {

void *memory_manager::alloc_node(size_t n)
{
  void *p = malloc(n);
  total_node_size_ += n;
  allocated_nodes_.insert(reinterpret_cast<intptr_t>(p));
#ifdef DEBUG_MEMORY
  std::printf("alloc node %zu: %p\n", n, p);
#endif
  return p;
}

void *memory_manager::alloc_closure(size_t n)
{
  void *p = malloc(n);
  total_closure_size_ += n;
  allocated_closures_.insert(reinterpret_cast<intptr_t>(p));
#ifdef DEBUG_MEMORY
  std::printf("alloc closure %zu: %p\n", n, p);
#endif
  return p;
}

void memory_manager::garbage_collect()
{
  reachable_.clear();
  for (std::set<intptr_t>::const_iterator it = allocated_nodes_.begin(); it != allocated_nodes_.end(); ++it) {
    mark(reinterpret_cast<const void *>(*it));
  }

#ifdef DEBUG_MEMORY
  std::printf("Allocated %zu closures, marked %zu closures\n", allocated_closures_.size(), reachable_.size());
#endif

  std::vector<intptr_t> garbage;
  for (std::set<intptr_t>::const_iterator it = allocated_closures_.begin(); it != allocated_closures_.end(); ++it) {
    if (!reachable_.count(*it)) {
      garbage.push_back(*it);
    }
  }
#ifdef DEBUG_MEMORY
  std::printf("  %zu garbage closures\n", garbage.size());
#endif
  for (std::vector<intptr_t>::const_iterator it = garbage.begin(); it != garbage.end(); ++it) {
    allocated_closures_.erase(*it);
    delete reinterpret_cast<object *>(*it);
  }
}

void memory_manager::mark(const void *p)
{
  const intptr_t t = reinterpret_cast<intptr_t>(p);
  if (reachable_.count(t)) {
    return;
  }
  if (allocated_nodes_.count(t)) {
    reinterpret_cast<const object *>(p)->mark();
  } else if (allocated_closures_.count(t)) {
    reachable_.insert(t);
    reinterpret_cast<const object *>(p)->mark();
  }
}

};
