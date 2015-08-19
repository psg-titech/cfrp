#ifndef CFRP_MEMORY_H
#define CFRP_MEMORY_H

#include <set>
#include <cstdlib>
#include <stdint.h>

namespace cfrp {

struct object
{
  virtual void mark() const = 0;
  virtual ~object() {}
};

class memory_manager
{
  std::set<intptr_t> allocated_nodes_, allocated_closures_, reachable_;
  size_t total_node_size_, total_closure_size_;
public:
  void *alloc_node(size_t n);
  void *alloc_closure(size_t n);
  inline size_t total_node_size() const { return total_node_size_; }
  inline size_t total_closure_size() const { return total_closure_size_; }
  void garbage_collect();
  void mark(const void *p);
};

extern memory_manager *global_memory;
};
#endif /* end of include guard */
