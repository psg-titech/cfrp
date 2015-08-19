#ifndef CFRP_RUNTIME_H
#define CFRP_RUNTIME_H

#include <cstdio>
#include <vector>
#include <queue>
#include "memory_manager.h"

namespace cfrp {

struct closure;
class node;

union value
{
  int int_value;
  closure *closure_value;
  node *node_value;

  value() : int_value(0) {}
  value(int n) : int_value(n) {}
  value(closure *c) : closure_value(c) {}
  value(node *n) : node_value(n) {}
};

struct closure : public object
{
  void *operator new(size_t n)
  {
    return ::cfrp::global_memory->alloc_closure(n);
  }

  virtual ~closure() {}

  virtual value operator()(value x) = 0;

  inline value operator()(value x1, value x2)
  {
    return operator()(x1).closure_value->operator()(x2);
  }

  inline value operator()(value x1, value x2, value x3)
  {
    return operator()(x1, x2).closure_value->operator()(x3);
  }
};

struct event
{
  bool changed;
  value data;
  event(bool b, value d) : changed(b), data(d) {}
};

class node : public object
{
protected:
  size_t nreceived_;
  std::vector<event> received_;
  std::vector<std::pair<node *, size_t> > kids_;

public:
  node() : nreceived_(0), received_(), kids_()
  {
#ifdef DEBUG_MEMORY
    std::fprintf(stderr, "Allocate node %p\n", this);
#endif
  }

  virtual ~node() {}

  void *operator new(size_t n)
  {
    return global_memory->alloc_node(n);
  }

  void add(node *n)
  {
    kids_.push_back(std::make_pair(n, n->received_.size()));
    n->received_.push_back(event(false, value()));
  }

  std::vector<std::pair<node *, size_t> >& kids() { return kids_; }

  void clear_events()
  {
    nreceived_ = 0;
  }

  void receive(int idx, const event& e)
  {
    received_[idx] = e;
    ++nreceived_;
  }

  bool ready() const
  {
    return nreceived_ == received_.size();
  }

  virtual event process() = 0;
};

class lift_node : public node
{
  closure *f_;
  value prev_;

public:
  lift_node(closure *f) : f_(f), prev_(0) {}
  virtual ~lift_node() {}
  virtual event process()
  {
    bool changed = false;
    for (std::vector<event>::const_iterator it = received_.begin(); it != received_.end(); ++it) {
      if (it->changed) {
        changed = true;
        break;
      }
    }
    if (changed) {
      prev_ = f_;
      for (std::vector<event>::const_iterator it = received_.begin(); it != received_.end(); ++it) {
        prev_ = (*prev_.closure_value)(it->data);
      }
    }
    return event(changed, prev_);
  }

  virtual void mark() const
  {
    global_memory->mark(f_);
    global_memory->mark(prev_.closure_value);
  }
};

template <unsigned N>
class lift_collect_closure : public closure
{
  closure *f_;
  const std::vector<node *> nodes_;

public:
  lift_collect_closure(closure *f, const std::vector<node *>& nodes) : f_(f), nodes_(nodes) {}

  value operator()(value s)
  {
    std::vector<node *> nodes = nodes_;
    nodes.push_back(s.node_value);
    if (nodes.size() == N) {
      node *n = new lift_node(f_);
      for (unsigned i = 0; i < N; i++) {
        nodes[i]->add(n);
      }
      return n;
    } else {
      return new lift_collect_closure<N>(f_, nodes);
    }
  }

  virtual void mark() const
  {
    global_memory->mark(f_);
    for (std::vector<node *>::const_iterator it = nodes_.begin(); it != nodes_.end(); ++it) {
      global_memory->mark(*it);
    }
  }
};

template <unsigned N>
class lift_closure : public closure
{
public:
  value operator()(value f)
  {
    return new lift_collect_closure<N>(f.closure_value, std::vector<node *>());
  }

  virtual void mark() const
  {
  }
};

class foldp_node : public node
{
  closure *f_;
  value acc_;

public:
  foldp_node(closure *f, value n) : f_(f), acc_(n) {}
  virtual ~foldp_node() {}

  virtual event process()
  {
    if (received_[0].changed) {
      acc_ = (*f_)(received_[0].data, acc_);
    }
    return event(received_[0].changed, acc_);
  }

  virtual void mark() const
  {
    global_memory->mark(f_);
    global_memory->mark(acc_.closure_value);
  }
};

class foldp_closure_2 : public closure
{
  closure *f_;
  value init_;
public:
  foldp_closure_2(closure *f, value init) : f_(f), init_(init) {}

  value operator()(value sig)
  {
    foldp_node *n = new foldp_node(f_, init_);
    sig.node_value->add(n);
    return n;
  }

  virtual void mark() const
  {
    global_memory->mark(f_);
    global_memory->mark(init_.closure_value);
  }
};

class foldp_closure_1 : public closure
{
  closure *f_;
public:
  foldp_closure_1(closure *f) : f_(f) {}

  value operator()(value init)
  {
    return new foldp_closure_2(f_, init);
  }

  virtual void mark() const
  {
    global_memory->mark(f_);
  }
};

class foldp_closure : public closure
{
public:
  value operator()(value f)
  {
    return new foldp_closure_1(f.closure_value);
  }

  virtual void mark() const
  {
  }
};

class switch_node : public node
{
  int input_idx;

public:
  switch_node() : input_idx(1) {}
  virtual ~switch_node() {}

  virtual event process()
  {
    if (received_[0].changed && input_idx == 1) {
      const value& v = received_[0].data;
      if (v.int_value) {
        ++input_idx;
        return event(true, received_[input_idx].data);
      }
    }
    return received_[input_idx];
  }
};

class engine
{
  std::vector<node *> input_nodes_;

public:
  void register_input_node(node *n)
  {
    input_nodes_.push_back(n);
  }

  void loop()
  {
    std::queue<node *> q;
    for (std::vector<node *>::iterator it = input_nodes_.begin(); it != input_nodes_.end(); ++it) {
      q.push(*it);
    }
    while (!q.empty()) {
      node *n = q.front();
      q.pop();
      const event e = n->process();
      n->clear_events();
      for (std::vector<std::pair<node *, size_t> >::iterator it = n->kids().begin(); it != n->kids().end(); ++it) {
        node *next_node = it->first;
        next_node->receive(it->second, e);
        if (next_node->ready()) {
          q.push(next_node);
        }
      }
    }
  }
};

};

#endif /* end of include guard */
