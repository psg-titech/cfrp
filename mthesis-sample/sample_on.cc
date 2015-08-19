#include "sample_on.h"

cfrp::event sample_on_node::process()
{
  if (received_[0].changed) {
    prev_ = received_[1].data;
    return cfrp::event(true, prev_);
  } else {
    return cfrp::event(false, prev_);
  }
}

void sample_on_node::mark() const
{
  cfrp::global_memory->mark(prev_.closure_value);
}

namespace foreign
{

class sample_on_closure_1 : public cfrp::closure
{
  cfrp::node *sampler_;

public:
  sample_on_closure_1(cfrp::node *sampler) : sampler_(sampler) {}

  cfrp::value operator()(cfrp::value target)
  {
    cfrp::node *target_node = target.node_value;
    cfrp::node *n = new sample_on_node(sampler_, target_node);
    sampler_->add(n);
    target_node->add(n);
    return n;
  }

  virtual void mark() const {}
};

struct sample_on_closure : public cfrp::closure
{
  cfrp::value operator()(cfrp::value sampler)
  {
    return new sample_on_closure_1(sampler.node_value);
  }

  virtual void mark() const {}
};

};

namespace initializer {
cfrp::value sample_on()
{
  return new foreign::sample_on_closure();
}
};
