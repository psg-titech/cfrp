#ifndef SAMPLE_SAMPLE_ON_H
#define SAMPLE_SAMPLE_ON_H

#include "runtime.h"

extern cfrp::value sample_on;

class sample_on_node : public cfrp::node
{
  cfrp::node *sampler_, *target_;
  cfrp::value prev_;

public:
  sample_on_node(cfrp::node *sampler, cfrp::node *target)
    : sampler_(sampler), target_(target)
  {}

  virtual ~sample_on_node() {}

  virtual cfrp::event process();

  virtual void mark() const;
};

namespace initializer {
cfrp::value sample_on();
};

#endif /* end of include guard */
