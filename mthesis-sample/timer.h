#ifndef SAMPLE_TIMER_H
#define SAMPLE_TIMER_H

#include "runtime.h"
#include <time.h>

class timer_input_node : public cfrp::node
{
  time_t last_;

public:
  timer_input_node() : last_(0) {}
  virtual ~timer_input_node() {}

  virtual cfrp::event process();
  virtual void mark() const {}
};

#endif /* end of include guard */
