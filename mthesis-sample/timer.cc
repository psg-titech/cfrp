#include "timer.h"

cfrp::event timer_input_node::process()
{
  time_t t;
  time(&t);
  const bool changed = t != last_;
  last_ = t;
  return cfrp::event(changed, cfrp::value((int)last_));
}
