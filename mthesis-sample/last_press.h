#ifndef SAMPLE_LAST_PRESS_H
#define SAMPLE_LAST_PRESS_H

#include "runtime.h"

class last_press_input_node : public cfrp::node
{
  char last_;

public:
  last_press_input_node() : last_('\0') {}
  virtual ~last_press_input_node() {}

  virtual cfrp::event process();
  virtual void mark() const {}
};


#endif /* end of include guard */
