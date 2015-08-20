#include "misc.h"

namespace foreign {

class print_int2_closure_1 : public cfrp::closure
{
  int x1_;
public:
  print_int2_closure_1(int x1) : x1_(x1) {}

  cfrp::value operator()(cfrp::value x2)
  {
    std::printf("%d %d\n", x1_, x2.int_value);
    return cfrp::value();
  }

  virtual void mark() const {}
};

struct print_int2_closure : public cfrp::closure
{
  cfrp::value operator()(cfrp::value x1)
  {
    return new print_int2_closure_1(x1.int_value);
  }

  cfrp::value operator()(cfrp::value x1, cfrp::value x2)
  {
    std::printf("%d %d\n", x1.int_value, x2.int_value);
    return cfrp::value();
  }

  virtual void mark() const {}
};

};

namespace initializer {
cfrp::value print_int2()
{
  return new foreign::print_int2_closure();
}
};
