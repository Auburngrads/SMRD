#include <base/base.hpp>

//' Helper function for wqm_cdfstr

double eps(double yy,
           double ee) {

  return std::max(std::fabs(yy * ee), ee);

};
