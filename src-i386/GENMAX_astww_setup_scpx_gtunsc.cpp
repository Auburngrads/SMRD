#include <base/base.hpp>

using namespace genx03;
using namespace genx05;
using namespace genx07;

void gtunsc(int &i1,
            int &i2,
            Rcpp::NumericVector &i3,
            Rcpp::NumericVector &i4,
            Rcpp::NumericVector &i5){
  
  i1 = genx03::g_ngame;
  i2 = genx07::g_nparm;
  i3 = genx05::g_ipxbru;
  i4 = genx05::g_ipsd;
  i5 = genx05::g_ipiscd;
  
  return;
  
}