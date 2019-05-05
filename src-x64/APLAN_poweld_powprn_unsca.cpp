#include <base/base.hpp>
#include <genfun/unfixp.hpp>
#include <genmax/unscpx.hpp>

using namespace genx03;
using namespace genx07;

// Subroutine to go from theta to thetas

void unsca(Rcpp::NumericVector &thetat,
           Rcpp::NumericVector &theta){
  
// Grab scratch space for thetas
   Rcpp::NumericVector ithets = Rcpp::NumericVector(genx07::g_nparm);
  
// Find the constrained set of parameters
   unfixp(thetat,genx03::g_ipkode,genx07::g_nparm,ithets);
   
// Find the parameters corresponding to the unscaled explanatory variables
   unscpx(ithets,theta);
   
return;
  
}