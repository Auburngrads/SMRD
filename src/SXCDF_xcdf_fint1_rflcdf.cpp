#include <base/base.hpp>

double rflcdf(double x,
              int ndist){
  
double rfl_cdf = 0.0e00;
  
// SEV distribution
   if(ndist == 1) rfl_cdf = 1.0e00 - (1 / std::exp(std::exp(x)));

// Normal distribution
   if(ndist == 2) rfl_cdf = R::pnorm(x,0,1,true,false);
   
// Logistic Distribution
   if(ndist == 3) rfl_cdf = std::exp(x) / (1.e00 + std::exp(x));
   
return rfl_cdf;
}
