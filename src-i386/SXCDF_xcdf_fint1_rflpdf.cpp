#include <base/base.hpp>

double rflpdf(double x,int ndist){
  
double cval = 0.39894228040143e00,rfl_pdf = 0.0e00;

// SEV distribution
   if(ndist == 1) rfl_pdf = std::exp(x - std::exp(x));

// Normal distribution
   if(ndist == 2) rfl_pdf = cval * std::exp(-1 * half * x * x);
   
// Logistic distribution
   if(ndist == 3) rfl_pdf = std::exp(x) / std::pow((one + std::exp(x)), 2);

return rfl_pdf;

}
