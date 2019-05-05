#include <base/base.hpp>

//'  Untransform (antilog) fitted values, residuals,
//'  and quantile estimates with a check.
//'  
//'  We have small and big here to limit the size of 
//'  the fitted value by distribution

double wqm_upck(double y, 
                int kdist){

Rcpp::NumericVector big   = Rcpp::NumericVector::create(1.0e30,45.0e00,1.0e30,20.0e00,
                                                        1.0e30,45.0e00,1.0e30,20.0e00);

Rcpp::NumericVector small = Rcpp::NumericVector::create(-1.0e30,-45.0e0,-1.0e30,-20.0e0,
                                                        -1.0e30,-45.0e0,-1.0e30,-20.0e0);

double upck = y;

if((kdist %2) == 0) {
  
   upck = std::exp(y);
   
   if(y > big.at(kdist - 1))   upck = 1.0e30;
   if(y < small.at(kdist - 1)) upck = 0.0e00;
   
}
   
   return upck;
   
}
