#include <base/base.hpp>
//#include <genmax/uszhat.hpp>
#include <genmax/zgtran.hpp>

//' Standardized observation from a specified general distribution

double gzhat(double y,
             Rcpp::NumericVector gamme,
             int kdist){
   
   
double gz_hat = zero;
// Check for user specified zhat
// standardized observation corresponding to a user-specified cdf
   if(kdist > 100){
      
//      gz_hat = uszhat(y,gamme,kdist);
      
   }
   
// Standardize for location-scale distributions
   if((kdist > 0) and (kdist < 7)) {
      
       gz_hat = zgtran((y - gamme.at(0)) / gamme.at(1),kdist);
      
   }
   
// Exponential distribution
   if((kdist == 7) or (kdist == 8)){
      
      gz_hat = zgtran(y - gamme.at(0),1);
      
   }

// Generalized gamma
   if((kdist == 9) or (kdist == 10)){
      
       gz_hat = zgtran((y - gamme.at(0)) / gamme.at(1),5);
      
   }
   
// Ordinary gamma
   if((kdist == 11) or kdist == 12) {
      
      gz_hat = zgtran(y - gamme.at(0),5);
      
   }
   
return gz_hat;
      
}