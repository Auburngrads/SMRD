#include <base/base.hpp>
#include <spgeng/zgtran.hpp>
#include <spgeng/usrcdf.hpp>
#include <utility/dexpc.hpp>
#include <sbvn/derfc.hpp>
#include <spmlgeng/pbmgg.hpp>

//' 1-cdf of specified general distribution

double gcdfm(double y,
             Rcpp::NumericVector gamme,
             int kdist){
  
double z,g_cdfm = 0.0e00;
  
// Check for user specified cdf
   if(kdist > 100){ g_cdfm = one - usrcdf(y,gamme,kdist); }
   
// Standardize for location-scale distributions
   if((kdist > 0) and (kdist < 7)){
     
       z = zgtran((y - gamme.at(0)) / gamme.at(1),kdist);
     
       // sev distribution
       if((kdist == 1) or (kdist == 2)) { g_cdfm = dexpc(-1 * dexpc(z)); }
       
       // normal distribution
       if((kdist == 3) or (kdist == 4)) { g_cdfm = half * derfc(z * root); }
       
       // logistic distribution
       if((kdist == 5) or (kdist == 6)) { g_cdfm = dexpc(-z) / (one + dexpc(-z)); }

   } 

// Exponential   
if((kdist == 7) or (kdist == 8)){
  
    z = zgtran(y - gamme.at(0),1);
    g_cdfm = dexpc(-1 * dexpc(z));
  
}

// Generalized Gamma   
if((kdist == 9) or (kdist == 10)){
  
    z = zgtran((y - gamme.at(0)) / gamme.at(1),5);
    g_cdfm = pbmgg(z,gamme.at(3),gamme.at(5),gamme.at(4));
  
}

// Regular Gamma   
if((kdist == 11) or (kdist == 12)){
  
    z = zgtran(y - gamme.at(0),5);
    g_cdfm = pbmgg(z,gamme.at(1),gamme.at(3),gamme.at(2));
  
}

return g_cdfm;

}