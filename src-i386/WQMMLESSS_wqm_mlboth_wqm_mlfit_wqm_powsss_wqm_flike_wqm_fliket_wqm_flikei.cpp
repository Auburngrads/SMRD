#include <base/base.hpp>
#include <wqmmlesss/wqm_fcdfml.hpp>
#include <wqmmlesss/wqm_fpdfl.hpp>
#include <wqmmlesss/wqm_phibl.hpp>
#include <wqmmlesss/wqm_phibf.hpp>

//' Compute the loglikelihood for
//' a single observation

double wqm_flikei(int itypep,
                  double z,
                  double z2,
                  double sigmal,
                  int kdist){

double flikei = 0.0e00;
int itype;

// If itypep is negative, we're computing 
// the truncation likelihood denominator
// in the denom, left is the survival 
// prob and right is the cdf

if(itypep < 0) {
  
   itype = -1 * itypep;
   if(itype == 1) flikei = wqm_fpdfl(z,sigmal,kdist);
   if(itype == 2) flikei = wqm_phibl(z,kdist);
   if(itype == 3) flikei = wqm_fcdfml(z,kdist);
  
 } else {
     
   itype = itypep;
   if(itype == 1) flikei = wqm_fpdfl(z,sigmal,kdist);
   if(itype == 2) flikei = wqm_fcdfml(z,kdist);
   if(itype == 3) flikei = wqm_phibl(z,kdist);
   
   }
 
    if((itype == 4) or (itype == 5)) {
     
       flikei = std::log(std::max(wqm_phibf(z2,kdist) - wqm_phibf(z,kdist), 1.0e-35));
     
   }

   return flikei;
   
}

#include <base/base.hpp>
#include <utility/wqm_dxerc.hpp>

//' Compute the log probability density

double wqm_fcdfml(double z,
                  int kdist){
  
double fcdfml = 0.0e00;

// sev
if((kdist == 1) or (kdist == 2)) fcdfml = -1 * std::exp(z);

// normal
if((kdist == 3) or (kdist == 4)) fcdfml = std::log(half * wqm_dxerc(z * root));

// logistic
if((kdist == 5) or (kdist == 6)) fcdfml = -z - std::log(one + std::exp(-z));

// lev
if((kdist == 7) or (kdist == 8)) fcdfml = std::log(one - std::exp(-std::exp(-z)));

    return fcdfml; 
      
}

#include <base/base.hpp>

//' Compute the log probability density

double wqm_fpdfl(double z,
                 double sigmal,
                 int kdist){
  
double fpdfl = 0.0e00;
double hlntp = -.9189385332046794e00;

// sev
if((kdist == 1) or (kdist == 2)) fpdfl = -1 * std::exp(z) + z - sigmal;

// normal
if((kdist == 3) or (kdist == 4)) fpdfl = hlntp - sigmal - half * z * z;

// logistic
if((kdist == 5) or (kdist == 6)) fpdfl = -z - two * std::log(one + std::exp(-z)) - sigmal;

// lev
if((kdist == 7) or (kdist == 8)) fpdfl = -std::exp(-z) - z - sigmal;

    return fpdfl; 
      
}

#include <base/base.hpp>
#include <wqmmlesss/wqm_phibf.hpp>

//' Compute the log cdf of a location-scale
//' distribution

double wqm_phibl(double z,
                 int kdist){
  
double phibl = std::log(wqm_phibf(z,kdist));
  
  return phibl;
    
}

#include <base/base.hpp>
 
//' Compute the cdf of a location-scale 
//' distribution
  
double wqm_phibf(double z,
                 int kdist) {
  
double phibf = 0.0e00;

// SEV
if((kdist == 1) or (kdist == 2)) {
  
  if(z < 2.5) { 
    
     phibf = one - std::exp(-1 * std::exp(z));
    
  } else {
    
     phibf = 0.9999999999;
    
  }
  
} 

// Normal
if((kdist == 3) or (kdist == 4)) {
  
    phibf = R::pnorm(z, 0, 1, true, false);
  
}

// Logistic
if((kdist == 5) or (kdist == 6)) {
  
   phibf = one/(one + std::exp(-z));
  
}

// LEV
if((kdist == 7) or (kdist == 8)) {
  
   phibf = std::exp(-1 * std::exp(-z));
  
}

  return phibf;
  
}