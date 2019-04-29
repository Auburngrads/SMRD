#include <base/base.hpp>

//' Find pest
//' 
//'    pestp<=0         use upest=0 and pest=wqm_phibf(upest)
//'  0<pestp<1         use pest=pestp as the fixed percentile
//'    pestp>=1         use pest=pfail/2 as the fixed or baseline percentile
//'    set default in case pestp<0

void fxpest(double &pestp,
            double &pfail,
            int &kmccde,
            double &pest){

pest = zero;
   
if(pestp <= zero) return;

// Do not bother if kmccde is too big
   if(kmccde > 2) return;
   
// If pestp is a valid quantile, use it
   pest = pestp;
   
// Otherwise, if pest=1, choose the quantile automatically
   if(pestp < one) return;
   
   pest = pfail / two;
   
return;

}