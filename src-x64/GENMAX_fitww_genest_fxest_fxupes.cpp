#include <base/base.hpp>
#include <utility/dcheck.hpp>
#include <utility/wqm_quant.hpp>

//' by default, set upest=0.  then use pest everywhere to shift
//' if possible, set upest=phiinv(pest) for quick shift
//' if pest=0, we will always use upest=0 in estimation
//' if doing quantile stuff pest is always between 0 and 1
//' if pestp comes in as 1 or more, use data to get a good pest

void fxupes(double &pest,
            int &kmccde,
            int &kmod,
            int &kdist,
            double &upest,
            int &lupest){

int ier = 0;
upest = zero;
   
// If we have a complicated model we can't or won't use the upest shortcut
   if(kmccde > 2) return;
   if(kmod > 0) return;
   if(kdist > 8) return;
   
// We should arrive here only with a simple model and dist
// so we can set the upest short cut and the logical indicator
   lupest = 1;
   
// If pest<=0, we will always use upest=0 below for the usual location def
   if(pest <= zero) return;
   dcheck(pest,zero,0.99999999999999e00,0.5e00,0.5e00,ier,-34509);
   
   upest = wqm_quant(pest,kdist);
   
return;

}