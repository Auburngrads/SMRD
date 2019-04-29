#include <base/base.hpp>
#include <genfun/unfxpi.hpp>
#include <genfun/unfixc.hpp>

//' this routine does the inverse of fixp
//' untransform unrestricted variables to the
//' (possibly) restricted space using kodet
//' 
//' if pest!=0 and if kodet(1)=5, thetas(1) contains the estimate of a
//' percentile and we want to work backwards to get the estimate of
//' the usual intercept or location parameter

void unfixp(Rcpp::NumericVector &thetat,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            Rcpp::NumericVector &thetas){
  
for(int i = 1; i <= nparm; i++){
  
    thetas.at(i - 1) = unfxpi(thetat.at(i - 1),kodet.at(i - 1));
    
}

// thetas(0) is now a percentile at kpoint=0 (for evaluation at xbar).
// now go to get the location parmaeter at xbar
// or, more generally, the location parameter at kpoint
// check kmccde below
   thetas.at(0) = unfixc(thetas,nparm);
  
return;

}