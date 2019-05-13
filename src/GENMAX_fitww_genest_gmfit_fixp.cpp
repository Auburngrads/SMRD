#include <base/base.hpp>
#include <genfun/fixc.hpp>
#include <genmax/fxpi.hpp>

//' subroutine to go from the possible constrained space of thetas
//' to the transformed thetat unrestricted space
//' also, if pest.ne.0 and if kodet(1)=5, we can assume that
//' parameter 1 is a location parameter or a percentile that can be
//' used in place of the usual location parameter for estimation to
//' 
//'     a) reduce correlation between thetas(1) and the scale parameter
//'     b) rovide a convenient framework within which one can
//'        constrain a percentile estimate for likelihood tests and intervals
//' 
//' thetas(nparm)     is the input vector
//' thetat(nparm)    is the output vector
//' nparmm           is the number of elements in thetat that are not fixed
//' 
//'        kodet
//'           0       fixed
//'           1       unrestricted (usually location)
//'           2       0 - infinity (usually scale)
//'           3       0 - 1 (usually probability)
//'           4       unrestricted  (usually transformed shape)
//'           5       unrestricted special location-percentile parameter

void fixp(Rcpp::NumericVector &thetas,
          Rcpp::IntegerVector &kodet,
          int &nparm,
          Rcpp::NumericVector &thetat){
   
double t1hold;
int ier = 0;

// save thetas(1) for restore below
// we can do this here because kodet(1)=1 or 0 always
// so we never need to take logs
   t1hold = thetas.at(0);
   
// if we are using percentile translation,
// first, get the percentile that we are using as the key
// otherwise, just use present thetas(1)
// check kmccde below
   thetas.at(0) = fixc(thetas,nparm);
   
   for(int i = 1; i <= nparm; i++){
      
       thetat.at(i - 1) = fxpi(thetas.at(i - 1),kodet.at(i - 1),ier);
      
   }
   
// don't want to change input, so restore thetas(1)
   thetas.at(0) = t1hold;
   
return;

}