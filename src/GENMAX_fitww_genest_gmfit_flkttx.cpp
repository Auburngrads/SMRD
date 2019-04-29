#include <base/base.hpp>
#include <genmax/vecexp.hpp>
#include <genmax/flktt1.hpp>

//' Compute the (negative) of the log likelihood by
//' as a function of the unconstrained thetac vector that is compressed
//' npoint=0    single call to flkt(thetas,n)
//' npoint>0    npoint calls to flkt(i,thetas,n)
//' 
//' 24 may 1987  changed nparmm to nparmx in arglist because of
//'              problem with estbb in test bed. we do not want
//'              to change the argument before returning.
//' Note that since the fixed values are above, the dimension of
//' thetac is really nparm although poweld thinks it is nparmm

double flkttx(Rcpp::NumericVector thetac,
              int nparmx){
   
double fl_kttx;
   
// Grab space for the untransformed thetas values that we will compute
// and for the scratch space to hold the expanded theta vector
   Rcpp::NumericVector ithets = Rcpp::NumericVector(genx07::g_nparm);
   Rcpp::NumericVector ithett = Rcpp::NumericVector(genx07::g_nparm);
   
// Move the fixed parameters back into place
   vecexp(thetac,genx03::g_ipkode,genx07::g_nparm,ithett,nparmx);
   
   fl_kttx = flktt1(ithett,ithets,genx03::g_ipkode,
                    genx01::g_ipinow,genx07::g_nparm,
                    genx00::g_npoint);
   
return fl_kttx;
   
}