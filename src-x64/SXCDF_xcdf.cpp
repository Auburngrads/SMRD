#include <base/base.hpp>
#include <sxcdf/dqagi.hpp>
#include <sxcdf/fint1.hpp>

using namespace passer1;
using namespace passer2;

//' @description Function to compute the cdf of W,
//'              where W is log(time) to failure of a 
//'              specimen subjected to stress xstr, W|V 
//'              is has a location-scale distribution with
//'              \code{location=beta0+beta1*log(exp(xstr)-exp(v))}
//'              and \code{scale = sigma} and where 
//'              \code{V = log(fatigue limit)} and \code{V} 
//'              is location-scale with \code{location = ugamma}
//'              and \code{scale = sgammap}
//' @param ndist1 Integer code for distribution (1=sev, 2=normal, 3=logistic)
//' @param ndist2 Same as \code{ndist1}  
//' @param beta0  Intercept of mean log(time) model eqn
//' @param beta1 Slope of mean log(time) model eqn
//' @param xstr  Stress level
//' @param sigma Std devn of log(time)
//' @param ugamma Mean of the fatigue limits
//' @param sgamma Std devn of the fatigue limits
//' @param t      Point at which density is computed
//' @param answer Density value returned
//' @param ier    Return condition indicator (ier = 0 if no errors were detected in dqags.
//'               See dqags documentation for meaning of values of ier>0.

void xcdf(int &ndist1,
          int &ndist2,
          double &beta0,
          double &beta1,
          double &xstr,
          double &sigma,
          double &ugamma,
          double &sgamma,
          double &w,
          double &answer,
          int &ier){
  
Rcpp::NumericVector work  = Rcpp::NumericVector(400);
Rcpp::IntegerVector iwork = Rcpp::IntegerVector(100);
double eps,xlog;
int limit,limit4,inf,last,neval;
double abserr;

// Constants that one might want to change to achieve a higher degree of accuracy from the algorithm
   eps  = 1.0e-15;
   limit = 100;
   ier = 0;
   xlog = std::log(xstr);

// Define other constants needed to compute the integrand
   passer1::g_beta0p = beta0;
   passer1::g_beta1p = beta1;
   passer1::g_sigmap = sigma;
   passer1::g_ugammap = ugamma;
   passer1::g_sgammap = sgamma;
   passer1::g_xlogp = xlog;
   passer1::g_wp = w;
   passer2::g_ndist1p = ndist1;
   passer2::g_ndist2p = ndist2;

// Do the integration
   limit4 = limit * 4;
   inf = -1;
   dqagi(fint1,xlog,inf,eps,eps,answer,abserr,
         neval,ier,limit,limit4,last,iwork,work);
   
   answer = answer / sgamma;
   
   return;
   
}
