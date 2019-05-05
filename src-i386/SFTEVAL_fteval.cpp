#include <base/base.hpp>
#include <sfteval/dqags.hpp>
#include <sfteval/fundeg.hpp>

using namespace passer;

//' @description Computes the probability pr(t < tf)
//'              where t is the time of first crossing
//'              of a monotone degradation process with
//'              two random components
//'
//' @param kdmod degradation model (1: exponential decay, 2: paris law)
//' @param xmu1 mean of beta1 = log(Rate)
//' @param sig1 sd of beta1
//' @param xmu2 mean of beta2 = log(-Asymptote)
//' @param sig2 sd of beta2
//' @param rho correlation coefficient
//' @param df degradation failure criterion
//' @param d0 initial degradation level
//' @param sfact stress factor
//' @param tf time point on cdf at which probability is desired;
//'
//' @return \code{answer} computed probability pr(t < tf)
//'         \code{ier} return condition indicator - 0 if
//'                    no errors were detected in
//'                    dqags. See dqags documentation
//'                    for meaning of values of ier > 0;

void fteval(int &kdmod,
            double &xmu1,
            double &sig1,
            double &xmu2,
            double &sig2,
            double &rho,
            double &df,
            double &d0,
            double &sfact,
            double &tf,
            double &answer,
            int &ier){

Rcpp::IntegerVector iwork = Rcpp::IntegerVector(100);
Rcpp::NumericVector  work = Rcpp::NumericVector(400);

// constants that one might want to change to;
// achieve a higher degree of accuracy from the algorithm;
   double xm = 8.0e00, eps = 1.0e-10;
   int limit = 100;

// define constants needed to compute the integrand;
   passer::g_xmu1p = xmu1;
   passer::g_sig1p = sig1;
   passer::g_xmu2p = xmu2;
   passer::g_sig2p = sig2;
   passer::g_rhop = rho;
   passer::g_rootr = std::sqrt(one - rho * rho);
   passer::g_dfp = df;
   passer::g_tfp = tf;
   passer::g_d0p = d0;
   passer::g_sfactp = sfact;
   passer::g_kdmodp = kdmod;

double spread = sig1 * xm;
double abserr = 0.0e00;
int neval = 0, Last = 0;

// Do the integration
   double xmu1_minus = xmu1 - spread;
   double xmu1_plus  = xmu1 + spread;
   int limit4 = limit * 4; 
   dqags(fundeg,xmu1_minus,xmu1_plus,eps,eps,answer,
         abserr,neval,ier,limit,limit4,Last,iwork,work);

return;

}
