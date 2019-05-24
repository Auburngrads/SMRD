#include <base/base.hpp>
#include <sxpdf3/xpdf3.hpp>

// [[Rcpp::export]]
Rcpp::List SXPDF3(int ndist1,
                  int ndist2,
                  Rcpp::NumericVector beta0,
                  Rcpp::NumericVector beta1,
                  Rcpp::NumericVector xstr,
                  Rcpp::NumericVector sigma,
                  Rcpp::NumericVector ugamma,
                  Rcpp::NumericVector sgamma,
                  Rcpp::NumericVector w,
                  int num,
                  Rcpp::NumericVector answer,
                  Rcpp::IntegerVector ier,
                  int kprint){
  
debug::kprint = kprint;
   
Rcpp::NumericVector beta0p = clone(beta0);
Rcpp::NumericVector beta1p = clone(beta1);
Rcpp::NumericVector xstrp = clone(xstr);
Rcpp::NumericVector sigmap = clone(sigma);
Rcpp::NumericVector ugammap = clone(ugamma);
Rcpp::NumericVector sgammap = clone(sgamma);
Rcpp::NumericVector wp = clone(w);
Rcpp::NumericVector answerp = clone(answer);
Rcpp::IntegerVector ierp = clone(ier);
  
for(int i = 0; i < num; i++){
  
    xpdf3(ndist1,ndist2,beta0p.at(i),beta1p.at(i),
          xstrp.at(i),sigmap.at(i),ugammap.at(i),sgammap.at(i),
          wp.at(i),answerp.at(i),ierp.at(i));
     
}

    return Rcpp::List::create(Named("ier") = ierp,
                              Named("ndist1") = ndist1,
                              Named("ndist2") = ndist2,
                              Named("beta0") = beta0p,
                              Named("beta1") = beta1p,
                              Named("xstr") = xstrp,
                              Named("sigma") = sigmap,
                              Named("ugamma") = ugammap,
                              Named("sgamma") = sgammap,
                              Named("w") = wp,
                              Named("num") = num,
                              Named("answer") = answerp);
  
}



#include <base/base.hpp>
#include <sxpdf3/fint.hpp>
#include <sbq/zeroin.hpp>
#include <sxcdf/dqagi.hpp>
#include <sfteval/dqags.hpp>

using namespace passer3;
using namespace passer4;

// another subroutine to compute the pdf of W;
// where W is log(time) to failure of a;
// specimen subjected to stress xstr,;
// W|V is has a location-scale distribution with;
// location=beta0+beta1*log(exp(xstr)-exp(v));
// scale=sigma;
// where V=log(fatigue limit) and V is location-scale;
// with;
// location=ugamma;
// scale=sdgammap;
// The computations below use some transformations on;
// the model parameters. Essentially, the algorithm;
// uses 3 parameters instead of 5.;
//inputs:;
// ndist1, ndist2 integer codes for distributions;
// 1=sev, 2=normal, 3=logistic;
// beta0 intercept of mean log(time) model eqn;
// beta1 slope of mean log(time) model eqn;
// xstr stress level;
// sigma std devn of log(time);
// ugamma mean of the fatigue limits;
// sdgamma std devn of the fatigue limits;
// w log(time) at which density is computed;
//transformations (3 parameters used):;
// beta11 = beta1/sigma;
// ugamma1 = ugamma-xstr;
// sdgamma = sdgamma;
// w1 = (w-beta0-beta1(xstr))/sigma;
//outputs:;
// answer density value;
// ier return condition indicator;
// 0 if no errors were;
// detected in dqags.;
// see dqags documentation for meaning of;
// values of ier>0;
void xpdf3(int &ndist1,
           int &ndist2,
           double &beta0,
           double &beta1,
           double &xstr,
           double &sigma,
           double &ugamma,
           double &sdgamma,
           double &w,
           double &answer,
           int &ier){
  
Rcpp::IntegerVector iwork(100);
Rcpp::NumericVector work(400),glimits(4);
double eps,xlog,beta11,ugamma1,w1,g1,g2,abserr;
double answer1,answer2,answer3,answer0,vbound;
double lim_zero = 0.0e00;
int limit,neg,nzero,inf_neg = -1,limit4,ni;
int last,neval;

// Constants that one might want to change to
// Achieve a higher degree of accuracy from the algorithm
   eps = 1.0e-10;
   limit = 100;
   limit4 = 4 * limit;
   ier = 0;
   xlog = std::log(xstr);
   
// Transform model parameters and w=log(life)
   beta11 = beta1 / sigma;
   ugamma1 = ugamma - xlog;
   w1 = (w - beta0 - beta1 * xlog) / sigma;
   
// Common variables
   passer3::g_beta11p = beta11;
   passer3::g_ugamma1p = ugamma1;
   passer3::g_sdgammap = sdgamma;
   passer3::g_w1p = w1;
   passer4::g_ndist1p = ndist1;
   passer4::g_ndist2p = ndist2;
   
// Find bounds for integrations
////////////////////////////////////////////////////////////////////
// The integrand behaves like the conditional pdf of log(life) given
// v = log(fatigue limit). With respect to the integrator v, this pdf is
// maximum at vbound=one - exp(w1 / beta11).
// Points 4 standardard deviations from w1 = (w-beta0-beta1*xlog)/sigma 
// are also taken. The integration is from -(infinity) to 0. The procedure 
// below checks if each bound is negative. If all the above bounds are positive, 
// use dqagi to integrate. 
// neg = # of bounds below 0
// glimits = vector of sorted bounds;
   neg = 0;
   nzero = 0;
   answer1 = zero;
   answer2 = zero;
   answer3 = zero;
   answer0 = zero;
   vbound = one - std::exp(w1 / beta11);
   
   if(vbound <= zero) {
     
      glimits.at(0) = 2.0e00;
      neg = neg + 1;
     
   } else {
     
      glimits.at(0) = std::log(vbound);
     
   }
   
   if(debug::kprint > 1) {
     
      Rcpp::Rcout << "\nXPDF3**1 vbound = " << vbound << std::endl;
      Rcpp::Rcout << "glimits = " << glimits << std::endl;
     
   }

   vbound = one - std::exp((w1 - 4.0e00) / beta11);
   
   if(vbound <= zero) {
     
      glimits.at(1) = 2.0e00;
      neg = neg + 1;
     
   } else {
      
      glimits.at(1) = std::log(vbound);
     
   }
   
   if(debug::kprint > 1) {
     
      Rcpp::Rcout << "\nXPDF3**2 vbound = " << vbound << std::endl;
      Rcpp::Rcout << "glimits = " << glimits << std::endl;
     
   }

   vbound = one - std::exp((w1 + 4.0e00) / beta11);
   
   if(vbound <= zero) {
     
      glimits.at(2) = 2.0e00;
      neg = neg + 1;
     
   } else {
     
      glimits.at(2) = std::log(vbound);
     
   }
   
   if(debug::kprint > 1) {
     
      Rcpp::Rcout << "\nXPDF3**3 vbound = " << vbound << std::endl;
      Rcpp::Rcout << "glimits = " << glimits << std::endl;
     
   }

// Sort bounds and put in glimits
   for(int i = 1; i <= 2; i++){

       g1 = glimits.at(i - 1);
       ni = i + 1;

       for(int j = ni; j <= 3; j++){

           g2 = glimits.at(j - 1);

           if(g2 <= g1) {

              glimits.at(i - 1) = g2;
              glimits.at(j - 1) = g1;

           }
       }
   }
   
// Use some other bounds if all bounds > 0
// Check if < 0
if(debug::kprint > 1) {
  
   Rcpp::Rcout << "\nXPDF3 neg = " << neg << std::endl;
  
}
if(neg != nzero) {

   glimits.at(0) = ugamma1 - 4.0e00 * sdgamma;
   glimits.at(1) = ugamma1 + 4.0e00 * sdgamma;

if(glimits.at(1) <= zero) {
  
   dqags(fint,glimits.at(1),lim_zero,eps,eps,answer1,
         abserr,neval,ier,limit,limit4,last,iwork,work);
   dqags(fint,glimits.at(0),glimits.at(1),eps,eps,answer2,
         abserr,neval,ier,limit,limit4,last,iwork,work);
   dqagi(fint,glimits.at(0),inf_neg,eps,eps,answer3,abserr,
         neval,ier,limit,limit4,last,iwork,work);
   
 } else {

   if(glimits.at(0) <= zero) {
     
      dqags(fint,glimits.at(0),lim_zero,eps,eps,answer1,
            abserr,neval,ier,limit,limit4,last,iwork,work);
      dqagi(fint,glimits.at(0),inf_neg,eps,eps,answer2,abserr,
            neval,ier,limit,limit4,last,iwork,work);
      
    } else {
     
      dqagi(fint,lim_zero,inf_neg,eps,eps,answer1,abserr,
            neval,ier,limit,limit4,last,iwork,work);
     
   }
 
 }
 
answer0 = answer1 + answer2 + answer3;
 

} else { // Use original bounds if at least one is negative
  
  glimits.at(neg) = zero;
  
  for(int i = 1; i <= neg; i++){
  
      ni = i + 1;
      dqags(fint,glimits.at(i - 1),glimits.at(ni - 1),eps,eps,answer1,
            abserr,neval,ier,limit,limit4,last,iwork,work);
      answer0 = answer0 + answer1;
  
  }
  
  dqagi(fint,glimits.at(0),inf_neg,eps,eps,answer1,abserr,
        neval,ier,limit,limit4,last,iwork,work);
  
  answer0 = answer0 + answer1;

}

if(debug::kprint >= 5){
   
   Rcpp::Rcout << "\nEND OF XPDF3\n" << std::endl;
   Rcpp::Rcout << "answer  = " << answer  << std::endl;
   Rcpp::Rcout << "answer0 = " << answer0 << std::endl;
   Rcpp::Rcout << "answer1 = " << answer1 << std::endl;
   Rcpp::Rcout << "answer2 = " << answer2 << std::endl;
   Rcpp::Rcout << "answer3 = " << answer3 << std::endl;
   Rcpp::Rcout << "glimits = " << glimits << std::endl;
   
}

answer = answer0 / (sigma * sdgamma);

return;

}



#include <base/base.hpp>
#include <sxcdf/rflpdf.hpp>

using namespace passer3;
using namespace passer4;

// Function to compute integrand for computing probability involving the compound distribution
//
// @details The following variables are communicated through 
//          the global namespace \code{passer3}
//          
// \document{
//   \item{beta11p}{slope of the mean log(time)}
//   \item{ugamma1p}{mean of fatigue limit}
//   \item{sdgammap}{std devn of fatigue limit}
//   \item{w1p}{log(time)}
//   }
//
double fint(double x){

  double f_int,y,z;
  
  y = (x - passer3::g_ugamma1p) / passer3::g_sdgammap;
  z =  passer3::g_w1p - passer3::g_beta11p * std::log(1.0e00 - std::exp(x));
  
  f_int = rflpdf(z, passer4::g_ndist1p) * rflpdf(y, passer4::g_ndist2p);
  
  return f_int;
  
}
