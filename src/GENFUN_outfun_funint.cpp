#include <base/base.hpp>
#include <genfun/gfun.hpp>
#include <genfun/intgen.hpp>

// Estimate and compute confidence interval for user defined func

void funint(Rcpp::List (*func)(Rcpp::List),
            int &ifkode,
            double &conlev,
            Rcpp::NumericVector &thetas,
            Rcpp::NumericMatrix &vcvs,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            double &epsx,
            double &fest,
            double &s,
            double &xl,
            double &xu){
  
// igsd=1 because we want to compute a standard error below
   int igsd = 1;
   double vest = 0.0e00;
  
// Get function estimate and estimate of variance
   gfun(func,thetas,vcvs,kodet,nparm,igsd,fest,vest,epsx);
   
   Rcpp::Rcout << "\nfunint after gfun\n" << std::endl;
   
// Compute the confidence interval
   intgen(fest,vest,ifkode,conlev,s,xl,xu);
   
return;

}

  