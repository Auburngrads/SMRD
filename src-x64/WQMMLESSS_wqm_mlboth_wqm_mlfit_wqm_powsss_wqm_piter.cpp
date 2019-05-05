#include <base/base.hpp>
#include <wqmmlesss/wqm_put.hpp>
#include <wqmmlesss/wqm_combet.hpp>

//' Compute unstandardized paramerer estimates

void wqm_piter(double &f,
               Rcpp::NumericVector &thetg,
               Rcpp::NumericVector &thetad,
               Rcpp::NumericVector &thetaf,
               Rcpp::LogicalVector &lfix,
               int &nparm,
               double &upcen,
               Rcpp::NumericVector &diag,
               Rcpp::NumericMatrix &tmat,
               Rcpp::NumericVector &thetb,
               int &nrow,
               int &nter,
               int &iterc,
               int &nfcc){
  
 wqm_put(thetaf,thetad,thetg,lfix,nparm);
 wqm_combet(thetaf,diag,tmat,nparm,thetb);

// If there was a percentile fix, get orig intercept
   thetb.at(0) = thetb.at(0) - upcen * std::exp(thetaf.at(nparm - 1));

return;
  
}
