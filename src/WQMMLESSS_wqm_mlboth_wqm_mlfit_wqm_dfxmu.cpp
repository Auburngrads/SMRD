#include <base/base.hpp>

// Function to compute the estimated location parameter

double wqm_dfxmu(int i,
                 Rcpp::NumericMatrix xnew,
                 int nrow,
                 int nter,
                 Rcpp::NumericVector thetaf,
                 int nparm,
                 double upcen,
                 double sigma){

double dfxmu = 0.0e00;
  
for(int j = 0; j < nter; j++){
  
    dfxmu = dfxmu + thetaf.at(j) * xnew.at(i,j);
  
}

  // Percentile correction

  dfxmu = dfxmu - upcen * sigma;

  return dfxmu;
}