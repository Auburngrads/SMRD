#ifndef smrd_pairrisk_H
#define smrd_pairrisk_H
int pairrisk(int k, 
             Rcpp::IntegerVector inwindowj, 
             Rcpp::NumericVector tuniq, 
             int muniqrecurr,
             int nunitsgroups,
             Rcpp::IntegerVector wpoint, 
             int nwindows, 
             Rcpp::NumericVector twindowsl, 
             Rcpp::NumericVector twindowsu, 
             Rcpp::IntegerVector wcounts);
#endif