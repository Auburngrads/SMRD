#ifndef smrd_setup_winmcfdata_H
#define smrd_setup_winmcfdata_H
void setup_winmcfdata(int &numrecurr, 
                      Rcpp::NumericVector &timeofrecurr,
                      Rcpp::IntegerVector &krecurrid, 
                      Rcpp::NumericVector &dcost, 
                      int &muniqrecurr, 
                      Rcpp::NumericVector &tuniq,
                      int &lnumrecurr, 
                      Rcpp::IntegerVector &apoint, 
                      Rcpp::IntegerVector &iorder, 
                      Rcpp::IntegerVector &iscrat);
#endif