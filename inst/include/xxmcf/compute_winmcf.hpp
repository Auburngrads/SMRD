#ifndef smrd_compute_winmcf_H
#define smrd_compute_winmcf_H
void compute_winmcf(int &muniqrecurr, 
                    Rcpp::NumericVector &tuniq, 
                    Rcpp::IntegerVector &delta, 
                    Rcpp::IntegerVector &apoint,
                    int &lnumrecurr, 
                    Rcpp::IntegerVector &krecurrid, 
                    Rcpp::NumericVector &dcost, 
                    int &nunitsgroups, 
                    Rcpp::IntegerVector &wpoint,
                    int &nwindows, 
                    Rcpp::NumericVector &twindowsl, 
                    Rcpp::NumericVector &twindowsu, 
                    Rcpp::IntegerVector &wcounts, 
                    Rcpp::IntegerVector &iordl, 
                    Rcpp::IntegerVector &iordu,
                    Rcpp::IntegerVector &inwindowj, 
                    Rcpp::NumericVector &muhat, 
                    Rcpp::NumericVector &varhat, 
                    Rcpp::NumericVector &dbar, 
                    Rcpp::IntegerVector &iscrat, 
                    Rcpp::IntegerVector &iorder);
#endif