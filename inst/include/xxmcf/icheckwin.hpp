#ifndef smrd_icheckwin_H
#define smrd_icheckwin_H
int icheckwin(double tuniq, 
              int index, 
              int nunitsgroups,
              Rcpp::IntegerVector wpoint, 
              int nwindows, 
              Rcpp::NumericVector twindowsl, 
              Rcpp::NumericVector twindowsu);
#endif