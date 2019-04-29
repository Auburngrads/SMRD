#ifndef SMRD_floglikenhpp_H
#define SMRD_floglikenhpp_H
double floglikenhpp(Rcpp::NumericVector time, 
                    int ntimes, 
                    Rcpp::NumericVector recurrcosts,
                    Rcpp::NumericVector timel, 
                    Rcpp::NumericVector timeu, 
                    Rcpp::IntegerVector kwcount, 
                    int nwindows, 
                    int kform, 
                    Rcpp::NumericVector theta);
#endif