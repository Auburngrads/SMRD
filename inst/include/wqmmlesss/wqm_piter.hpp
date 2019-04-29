#ifndef SMRD_wqm_piter_H
#define SMRD_wqm_piter_H
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
               int &nfcc);
#endif
