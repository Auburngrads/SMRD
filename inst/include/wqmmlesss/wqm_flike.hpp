#ifndef SMRD_wqm_flike_H
#define SMRD_wqm_flike_H
Rcpp::List wqm_flike(Rcpp::NumericMatrix y,
                     Rcpp::NumericMatrix xnew,
                     Rcpp::IntegerVector cen,
                     Rcpp::IntegerVector wt,
                     int nty,
                     Rcpp::NumericMatrix ty,
                     Rcpp::IntegerVector tcodes,
                     int nrow,
                     int nter,
                     int ny,
                     Rcpp::NumericVector diag,
                     Rcpp::NumericMatrix tmat,
                     Rcpp::NumericVector thetb,
                     int kdist,
                     Rcpp::NumericVector thetg,
                     int nparm,
                     double upcen);
#endif
