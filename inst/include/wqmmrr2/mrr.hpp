#ifndef SMRD_mrr_H
#define SMRD_mrr_H
void mrr(Rcpp::IntegerVector &key, 
         Rcpp::IntegerVector &krevrank, 
         Rcpp::NumericVector &y, 
         Rcpp::NumericVector &ynew,
         Rcpp::IntegerVector &cen,
         Rcpp::NumericVector &rmdrank,
         Rcpp::IntegerVector &wt,
         Rcpp::IntegerVector &censtar,
         Rcpp::NumericVector &xstar,
         Rcpp::NumericVector &x,
         Rcpp::IntegerVector &rorder,
         Rcpp::IntegerVector &censtarn, 
         Rcpp::IntegerVector &wtnew, 
         int &nrownw,
         Rcpp::NumericVector &thetamrr);
#endif
