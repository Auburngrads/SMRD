#ifndef SMRD_mrr2_H
#define SMRD_mrr2_H
void mrr2(Rcpp::NumericVector &y,
          Rcpp::IntegerVector &cen,
          Rcpp::IntegerVector &wt,
          Rcpp::IntegerVector &iscrat,
          Rcpp::NumericVector &rscrat,
          int &nrownw,
          Rcpp::NumericVector &thetamrr,
          Rcpp::IntegerVector &ikey,
          Rcpp::IntegerVector &ikrevrank,
          Rcpp::NumericVector &iynew,
          Rcpp::NumericVector &irmdrank,
          Rcpp::IntegerVector &icenstar,
          Rcpp::NumericVector &ixstar,
          Rcpp::NumericVector &ix,
          Rcpp::IntegerVector &irorder,
          Rcpp::IntegerVector &icenstarn,
          Rcpp::IntegerVector &iwtnew);
#endif
