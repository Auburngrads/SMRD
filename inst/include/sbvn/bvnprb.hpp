#ifndef SMRD_bvnprb_H
#define SMRD_bvnprb_H
void bvnprb(int &i,
            Rcpp::NumericVector &ah,
            Rcpp::NumericVector &ak,
            Rcpp::NumericVector &xmu1,
            Rcpp::NumericVector &xmu2,
            Rcpp::NumericVector &v1,
            Rcpp::NumericVector &v2,
            Rcpp::NumericVector &c12,
            Rcpp::NumericVector &prob);
#endif