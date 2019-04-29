#ifndef smrd_flkt1_H
#define smrd_flkt1_H
double flkt1(int kpnow,
             int kmod,
             int kdist,
             int weigi,
             Rcpp::NumericVector thetas,
             int nparm,
             Rcpp::NumericMatrix y,
             int ncoly,
             int nrownw,
             Rcpp::IntegerVector codes,
             Rcpp::IntegerVector weight,
             int ncolty,
             Rcpp::NumericMatrix ty,
             Rcpp::IntegerVector tcodes,
             Rcpp::NumericVector gamme,
             Rcpp::IntegerVector innow,
             int ngame);
#endif