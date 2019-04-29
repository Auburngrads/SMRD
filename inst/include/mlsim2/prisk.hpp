#ifndef SMRD_prisk_H
#define SMRD_prisk_H
void prisk(int &kdist,
           double &xmu,
           double &sigma,
           Rcpp::IntegerVector &nsamsz,
           Rcpp::NumericVector &centim,
           int &ngroup,
           double &prdelt,
           Rcpp::NumericVector &rhostr,
           Rcpp::IntegerVector &krfail,
           Rcpp::IntegerVector &nmrvec,
           double &crisk);
#endif
