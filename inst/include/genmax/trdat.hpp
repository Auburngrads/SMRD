#ifndef smrd_trdat_H
#define smrd_trdat_H
void trdat(Rcpp::NumericMatrix &y,
           int &ncoly,
           int &nrownw,
           Rcpp::NumericMatrix &x,
           int &ncolx,
           Rcpp::IntegerVector &weight,
           Rcpp::NumericMatrix &ty,
           int &ncolty,
           int &llog,
           Rcpp::IntegerVector &iscd,
           Rcpp::NumericVector &xbar,
           Rcpp::NumericVector &xbaru,
           Rcpp::NumericVector &sd,
           int &ier);
#endif