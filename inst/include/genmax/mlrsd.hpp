#ifndef smrd_mlrsd_H
#define smrd_mlrsd_H
void mlrsd(Rcpp::NumericMatrix &ipy,
           int &ncoly,
           int &nrownw,
           Rcpp::NumericVector &thetas,
           int &kdist,
           Rcpp::NumericVector &resid,
           Rcpp::NumericVector &yhat);
#endif