#ifndef smrd_mlrsd1_H
#define smrd_mlrsd1_H
void mlrsd1(Rcpp::NumericMatrix &y,
            int &ncoly,
            int &nrownw,
            Rcpp::NumericVector &thetas,
            Rcpp::NumericVector &gamme,
            int &kdist,
            Rcpp::NumericVector &resid,
            Rcpp::NumericVector &yhat);
#endif