#ifndef smrd_ilsxx_H
#define smrd_ilsxx_H
void ilsxx(Rcpp::NumericVector &thetas,
           Rcpp::NumericVector &thtmp,
           Rcpp::IntegerVector &kodet,
           Rcpp::IntegerVector &ifix,
           int &nter,
           int &nparm,
           Rcpp::NumericMatrix &y,
           int &ncoly,
           Rcpp::NumericMatrix &times,
           Rcpp::IntegerVector &codes,
           Rcpp::IntegerVector &weight,
           int &nrownw,
           Rcpp::NumericMatrix &x,
           int &ncolx,
           Rcpp::IntegerVector &iplab,
           Rcpp::NumericMatrix &vcv,
           Rcpp::NumericMatrix &r,
           Rcpp::NumericMatrix &yhat,
           Rcpp::NumericMatrix &resid,
           int &ier);
#endif