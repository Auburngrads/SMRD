#ifndef smrd_wlsq_H
#define smrd_wlsq_H
void wlsq(Rcpp::NumericMatrix &y,
          Rcpp::IntegerVector &weight,
          Rcpp::NumericMatrix &x,
          int &nter,
          int &npoint,
          Rcpp::NumericVector &theta,
          Rcpp::IntegerVector &kodet,
          Rcpp::IntegerVector &iplab,
          Rcpp::NumericMatrix &vcv,
          int &idim,
          Rcpp::NumericMatrix &resid,
          Rcpp::NumericMatrix &yhat,
          double &sd,
          int &ier);
#endif