#ifndef smrd_wlsq1_H
#define smrd_wlsq1_H
void wlsq1(Rcpp::NumericMatrix &y,
           Rcpp::IntegerVector &weight,
           Rcpp::NumericMatrix &x,
           int &nter,
           int &npoint,
           Rcpp::NumericVector &theta,
           Rcpp::IntegerVector &kodet,
           Rcpp::IntegerVector &iplab,
           int &nparm,
           Rcpp::NumericVector &beta,
           Rcpp::NumericMatrix &vcv,
           int &idim,
           Rcpp::NumericMatrix &resid,
           Rcpp::NumericMatrix &yhat,
           double &sd,
           Rcpp::IntegerVector &ic,
           Rcpp::IntegerVector &jc,
           Rcpp::NumericVector &yfix,
           int &ier);
#endif