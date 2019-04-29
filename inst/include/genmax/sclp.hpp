#ifndef smrd_sclp_H
#define smrd_sclp_H
void sclp(int &ipoint,
          int &igame,
          Rcpp::NumericVector &theta,
          int &nx,
          int &Int,
          Rcpp::List &ipxcg,
          Rcpp::NumericVector &xbar,
          Rcpp::NumericVector &sd,
          Rcpp::NumericVector &thetas,
          Rcpp::IntegerVector &iscd);
#endif