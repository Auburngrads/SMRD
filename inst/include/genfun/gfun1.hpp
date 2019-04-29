#ifndef smrd_gfun1_H
#define smrd_gfun1_H
void gfun1(Rcpp::List (*func)(Rcpp::List),
           Rcpp::NumericVector &thetas,
           Rcpp::NumericMatrix &vcvs,
           Rcpp::IntegerVector &kodet,
           int &nparm,
           int &igsd,
           double &fest,
           double &vest,
           Rcpp::NumericVector &grad,
           Rcpp::NumericVector &delta,
           double &epsx);
#endif