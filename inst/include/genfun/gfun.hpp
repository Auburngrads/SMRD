#ifndef smrd_gfun_H
#define smrd_gfun_H
void gfun(Rcpp::List (*func)(Rcpp::List),
          Rcpp::NumericVector &thetas,
          Rcpp::NumericMatrix &vcvs,
          Rcpp::IntegerVector &kodet,
          int &nparm,
          int &igsd,
          double &fest,
          double &vest,
          double &epsx);
#endif