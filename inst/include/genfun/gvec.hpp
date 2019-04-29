#ifndef smrd_gvec_H
#define smrd_gvec_H
void gvec(Rcpp::List (*func)(Rcpp::List),
          int &npoint,
          Rcpp::NumericVector &theta,
          Rcpp::NumericVector &delta,
          int &n,
          int &ktrcde,
          Rcpp::IntegerVector &kodet,
          Rcpp::NumericVector &d);
#endif