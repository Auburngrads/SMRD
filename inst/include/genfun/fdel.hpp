#ifndef smrd_fdel_H
#define smrd_fdel_H
void fdel(Rcpp::List (*func)(Rcpp::List),
          Rcpp::NumericVector &theta,
          int &ktrcde,
          Rcpp::IntegerVector &kodet,
          int &n,
          double &eps,
          Rcpp::NumericVector &delta);
#endif