#ifndef smrd_hmat_H
#define smrd_hmat_H
void hmat(Rcpp::List (*func)(Rcpp::List),
          int &npoint,
          Rcpp::NumericVector &theta,
          Rcpp::NumericVector &delta,
          int &n,
          Rcpp::NumericMatrix &v,
          int &idim);
#endif