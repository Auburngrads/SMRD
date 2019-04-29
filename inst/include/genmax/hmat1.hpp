#ifndef smrd_hmat1_H
#define smrd_hmat1_H
void hmat1(Rcpp::List (*func)(Rcpp::List),
           int &npoint,
           Rcpp::NumericVector &theta,
           Rcpp::NumericVector &delta,
           int &n,
           Rcpp::NumericMatrix &v,
           int &idim,
           Rcpp::NumericVector &tmm,
           Rcpp::NumericVector &tmp,
           Rcpp::NumericVector &tpm,
           Rcpp::NumericVector &tpp,
           Rcpp::NumericVector &tmms,
           Rcpp::NumericVector &tmps,
           Rcpp::NumericVector &tpms,
           Rcpp::NumericVector &tpps,
           Rcpp::NumericVector &tfunv);
#endif