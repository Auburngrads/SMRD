#ifndef smrd_gvec1_H
#define smrd_gvec1_H
void gvec1(Rcpp::List (*func)(Rcpp::List),
           int &npoint,
           Rcpp::NumericVector &theta,
           Rcpp::NumericVector &delta,
           int n,
           int &ktrcde,
           Rcpp::IntegerVector &kodet,
           Rcpp::NumericVector &d,
           Rcpp::NumericVector &tm,
           Rcpp::NumericVector &tp,
           Rcpp::NumericVector &tms,
           Rcpp::NumericVector &tps);
#endif