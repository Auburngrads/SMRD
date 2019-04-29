#ifndef smrd_wqm_cdfcm_H
#define smrd_wqm_cdfcm_H
void wqm_cdfcm(Rcpp::IntegerVector &ilcv,
               Rcpp::IntegerVector &iucv,
               Rcpp::IntegerVector &iltv,
               Rcpp::IntegerVector &iutv,
               Rcpp::NumericVector &probd,
               Rcpp::NumericVector &f,
               Rcpp::IntegerVector &weight,
               int &m,
               int &n,
               int &nty,
               int &m1,
               int &mm1,
               double &small,
               int &mnzs,
               int &nnzs);
#endif
