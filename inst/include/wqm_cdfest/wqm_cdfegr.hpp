#ifndef smrd_wqm_cdfegr_H
#define smrd_wqm_cdfegr_H
void wqm_cdfegr(Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                Rcpp::IntegerVector &weight,
                int &nty,
                int &n,
                Rcpp::NumericVector &probd,
                int &m,
                Rcpp::NumericVector &pgrad);
#endif
