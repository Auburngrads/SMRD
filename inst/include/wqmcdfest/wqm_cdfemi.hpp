#ifndef smrd_wqm_cdfemi_H
#define smrd_wqm_cdfemi_H
void wqm_cdfemi(Rcpp::NumericVector &s,
                Rcpp::NumericVector &probd,
                int &m,
                Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &weight,
                int &nty,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                int &n,
                double &xnobs);
#endif
