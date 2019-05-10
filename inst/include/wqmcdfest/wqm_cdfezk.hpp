#ifndef smrd_wqm_cdfezk_H
#define smrd_wqm_cdfezk_H
void wqm_cdfezk(Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                Rcpp::IntegerVector &weight,
                int &nty,
                int &n,
                Rcpp::NumericVector &probd,
                int &m,
                Rcpp::NumericVector &pgrad,
                double &biggr,
                double &smpro,
                int &indc);
#endif
