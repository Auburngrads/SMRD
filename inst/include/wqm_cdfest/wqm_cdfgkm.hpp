#ifndef smrd_wqm_cdfgkm_H
#define smrd_wqm_cdfgkm_H
void wqm_cdfgkm(Rcpp::IntegerVector &weight,
                Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                Rcpp::NumericVector &fail,
                Rcpp::NumericVector &xrcen,
                Rcpp::NumericVector &xlcen,
                Rcpp::NumericVector &xltru,
                Rcpp::NumericVector &xrtru,
                Rcpp::NumericVector &prob,
                Rcpp::NumericVector &sd,
                int &n,
                int &m,
                int &nty,
                int &ier);
#endif
