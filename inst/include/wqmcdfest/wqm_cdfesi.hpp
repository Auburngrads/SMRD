#ifndef SMRD_wqm_cdfesi_H
#define SMRD_wqm_cdfesi_H
void wqm_cdfesi(Rcpp::IntegerVector &weight,
                Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                Rcpp::NumericVector &probd,
                Rcpp::NumericVector &sd,
                int &m,
                int &n,
                double small,
                int &nty,
                int &ier,
                int &maxmsd);
#endif
