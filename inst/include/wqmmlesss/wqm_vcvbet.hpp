#ifndef SMRD_wqm_vcvbet_H
#define SMRD_wqm_vcvbet_H
void wqm_vcvbet(Rcpp::NumericMatrix &vcvg,
                Rcpp::NumericVector &diag,
                Rcpp::NumericMatrix &tmat,
                int &nparm,
                Rcpp::NumericMatrix &vcvb);
#endif
