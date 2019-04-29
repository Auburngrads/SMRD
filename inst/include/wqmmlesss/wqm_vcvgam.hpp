#ifndef SMRD_wqm_vcvgam_H
#define SMRD_wqm_vcvgam_H
void wqm_vcvgam(Rcpp::NumericMatrix &vcvb,
                Rcpp::NumericVector &diag,
                Rcpp::NumericMatrix &tmat,
                int &nparm,
                Rcpp::NumericMatrix &vcvg);
#endif
