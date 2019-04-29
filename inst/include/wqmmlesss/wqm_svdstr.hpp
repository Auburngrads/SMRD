#ifndef SMRD_wqm_svdstr_H
#define SMRD_wqm_svdstr_H
void wqm_svdstr(int &nrow,
                int &nter,
                int &nparm,
                Rcpp::LogicalVector &lfix,
                Rcpp::NumericMatrix &xold,
                Rcpp::NumericVector &w,
                Rcpp::NumericMatrix &u,
                Rcpp::NumericMatrix &v,
                int &ierr,
                Rcpp::NumericVector &rv1);
#endif
