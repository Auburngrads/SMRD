#ifndef SMRD_wqm_svdfix_H
#define SMRD_wqm_svdfix_H
void wqm_svdfix(Rcpp::LogicalVector &lfix,
                int &nrow,
                int &nter,
                int &nparm,
                Rcpp::NumericMatrix &xold,
                Rcpp::NumericMatrix &xnew,
                Rcpp::NumericVector &diag,
                Rcpp::NumericMatrix &tmat,
                double &dgmin,
                int &ierr,
                Rcpp::NumericMatrix &vcvg,
                Rcpp::NumericVector &rv1,
                bool &lsvd);
#endif
