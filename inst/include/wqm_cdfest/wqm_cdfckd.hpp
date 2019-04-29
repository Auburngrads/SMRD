#ifndef smrd_wqm_cdfckd_H
#define smrd_wqm_cdfckd_H
void wqm_cdfckd(Rcpp::NumericMatrix &y,
                int &ny,
                Rcpp::NumericMatrix &ty,
                int &nty,
                Rcpp::IntegerVector &codes,
                Rcpp::IntegerVector &weight,
                Rcpp::IntegerVector &tcodes,
                int &n,
                double &tol,
                int &ier);
#endif
