#ifndef smrd_wqm_cdfstr_H
#define smrd_wqm_cdfstr_H
void wqm_cdfstr(Rcpp::NumericMatrix &y,
                int &ny,
                Rcpp::NumericVector &ys,
                Rcpp::NumericMatrix &ty,
                int &nty,
                Rcpp::IntegerVector &tcodes,
                Rcpp::IntegerVector &iorder,
                Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &codes,
                Rcpp::IntegerVector &weight,
                int &n,
                Rcpp::NumericVector &p,
                Rcpp::NumericVector &q,
                int &m);
#endif
