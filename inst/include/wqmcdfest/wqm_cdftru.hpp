#ifndef smrd_wqm_cdftru_H
#define smrd_wqm_cdftru_H
void wqm_cdftru(Rcpp::NumericVector &ys,
                Rcpp::NumericMatrix &ty,
                int &nty,
                Rcpp::IntegerVector &tcodes,
                int &n,
                Rcpp::NumericVector &p,
                Rcpp::NumericVector &q,
                int &m,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                int &ier);

#endif
