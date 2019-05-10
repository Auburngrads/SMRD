#ifndef smrd_wqm_cdfema_H
#define smrd_wqm_cdfema_H
void wqm_cdfema(Rcpp::IntegerVector &weight,
                Rcpp::IntegerVector &codes,
                Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                Rcpp::NumericVector &pgrad,
                Rcpp::NumericVector &prob,
                Rcpp::NumericVector &s,
                Rcpp::NumericVector &probd,
                int &n,
                int &m,
                int &nty,
                int &nstart,
                int &maxit,
                double &tol,
                double &pchmax);
#endif
