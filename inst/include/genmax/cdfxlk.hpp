#ifndef smrd_cdfxlk_H
#define smrd_cdfxlk_H
void cdfxlk(Rcpp::IntegerVector &ilcv,
            Rcpp::IntegerVector &iucv,
            Rcpp::IntegerVector &iltv,
            Rcpp::IntegerVector &iutv,
            Rcpp::IntegerVector &weight,
            int &nty,
            int &n,
            Rcpp::NumericVector &probd,
            int &m,
            double &xllgkm);
#endif