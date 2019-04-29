#ifndef smrd_cdfcvf_H
#define smrd_cdfcvf_H
void cdfcvf(Rcpp::NumericVector &p,
            Rcpp::NumericVector &q,
            Rcpp::NumericVector &prob,
            Rcpp::NumericVector &sd,
            int &lsd,
            int &m);
#endif