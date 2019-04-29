#ifndef smrd_unscv1_H
#define smrd_unscv1_H
void unscv1(Rcpp::NumericMatrix &vcvs,
            Rcpp::NumericMatrix &vcv,
            Rcpp::NumericMatrix &vcvn,
            int &idim,
            int &nparm,
            Rcpp::IntegerVector &nxg,
            Rcpp::IntegerVector &nterg,
            Rcpp::IntegerVector &intg,
            Rcpp::List &ipxcg,
            Rcpp::IntegerVector &ipthet,
            int &ngame,
            Rcpp::NumericVector &xbaru,
            Rcpp::NumericVector &sd,
            Rcpp::NumericVector &g1,
            Rcpp::NumericVector &g2);
#endif