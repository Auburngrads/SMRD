#ifndef smrd_dinvx_H
#define smrd_dinvx_H
void dinvx(Rcpp::NumericMatrix &a,
           int &ntc,
           double xtol,
           Rcpp::IntegerVector &ir,
           Rcpp::IntegerVector &jc,
           int &irank,
           int &idim);
#endif