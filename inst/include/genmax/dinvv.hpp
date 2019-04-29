#ifndef smrd_dinvv_H
#define smrd_dinvv_H
void dinvv(Rcpp::NumericMatrix &amat,
           int n,
           double &xtol,
           int &irank,
           int &idim);
#endif