#ifndef smrd_dqelg_H
#define smrd_dqelg_H
void dqelg(int &n,
           Rcpp::NumericVector &epstab,
           double &result,
           double &abserr,
           Rcpp::NumericVector &res3la,
           int &nres);
#endif