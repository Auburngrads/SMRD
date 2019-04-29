#ifndef SMRD_wqm_dfxmu_H
#define SMRD_wqm_dfxmu_H
double wqm_dfxmu(int i,
                 Rcpp::NumericMatrix xnew,
                 int nrow,
                 int nter,
                 Rcpp::NumericVector thetaf,
                 int nparm,
                 double upcen,
                 double sigma);
#endif
