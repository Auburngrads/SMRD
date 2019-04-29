#ifndef smrd_stardt_H
#define smrd_stardt_H
void stardt(Rcpp::NumericVector &yplot,
            Rcpp::NumericVector &pplot,
            int &mplot,
            int &kdist,
            Rcpp::NumericVector &thetas,
            int &nparm,
            Rcpp::IntegerVector &intd,
            Rcpp::IntegerVector &nxd);
#endif