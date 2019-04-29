#ifndef smrd_gmfit_H
#define smrd_gmfit_H
void gmfit(int &ifit,
           Rcpp::NumericVector &thetas,
           Rcpp::IntegerVector &kodet,
           int &maxit,
           Rcpp::NumericVector &fstder,
           Rcpp::IntegerVector &ipplab,
           double &xlogl,
           int &nparm,
           Rcpp::NumericMatrix &vcvs,
           Rcpp::NumericMatrix &r,
           double &escale,
           double &epsx,
           int &nrownw,
           int &ier);
#endif