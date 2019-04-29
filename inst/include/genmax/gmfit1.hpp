#ifndef smrd_gmfit1_H
#define smrd_gmfit1_H
void gmfit1(int &ifit,
            Rcpp::NumericVector &thetat,
            Rcpp::NumericVector &thetas,
            Rcpp::IntegerVector &kodet,
            int &maxit,
            Rcpp::NumericVector &delta,
            Rcpp::NumericVector &fstder,
            Rcpp::IntegerVector &ilabp,
            double &xlogl,
            int &nparm,
            Rcpp::NumericMatrix &vcvs,
            Rcpp::NumericMatrix &vcvss,
            Rcpp::NumericMatrix &r,
            double &escale,
            double &epsx,
            int &nrownw,
            int &ier);
#endif