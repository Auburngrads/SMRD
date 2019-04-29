#ifndef smrd_powelm_H
#define smrd_powelm_H
void powelm(Rcpp::NumericVector &x,
            Rcpp::NumericVector &e,
            int &n,
            double &f,
            double &escale,
            int &iprpow,
            int &icon,
            int &maxit,
            double (*func)(Rcpp::NumericVector, int),
            Rcpp::NumericVector &w,
            int &ier);
#endif