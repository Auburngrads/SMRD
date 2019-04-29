#ifndef SMRD_poweld_H
#define SMRD_poweld_H
void poweld(Rcpp::NumericVector &x,
            Rcpp::NumericVector &e,
            int &n,
            double &f,
            double &escale,
            int &iprpow,
            int &icon,
            int &maxit,
            double (*func)(Rcpp::NumericVector, int),
            int &ier);
#endif