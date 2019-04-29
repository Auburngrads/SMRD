#ifndef smrd_ccheck_H
#define smrd_ccheck_H
void ccheck(Rcpp::NumericVector &thetas,
            Rcpp::NumericVector &thtmp,
            int &nter,
            double &tol,
            int &lrel,
            int &lconv);
#endif