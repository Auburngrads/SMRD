#ifndef smrd_unfixv_H
#define smrd_unfixv_H
void unfixv(Rcpp::NumericMatrix &v,
            Rcpp::NumericVector &g,
            Rcpp::NumericVector &thetas,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            int &idim);
#endif