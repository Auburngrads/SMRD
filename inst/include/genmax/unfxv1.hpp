#ifndef smrd_unfxv1_H
#define smrd_unfxv1_H
void unfxv1(Rcpp::NumericMatrix &v,
            Rcpp::NumericVector &g,
            Rcpp::NumericVector &thetas,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            int &idim,
            Rcpp::NumericVector &gs,
            Rcpp::NumericVector &gd);
#endif