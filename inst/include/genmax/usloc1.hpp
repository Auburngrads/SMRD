#ifndef smrd_usloc1_H
#define smrd_usloc1_H
void usloc1(Rcpp::NumericVector &theta,
            Rcpp::IntegerVector &kodet,
            Rcpp::NumericMatrix &vcv,
            Rcpp::NumericVector &vhold,
            Rcpp::NumericVector &delta,
            Rcpp::NumericVector &grad,
            int &nparm,
            int &idim);
#endif