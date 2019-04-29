#ifndef smrd_estep_H
#define smrd_estep_H
void estep(int &nter,
           int &nparm,
           Rcpp::NumericMatrix &y,
           double &sigma,
           Rcpp::NumericMatrix &times,
           Rcpp::NumericMatrix &yhat,
           Rcpp::IntegerVector &codes,
           int &ncoly,
           int &nrownw,
           int &ier);
#endif