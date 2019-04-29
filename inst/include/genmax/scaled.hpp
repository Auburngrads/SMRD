#ifndef smrd_scaled_H
#define smrd_scaled_H
void scaled(Rcpp::NumericMatrix &mat,
            int col,
            Rcpp::IntegerVector &weight,
            double &xbar,
            double &xbaru,
            double &sd,
            int &n,
            int &iscd);
#endif