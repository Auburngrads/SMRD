#ifndef smrd_wlsrsd_H
#define smrd_wlsrsd_H
void wlsrsd(Rcpp::NumericMatrix &y,
            Rcpp::NumericMatrix &x,
            Rcpp::IntegerVector &weight,
            int &ny,
            int &nter,
            int &npoint,
            Rcpp::NumericVector &beta,
            Rcpp::NumericMatrix &resid,
            Rcpp::NumericVector &yhat,
            double &sd,
            int &ier);
#endif