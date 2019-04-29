#ifndef smrd_dfxb1_H
#define smrd_dfxb1_H
double dfbx1(int kpnow,
             Rcpp::NumericMatrix x,
             Rcpp::NumericVector xbaru,
             Rcpp::NumericVector sd,
             Rcpp::IntegerVector ipcolx,
             int nrownw,
             int irelag,
             int nterg,
             int intg,
             Rcpp::NumericVector thetas,
             int ipthta,
             Rcpp::NumericVector xsave);
#endif