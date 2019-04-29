#ifndef SMRD_wqm_put_H
#define SMRD_wqm_put_H
NumericVector wqm_put(Rcpp::NumericVector thetaf,
                      Rcpp::NumericVector thetad,
                      Rcpp::NumericVector thetg,
                      Rcpp::LogicalVector lfix,
                      int nparm);
#endif
