#ifndef smrd_wqm_riskset_H
#define smrd_wqm_riskset_H
void wqm_riskset(int &muniqrecurr,
                 Rcpp::NumericVector &tuniq,
                 int &nwindows,
                 Rcpp::NumericVector &twindowsl,
                 Rcpp::NumericVector &twindowsu,
                 Rcpp::IntegerVector &wcounts,
                 Rcpp::IntegerVector &iordl,
                 Rcpp::IntegerVector &iordu,
                 Rcpp::IntegerVector &delta,
                 Rcpp::IntegerVector &iscrat);
#endif