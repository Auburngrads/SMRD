#ifndef SMRD_wqm_tran_H
#define SMRD_wqm_tran_H
void wqm_tran(Rcpp::NumericMatrix &y,
              Rcpp::IntegerVector &cen,
              Rcpp::IntegerVector &wt,
              int &nrow,
              int &ny,
              Rcpp::NumericVector &gamthr,
              bool ltrunc,
              bool &ltr3,
              int &ier);
#endif
