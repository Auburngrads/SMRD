#ifndef SMRD_wqm_dinsss_H
#define SMRD_wqm_dinsss_H
void wqm_dinsss(Rcpp::NumericMatrix &a,
                int &ntc,
                double &xtol,
                int &irank,
                Rcpp::IntegerVector &jc,
                Rcpp::IntegerVector &ir,
                int &idim);
#endif
