#ifndef smrd_wqm_plestx_H
#define smrd_wqm_plestx_H
void wqm_plestx(Rcpp::NumericVector &enter,
                Rcpp::NumericVector &failr,
                Rcpp::NumericVector &failp,
                Rcpp::NumericVector &xlose,
                int &m,
                int &maxiuc,
                int &minilc,
                int irev,
                Rcpp::NumericVector &prob,
                Rcpp::NumericVector &sd);
#endif
