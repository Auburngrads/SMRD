#ifndef SMRD_wqm_points_H
#define SMRD_wqm_points_H
void wqm_points(Rcpp::NumericVector &q,
                Rcpp::NumericVector &p,
                Rcpp::NumericVector &prob,
                Rcpp::NumericVector &sd,
                int &lsd,
                int &m,
                Rcpp::NumericVector &yplot,
                Rcpp::NumericVector &pplot,
                Rcpp::NumericVector &sdplot,
                int &mplot);
#endif
