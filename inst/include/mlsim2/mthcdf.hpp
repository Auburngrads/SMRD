#ifndef SMRD_mthcdf_H
#define SMRD_mthcdf_H
void mthcdf(Rcpp::NumericVector &pold,
            Rcpp::NumericVector &qold,
            Rcpp::NumericVector &prbold,
            Rcpp::NumericVector &sdold,
            int &mold,
            Rcpp::NumericVector &pnew,
            Rcpp::NumericVector &qnew,
            Rcpp::NumericVector &prbnew,
            Rcpp::NumericVector &sdnew,
            int &mnew,
            Rcpp::NumericVector &prtvec,
            Rcpp::NumericVector &srtvec);
#endif
