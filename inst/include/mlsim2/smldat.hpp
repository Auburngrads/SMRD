#ifndef SMRD_smldat_H
#define SMRD_smldat_H
void smldat(Rcpp::NumericVector &theta,
            int &nparm,
            Rcpp::IntegerVector &nsamsz, // igroup
            int &kctype,
            Rcpp::NumericVector &centim, // igroup
            double &pretim,
            int &kdist,
            Rcpp::NumericMatrix &x, // iobs
            Rcpp::NumericMatrix &y, // iobs
            Rcpp::IntegerVector &cen, // iobs
            Rcpp::IntegerVector &wt, // iobs
            int &nrow,
            int &nter,
            int &ny,
            int &nty,
            Rcpp::NumericMatrix &ty, // iobs
            Rcpp::IntegerVector &tcodes, // iobs
            int &nrownw,
            Rcpp::IntegerVector &krfail, // igroup
            int &igroup,
            int &iref,
            int &kpred,
            int &iersim);
#endif
