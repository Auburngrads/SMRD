#ifndef smrd_fdiscd_H
#define smrd_fdiscd_H
void fdiscd(Rcpp::IntegerVector &iscd,
            int &ncolx,
            Rcpp::IntegerVector &intd,
            Rcpp::List &ipxcd,
            Rcpp::IntegerVector &irelad,
            Rcpp::IntegerVector &nxd,
            int &npard,
            int &kcentr,
            int &kmod,
            int &ier);
#endif