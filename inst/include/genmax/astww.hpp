#ifndef smrd_astww_H
#define smrd_astww_H
void astww(int &kmod,
           int &kdist,
           Rcpp::IntegerVector &intd,
           Rcpp::IntegerVector &nxd,
           Rcpp::List &ipxcd,
           Rcpp::IntegerVector &irelad,
           int &npard,
           Rcpp::NumericVector &theta,
           Rcpp::NumericVector &thetas,
           Rcpp::IntegerVector &kodet,
           Rcpp::IntegerVector &ifix,
           int &nparm,
           Rcpp::NumericMatrix &ipy, // genmax_g::ipynew
           int &ncoly,
           int &nrownw,
           Rcpp::NumericMatrix &ipx,    // genmax_g::ipxnew
           int &ncolx, 
           Rcpp::IntegerVector &ipcode, // genmax_g::ipcode
           Rcpp::IntegerVector &ipweig, // genmax_g::ipweig
           Rcpp::NumericMatrix &ipty,   // genmax_g::iptynw
           int &ncolty,
           Rcpp::IntegerVector &iptc,   // genmax_g::iptc 
           int &kcentr,
           Rcpp::IntegerVector &ipplab, // genmax_g::iplabp
           double &pest,
           int &maxit,
           Rcpp::NumericMatrix &vcvs,
           Rcpp::NumericMatrix &vcv,
           Rcpp::NumericMatrix &r,
           int &maxpp,
           Rcpp::IntegerVector &ilabp,
           Rcpp::IntegerVector &ilabd,
           int &ier,
           int &kgtall,
           int &nregr,
           int &llog,
           int &kmodp,
           int &maxpd,
           double &pfail,
           int &kmccde,
           int &nstart,
           int &maxmsd,
           double &tol,
           int &lsd,
           double &pchmax);
#endif