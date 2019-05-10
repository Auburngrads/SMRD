#ifndef smrd_outpar_H
#define smrd_outpar_H
void outpar(int ifit,
            int kmod,
            int kdist,
            double xlike,
            Rcpp::NumericVector theta,
            Rcpp::IntegerVector kodet,
            Rcpp::IntegerVector ilabp,
            Rcpp::NumericMatrix vcv,
            Rcpp::NumericMatrix r,
            int nparm,
            double conlev,
            int idim);
#endif