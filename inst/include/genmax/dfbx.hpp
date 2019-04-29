#ifndef smrd_dfbx_H
#define smrd_dfbx_H
double dfbx(int kpnow,
            Rcpp::NumericVector thetas,
            int ipthta,
            Rcpp::IntegerVector ipxcg,
            int nterg,
            int intg,
            int irelag);
#endif