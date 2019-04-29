#ifndef smrd_funint_H
#define smrd_funint_H
void funint(Rcpp::List (*func)(Rcpp::List),
            int &ifkode,
            double &conlev,
            Rcpp::NumericVector &thetas,
            Rcpp::NumericMatrix &vcvs,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            double &epsx,
            double &fest,
            double &s,
            double &xl,
            double &xu);
#endif