#ifndef smrd_pargen_H
#define smrd_pargen_H
void pargen(Rcpp::NumericVector &theta,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            Rcpp::IntegerVector &ilabp,
            double &conlev,
            Rcpp::NumericMatrix &vcvs,
            int &idim);
#endif