#ifndef smrd_matexp_H
#define smrd_matexp_H
void matexp(Rcpp::NumericMatrix &vcv,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            int &idim);
#endif