#ifndef smrd_estim_H
#define smrd_estim_H
void estim(Rcpp::NumericVector &thetat,
           Rcpp::IntegerVector &kodet,
           int &nparm,
           int &nparmm,
           double &tol,
           double &stepmx,
           int &maxit,
           double &xlogl,
           double (*func)(Rcpp::NumericVector, int),
           int &iprpow,
           Rcpp::IntegerVector &ilabp,
           int &ier);
#endif