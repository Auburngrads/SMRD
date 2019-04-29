#ifndef smrd_estim1_H
#define smrd_estim1_H
void estim1(Rcpp::NumericVector &thetat,
            Rcpp::NumericVector &thetam,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            int &nparmm,
            Rcpp::IntegerVector &ilabp,
            double &tol,
            double &stepmx,
            int &icon,
            int &maxit,
            double &xlogl,
            double (*func)(Rcpp::NumericVector, int),
            Rcpp::NumericVector &e,
            Rcpp::NumericVector &work,
            int iprpow,
            int &ier);
#endif