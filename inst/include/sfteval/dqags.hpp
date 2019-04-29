#ifndef smrd_dqags_H
#define smrd_dqags_H
void dqags(double (*f)(double),
           double &a,
           double &b,
           double &epsabs,
           double &epsrel,
           double &result,
           double &abserr,
           int &neval,
           int &ier,
           int &limit,
           int &lenw,
           int &last,
           Rcpp::IntegerVector &iwork,
           Rcpp::NumericVector &work);
#endif