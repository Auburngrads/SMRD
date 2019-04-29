#ifndef SMRD_wqm_dqags_H
#define SMRD_wqm_dqags_H
void wqm_dqags(double (*f)(double),
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