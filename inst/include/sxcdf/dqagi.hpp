#ifndef SMRD_dqagi_H
#define SMRD_dqagi_H
void dqagi(double (*f)(double),
           double &bound,
           int &inf,
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