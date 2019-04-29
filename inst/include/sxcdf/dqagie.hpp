#ifndef SMRD_dqagie_H
#define SMRD_dqagie_H
void dqagie(double (*f)(double),
            double &bound,
            int &inf,
            double &epsabs,
            double &epsrel,
            int &limit,
            double &result,
            double &abserr,
            int &neval,
            int &ier,
            Rcpp::NumericVector &work,
            int &l1,
            int &l2,
            int &l3,
            Rcpp::IntegerVector &iord,
            int &last);
#endif