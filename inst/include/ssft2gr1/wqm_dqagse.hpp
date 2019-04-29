#ifndef SMRD_wqm_dqagse_H
#define SMRD_wqm_dqagse_H
void wqm_dqagse(double (*f)(double),
                double &a,
                double &b,
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