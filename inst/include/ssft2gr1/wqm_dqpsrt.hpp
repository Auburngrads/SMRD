#ifndef SMRD_wqm_dqpsrt_H
#define SMRD_wqm_dqpsrt_H
void wqm_dqpsrt(int &limit,
                int &last,
                int &maxerr,
                double &ermax,
                Rcpp::NumericVector &work,
                int &l3,
                Rcpp::IntegerVector &iord,
                int &nrmax);
#endif