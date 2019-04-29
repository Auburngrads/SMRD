#ifndef smrd_dqpsrt_H
#define smrd_dqpsrt_H
void dqpsrt(int &limit,
            int &Last,
            int &maxerr,
            double &ermax,
            Rcpp::NumericVector &work,
            int &l3,
            Rcpp::IntegerVector &iord,
            int &nrmax);
#endif