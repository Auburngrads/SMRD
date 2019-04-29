#ifndef SMRD_parisi_H
#define SMRD_parisi_H
void parisi(Rcpp::NumericVector &times,
            int &number_times,
            Rcpp::NumericVector &paris_n,
            Rcpp::NumericVector &paris_c,
            Rcpp::NumericVector &paris_sigma, 
            double &a0,
            Rcpp::NumericVector &crack_size);
#endif