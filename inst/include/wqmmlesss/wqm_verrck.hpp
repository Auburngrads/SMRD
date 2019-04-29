#ifndef SMRD_wqm_verrck_H
#define SMRD_wqm_verrck_H
void wqm_verrck(Rcpp::NumericMatrix &vcvd,
                Rcpp::NumericMatrix &vcvdd,
                Rcpp::NumericVector &fsderd,
                Rcpp::LogicalVector &lfix,
                double &xtol,
                int &nparm,
                int &nparmm,
                int &ier);
#endif
