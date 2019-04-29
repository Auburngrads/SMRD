#ifndef smrd_descrb_H
#define smrd_descrb_H
void descrb(Rcpp::NumericMatrix &vec,
            int col,
            Rcpp::IntegerVector &weight,
            int &n,
            double &xbar,
            double &sd);
#endif