#ifndef smrd_wqm_cdffac_H
#define smrd_wqm_cdffac_H
void wqm_cdffac(int &il,
                int &iu,
                Rcpp::NumericVector &f,
                Rcpp::NumericVector &probd,
                int &wt,
                bool &ltrunc,
                double &small,
                int &nnzs,
                int &mnzs,
                int &m,
                int &nty);
#endif
