#ifndef smrd_cdfggg_H
#define smrd_cdfggg_H
void cdfggg(Rcpp::NumericMatrix &y,
            int &ny,
            Rcpp::IntegerVector &codes,
            Rcpp::IntegerVector &weight,
            Rcpp::NumericMatrix &ty,
            int &nty,
            Rcpp::IntegerVector &tcodes,
            int &n,
            int &nstart,
            int &maxit,
            double &tol,
            int &maxmsd,
            Rcpp::NumericVector &p,
            Rcpp::NumericVector &q,
            Rcpp::NumericVector &prob,
            Rcpp::NumericVector &sd,
            int &m,
            double &pchmax,
            int &lsd,
            int &ier);
#endif