#include <base/base.hpp>

//' @description Set standard deviations equal to
//'              zero if there is only one nonzero
//'              step probability or if the matrix
//'              is too big.

void wqm_cdfzs(int &m,
               int &ier,
               Rcpp::NumericVector &sd,
               int &nnzs,
               int &maxmsd){

for(int i = 1; i <= m; i++) { 
  
    sd.at(i - 1) = zero; 
  
}

    ier = 23;

    if(nnzs > maxmsd) ier = 21;

return;
  
}
