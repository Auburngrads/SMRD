#include <base/base.hpp>

//' Pick up sale factors for the parameters from sd

void pkgvs(Rcpp::NumericVector &g1,
           int &nparm,
           int &inthet,
           int &nterg,
           Rcpp::IntegerVector &icolx,
           Rcpp::NumericVector &sd){
   
int jj, index;
   
for(int j = 1; j <= nterg; j++){
   
    jj = icolx.at(j - 1) + 1;
    index = inthet + j - 1;
    g1.at(index - 1) = sd.at(jj - 1);
    
}

return;
   
}
