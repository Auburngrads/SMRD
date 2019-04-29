#include <base/base.hpp>

//' Compute betxxx from gamxxx    betxxx=td^{-1}(gamxxx)

void wqm_combet(Rcpp::NumericVector &gamxxx,
                Rcpp::NumericVector &diag,
                Rcpp::NumericMatrix &tmat,
                int &nparm,
                Rcpp::NumericVector &betxxx){

for(int i1 = 0; i1 < nparm; i1++){
  
    betxxx.at(i1) = zero;
  
    for(int i2 = 0; i2 < nparm; i2++){
      
        betxxx.at(i1) = betxxx.at(i1) + tmat.at(i1,i2) * (one / diag.at(i2)) * gamxxx.at(i2);
      
    }
}

      return;
  
}