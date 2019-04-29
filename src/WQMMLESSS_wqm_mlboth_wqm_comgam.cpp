#include <base/base.hpp>

//' compute gamxxx from betxxx  gamxxx=dt'(betxxx)

void wqm_comgam(Rcpp::NumericVector &betxxx,
                Rcpp::NumericVector &diag,
                Rcpp::NumericMatrix &tmat,
                int &nparm,
                Rcpp::NumericVector &gamxxx){
  
for(int i1 = 0; i1 < nparm; i1++){
  
    gamxxx.at(i1) = (float)zero;
  
    for(int i2 = 0; i2 < nparm; i2++){
      
        gamxxx.at(i1) = (float)gamxxx.at(i1) + (float)diag.at(i1) * (float)tmat.at(i2,i1) * (float)betxxx.at(i2);
      
    }
  
}

      return;
  
}