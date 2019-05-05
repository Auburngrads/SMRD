#include <base/base.hpp>

//' fill vector \code{x} with \code{Const}

Rcpp::NumericVector wqm_fillr(double Const,
                              Rcpp::NumericVector x,
                              int n){

if(n > 0) {
  
   for(int i = 0; i < n; i++){
     
       x.at(i) = Const;
     
   }
   
}

      return x;
        
}
