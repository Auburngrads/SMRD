#include <base/base.hpp>

//' Copy real xr to double xd

Rcpp::List wqm_copyrd(Rcpp::NumericVector xr,
                      Rcpp::NumericVector xd,
                      int n){
  
if(n > 0){
  
  for(int i = 0; i < n; i++) {
    
      xd.at(i) = xr.at(i);
    
  }
  
}

      return Rcpp::List::create(Named("xr") = xr,
                                Named("xd") = xd);
}
