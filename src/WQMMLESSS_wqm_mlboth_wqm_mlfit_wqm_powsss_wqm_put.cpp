#include <base/base.hpp>

//' Make expanded parameter vector with fixed values
//' 
//' @param thetad Short vector used in Powell's algorithm.
//' @param thetg Contains the fixed values during the 
//'        iterations
//'        
//' @return Current full vector \code{thetaf}

NumericVector wqm_put(Rcpp::NumericVector thetaf,
                      Rcpp::NumericVector thetad,
                      Rcpp::NumericVector thetg,
                      Rcpp::LogicalVector lfix,
                      int nparm){
  
int n = 0;
  
for(int i = 1; i <= nparm; i++){
  
    thetaf.at(i - 1) = thetg.at(i - 1);
    if(lfix.at(i - 1)) continue;
    n = n + 1;
    thetaf.at(i - 1) = thetad.at(n - 1);
        
}

    return thetaf;

}
