#include <base/base.hpp>
#include <genmax/descrb.hpp>

//' Compute xbar and sd of vec. If iscd = 1, center and scale vec,
//' if iscd = 0, scale vec only
//' 
//' @param xbaru If \code{iscd = 1} \code{xbaru = xbar}, 
//'        if \code{iscd = 0} \code{xbaru = xbar} 

void scaled(Rcpp::NumericMatrix &mat,
            int col,
            Rcpp::IntegerVector &weight,
            double &xbar,
            double &xbaru,
            double &sd,
            int &n,
            int &iscd){
  
// If sd=0 is detected, set xbaru=0 and sd=1, assuming
// that we have found the constant term
   descrb(mat,col,weight,n,xbar,sd);
   xbaru = xbar;
   
   if(sd <= zero){
     
      sd = one;
      xbaru = zero;
      return;
     
   }
   
// If iscd = 0 we will only scale and not center
   if(iscd == 0) xbaru = zero;
   
   for(int i = 1; i <= n; i++){
     
       mat.at(i - 1,col - 1) = (mat.at(i - 1,col - 1) - xbaru) / sd;
     
   }
   
   return;
   
}
