#include <base/base.hpp>

//' pick up the coefficents in the linear transformation implied
//' by centering the explanatory variables
//' there is vector for each parameter showing its relationship
//' with all of the other parameters (i.e. theta(nparm) above)

void pkgvx(int &iterg,
           Rcpp::NumericVector &g1,
           int &nparm,
           int &nterg,
           int &intg,
           int &nxg,
           int &inthet,
           Rcpp::NumericVector &xbaru,
           Rcpp::IntegerVector &icolx){
   
int jj;
g1 = Rcpp::NumericVector(nparm,zero);
   
// inthet contains the index of the current par in theta
   g1.at(inthet - 1) = one;
   
// We are done if this is not an intercept in a regression
   if((nxg ==  0) or (intg == 0) or (iterg > 1)) return;
   
// Otherwise we are looking at an intercept
   for(int j = 2; j <= nterg; j++){
      
       jj = icolx.at(j - 1) + 1;
       g1.at((inthet - 1) + j - 2) = -1 * xbaru.at(jj - 1);
       
   }
   
return;
      
}