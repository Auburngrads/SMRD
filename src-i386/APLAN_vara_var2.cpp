#include <base/base.hpp>
#include <vvar1/var1.hpp>

//' Calculate variance of percentile at some stress level
//' @param zivar is the variable defining that level (usually set to zero for design stress)
//' @details This is a wrap that does not return \code{f(5,5)}

double var2(double a,
            double b1,
            double b2,
            double thet1,
            Rcpp::NumericVector z,
            Rcpp::NumericVector pi,
            double zivar,
            int nlev,
            double perc,
            int idist,
            int knownt,
            Rcpp::NumericVector fp,
            Rcpp::NumericVector pq){
  
Rcpp::NumericMatrix f(5,5);
int index = 0;
double var_2;
  
var_2 = var1(a,b1,b2,thet1,z,pi,zivar,
             nlev,perc,idist,knownt,fp,pq,f,index);
  
return var_2;
  
}
