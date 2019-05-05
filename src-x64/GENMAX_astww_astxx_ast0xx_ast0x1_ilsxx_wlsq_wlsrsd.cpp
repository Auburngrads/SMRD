#include <base/base.hpp>
#include <utility/icheck.hpp>

void wlsrsd(Rcpp::NumericMatrix &y,
            Rcpp::NumericMatrix &x,
            Rcpp::IntegerVector &weight,
            int &ny,
            int &nter,
            int &npoint,
            Rcpp::NumericVector &beta,
            Rcpp::NumericMatrix &resid,
            Rcpp::NumericVector &yhat,
            double &sd,
            int &ier){
   
sd = zero;
int nobsr = 0, indwg;
double accum, rhold;

for(int i = 1; i <= npoint; i++){
   
      accum = zero;
      indwg = weight.at(i - 1);
      nobsr = nobsr + indwg;
      
      for(int j = 1; j <= nter; j++){
         
          accum = accum + beta.at(j - 1) * x.at(i - 1,j - 1);
         
      }
      
      rhold = y.at(i - 1,0) - accum;
      yhat.at(i - 1) = accum;
      
      resid.at(i - 1,0) = rhold;
      sd = sd + std::pow(resid.at(i - 1,0),2);
      
}

      icheck(nobsr,1,1000000,1,nobsr,ier,-2116);
      sd    = std::sqrt(std::fabs(sd / (nobsr - nter)));
      return;
      
}