#include <base/base.hpp>

//' Find the mean and standard deviation of a vector with weights

void descrb(Rcpp::NumericMatrix &mat,
            int col,
            Rcpp::IntegerVector &weight,
            int &n,
            double &xbar,
            double &sd){

double accum = 0.0e00;
int nobs = 0;

for(int i = 1; i <= n; i++){
  
    accum = accum + weight.at(i - 1) * mat.at(i - 1,col - 1);
    nobs = nobs + weight.at(i - 1);
    
}

xbar = accum / nobs;
accum = 0.0e00;

for(int i = 1; i <= n; i++){
  
    accum = accum + weight.at(i - 1) * std::pow(mat.at(i - 1,col - 1) - xbar, 2);
  
}
      
sd = std::sqrt(std::fabs(accum / (nobs - 1)));

      return;
      
}
