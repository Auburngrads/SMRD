#include <base/base.hpp>

//' Fix lower cdf confidence limits to be monotone
//' 
//' @name bfixl
// [[Rcpp::export]]
Rcpp::List BFIXL(Rcpp::NumericVector vec){

  int n = vec.length();

for(int i = 1; i < n; i++){

      if(vec.at(i) < vec.at(i - 1)) {

         vec.at(i) = vec.at(i - 1);

      }
}

return Rcpp::List::create(Named("lower") = vec);

}



#include <base/base.hpp>

//' Fix upper cdf confidence limits to be monotone
//' 
//' @name bfixu
// [[Rcpp::export]]
Rcpp::List BFIXU(Rcpp::NumericVector vec){

  int n = vec.length();

  for(int i = (n - 1); i > 0; i--){

    // do in reverse
    if(vec.at(i - 1) > vec.at(i)) {

       vec.at(i - 1) = vec.at(i);

    }
  }
  
return Rcpp::List::create(Named("upper") = vec);
  
}