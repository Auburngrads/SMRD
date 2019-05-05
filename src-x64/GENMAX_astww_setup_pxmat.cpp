#include <base/base.hpp>

void pxmat(Rcpp::NumericMatrix x){
  
for(int i = 0; i < x.nrow(); i++){
  
    Rcpp::Rcout << "\nPXMAT\n"                 << std::endl;
    Rcpp::Rcout << "i = "         << i         << std::endl;
    Rcpp::Rcout << "xmat(i,0) = " << x.at(i,0) << std::endl;
    
}
  
return;

}
