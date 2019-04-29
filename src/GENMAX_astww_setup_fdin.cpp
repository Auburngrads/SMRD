#include <base/base.hpp>

//' Looks at codes and weights and decides which rows are in
void fdin(Rcpp::IntegerVector &code,
          Rcpp::IntegerVector &weight,
          Rcpp::IntegerVector &inow,
          int nrownw){
  
if(nrownw <= 0) return;

for(int i = 1; i <= nrownw; i++){
  
    inow.at(i - 1) = 1;
  
    if((code.at(i - 1) <= 0) or (weight.at(i - 1) <= 0)) inow.at(i - 1) = 0;
      
}

return;
      
}
