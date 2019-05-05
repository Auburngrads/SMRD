#include <base/base.hpp>

//' Control expected failing at all sub-experiments

void cefail(Rcpp::NumericVector &fp,
            Rcpp::NumericVector &ratio,
            Rcpp::NumericVector &pi){
  
Rcpp::NumericVector fadj(3);
double det;
  
for(int i = 1; i <= 3; i++){
  
    fadj.at(i - 1) = fp.at(i - 1) / ratio.at(i - 1);
  
}

det = fadj.at(0) * fadj.at(1) + fadj.at(1) * fadj.at(2) + fadj.at(0) * fadj.at(2);

pi.at(0) = fadj.at(1) * fadj.at(2) / det;
pi.at(1) = fadj.at(0) * fadj.at(2) / det;

return;

}
