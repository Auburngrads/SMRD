#include <base/base.hpp>

// Pivoting routine

void wqm_dpivot(Rcpp::NumericMatrix &a,
                int &is,
                int &nr,
                int &nc,
                int &idim){
  
double pivot = a.at(is - 1,is - 1);
  
for(int j = 1; j <= nc; j++){
  
    a.at(is - 1,j - 1) = a.at(is - 1,j - 1) / pivot;
  
}

for(int i = 1; i <= nr; i++){
  
    if(i == is) continue;
    pivot = a.at(i - 1,is - 1);
    
    for(int j = 1; j <= nc; j++){
      
        a.at(i - 1,j - 1) = a.at(i - 1,j - 1) - pivot * a.at(is - 1,j - 1);
      
    }
}

  return;

}