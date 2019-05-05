#include <base/base.hpp>

//' Compute a correlation matrix from the vcv matrix;

void wqm_corr(Rcpp::NumericMatrix &vcv,
              Rcpp::NumericMatrix &r,
              int &nparm){
  
double denom;
  
for(int k = 1; k <= nparm; k++){
  
    for(int j = 1; j <= k; j++){
      
    if((vcv.at(k - 1,k - 1) <= zero) or (vcv.at(j - 1,j - 1) <= zero)) {
      
        r.at(k - 1,j - 1) = zero;
        r.at(j - 1,k - 1) = zero;
        continue;
      
      }
    
    if(k == j) { r.at(k - 1,j - 1) = one; continue; }
      
       denom = std::sqrt(vcv.at(k - 1,k - 1) * vcv.at(j - 1,j - 1));
      
       if(denom <= zero) {
         
          r.at(k - 1,j - 1) = zero;
          r.at(j - 1,k - 1) = zero;
          continue;
         
       }
       
       r.at(k - 1,j - 1) = vcv.at(k - 1,j - 1) / denom;
       r.at(j - 1,k - 1) = r.at(k - 1,j - 1);
    
    }
}

   return;

}
