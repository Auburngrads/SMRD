#include <base/base.hpp>

//' Compute a correlation matrix from the vcv matrix
//' enter zero row/col for zero variance

void corrg(Rcpp::NumericMatrix &vcv,
           Rcpp::NumericMatrix &r,
           int &nparm){
   
for(int k = 1; k <= nparm; k++){
   
    for(int j = 1; j <= nparm; j++){
       
        r.at(k - 1,j - 1) = 0.0e00;
         
        if(vcv.at(k - 1,k - 1) * vcv.at(j - 1,j - 1) <= 0.0e00) continue;
        
        r.at(k - 1,j - 1) = vcv.at(k - 1,j - 1) / std::sqrt(std::abs(vcv.at(k - 1,k - 1) * vcv.at(j - 1,j - 1)));
      
    }
}

return;
   
}
