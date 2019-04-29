#include <base/base.hpp>

//' compute vcvg from vcvb  vcvg=dt'(vcvb)td

void wqm_vcvgam(Rcpp::NumericMatrix &vcvb,
                Rcpp::NumericVector &diag,
                Rcpp::NumericMatrix &tmat,
                int &nparm,
                Rcpp::NumericMatrix &vcvg){
  
for(int i1 = 0; i1 < nparm; i1++){
  
    for(int i4 = 0; i4 < nparm; i4++){
      
        vcvg.at(i1,i4) = zero;
      
    
        for(int i2 = 0; i2 < nparm; i2++){
          
            for(int i3 = 0; i3 < nparm; i3++){
              
                vcvg.at(i1,i4) = vcvg.at(i1,i4) + diag.at(i1) * tmat.at(i2,i1) * vcvb.at(i2, i3) * tmat.at(i3,i4) * diag.at(i4);
     
            }
        }
    }
}
        
      return;
  
}