#include <base/base.hpp>

//' Compute vcvb form vcvg    vcvb=td^{-1}(vcvg)d^{-1}t'

void wqm_vcvbet(Rcpp::NumericMatrix &vcvg,
                Rcpp::NumericVector &diag,
                Rcpp::NumericMatrix &tmat,
                int &nparm,
                Rcpp::NumericMatrix &vcvb){
  
for(int i1 = 0; i1 < nparm; i1++){
  
    for(int i4 = 0; i4 < nparm; i4++){
      
        vcvb.at(i1,i4) = zero;
      
        for(int i2 = 0; i2 < nparm; i2++){
          
            for(int i3 = 0; i3 < nparm; i3++){
              
                vcvb.at(i1,i4) = vcvb.at(i1,i4) + tmat.at(i1,i2) * (one / diag.at(i2)) * vcvg.at(i2,i3) * (one / diag.at(i3)) * tmat.at(i4,i3);
              
            }
        }
    }
}

      return;
}