#include <base/base.hpp>

//' squish vcv to eliminate 0 rows and cols where kodet=0

void matsqu(Rcpp::NumericMatrix &vcv,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            int &idim){
  
int nparmm = 0,ii,jj;
  
for(int i = 1; i <= nparm; i++){
  
    if(kodet.at(i - 1) > 0) nparmm = nparmm + 1;
    
}

ii = 0;

for(int i = 1; i <= nparm; i++){
  
    if(kodet.at(i - 1) == 0) continue;
    ii = ii + 1;
    jj = 0;
    
    for(int j = 1; j <= nparm; j++){
      
        if(kodet.at(j - 1) == 0) continue;
        jj = jj + 1;
        vcv.at(ii - 1,jj - 1) = vcv.at(i - 1,j - 1);
        
    }
        
}

return;

}