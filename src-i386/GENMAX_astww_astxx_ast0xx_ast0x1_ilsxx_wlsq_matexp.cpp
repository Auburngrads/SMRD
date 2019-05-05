#include <base/base.hpp>

//' Expand \code{vcv} to have 0 rows and cols where \code{kodet = 0}

void matexp(Rcpp::NumericMatrix &vcv,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            int &idim){

int nparmm = 0;
int ii,jj;
int inow,jnow;
  
for(int i = 1; i <= nparm; i++){
  
    if(kodet.at(i - 1) > 0) nparmm = nparmm + 1;

}

inow = nparmm + 1;

for(int i = 1; i <= nparm; i++){
  
    ii = (nparm - i) + 1;
    if(kodet.at(ii - 1) != 0) {
      
       inow = inow - 1;
       jnow = nparmm + 1;
       
       for(int j = 1; j <= nparm; j++){
         
           jj = (nparm - j) + 1;
         
           if(kodet.at(jj - 1) != 0) {
             
              jnow = jnow - 1;
              vcv.at(ii - 1,jj - 1) = vcv.at(inow - 1,jnow - 1);
              continue;
           
           }
           
           vcv.at(ii - 1,jj - 1) = 0.0e00;
       
       }
       
       continue;
       
    }
    
    for(int j = 1; j <= nparm; j++){
      
        vcv.at(ii - 1,j - 1) = 0.0e00;
      
    }
    
}

return;

}
