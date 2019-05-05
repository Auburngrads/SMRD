#include <base/base.hpp>

//' Compute the proportion of units that failed

double wqm_fpfail(Rcpp::IntegerVector codes,
                  Rcpp::IntegerVector weight,
                  int nrownw){
        
int total = 0;
double pfail  = zero;
double fpfail = one;
int icode;
  
for(int i = 0; i < nrownw; i++){
  
   total = total + weight.at(i);
   icode = codes.at(i);
   
   if((icode != 2) and (icode > 0)) {
     
       pfail = pfail + weight.at(i);
     
   }

}

      if(total > 0) fpfail = pfail / total;
      
   return fpfail;
}
