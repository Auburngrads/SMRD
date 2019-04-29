#include <base/base.hpp>
#include <wqmmlesss/wqm_deriv.hpp>

//' Compute second derivative factors needed for the
//' Fisher information matrix with truncated data

void wqm_derivt(Rcpp::NumericVector &qder,
                int &itype,
                double &z,
                double &z2,
                int &nty,
                int &ittype,
                double &trl,
                double &tru,
                int &kdist){

Rcpp::NumericVector qdert(5);
  
// If no truncation, things are simple
if((nty == 0) or (ittype == 1)) {
  
    wqm_deriv(qder,itype,z,z2,kdist);
    return;
  
 }
  
    ittype = -1 * ittype;
    wqm_deriv(qdert,ittype,trl,tru,kdist);
    ittype = -1 * ittype;

   // If there is truncation, we need to get 
   // the appropriate numerator interval

   if((ittype == 2) and (itype == 2)) { // Right censor & Right truncated
   
      // Set the lower end of interval to be the right censoring point
      // Set the upper end of interval to be the right truncation point
         z2 = trl;
         itype = 4;
   
   }

   if((ittype == 3) and (itype == 3)) { // Left censor & Left truncated
      
      // Set the lower end of interval to be the left truncation point;
      // Set the upper end of interval to be the left censoring;
         z2 = z;
         z = trl;
         itype = 4;
   
   }
   
    wqm_deriv(qder,itype,z,z2,kdist);
   
   for(int i = 0; i < 5; i++){
     
       qder.at(i) = qder.at(i) - qdert.at(i);
     
   }
 
   return; 
 
}