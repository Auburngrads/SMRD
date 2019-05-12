#include <base/base.hpp>

//' Check data for consistency

void wqm_cdfckd(Rcpp::NumericMatrix &y,
                int &ny,
                Rcpp::NumericMatrix &ty,
                int &nty,
                Rcpp::IntegerVector &codes,
                Rcpp::IntegerVector &weight,
                Rcpp::IntegerVector &tcodes,
                int &n,
                double &tol,
                int &ier){

double small = 0.001e00;
int itype;
ier = 0;
if(n <= 0)                       { ier = 1; return; }
if((ny < 1) or (ny > 2))         { ier = 2; return; }
if((nty < 0) or (nty > 2))       { ier = 3; return; }
if((tol < zero) or (tol > 0.10)) { ier = 4; return; }


for(int i = 1; i <= n; i++) {

    // check for negative weight
       if(weight.at(i - 1) < 0) { ier = 5; return; }

    // check the censor codes for consistancy
       itype = codes.at(i - 1);

    // skip dummy observations
       if(itype == 0) continue;
       if((itype < 0) or (itype > 5)) { ier = 6; return; }

    if(itype != 4) {

       if(std::fabs((y.at(i - 1, 0) - y.at(i - 1, ny - 1)) / std::max(y.at(i - 1, 0),small)) > small) {

          // type 1, 2, or 3 obs but yl != yu
             ier = 7; return; 
             
         } 
       
    }

     if(itype == 4) {
        
        // Check for backwards interval
           if(y.at(i - 1, 0) >= y.at(i - 1, ny - 1)) { ier = 8; return; }
 
        // Check for ny = 1, but there is a censor code 4    
           if(ny == 1) { ier = 9; return; }  

         }
     
     if(nty == 1) continue;

     // check the truncation codes for consistancy
        itype = tcodes.at(i - 1);
        
     // Check for tcode outside range 1 to 4
        if((itype < 1) or (itype > 4)) { ier = 10; return; }

        if(itype == 1) continue;

        if(itype != 4) {

           if(std::fabs((ty.at(i - 1, 0) - ty.at(i - 1, nty - 1)) / std::max(ty.at(i - 1, 0), small)) > small ) {

              // tcode=1, 2, or 3 but tyl != tyu
                 ier = 11; return;
                 
           }
           
        }

        if((itype == 2) or (itype == 3)) {
           
           // Check for backwards truncation interval
              if(ty.at(i - 1, 0) >= ty.at(i - 1, nty - 1)) { ier = 12; return; } 
              
           // Check for only 1 col of truncation values, code 4 or 5 found  
              if(nty == 1) { ier = 13; return; } 

         }

}

return;

}
