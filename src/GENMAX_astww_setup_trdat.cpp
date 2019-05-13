#include <base/base.hpp>
#include <genmax/vtran.hpp>
#include <genmax/scaled.hpp>

//' Transform data in place. Take logs of y and ty (if ncolty > 0).
//' Scale and center data according to iscd.

void trdat(Rcpp::NumericMatrix &y,
           int &ncoly,
           int &nrownw,
           Rcpp::NumericMatrix &x,
           int &ncolx,
           Rcpp::IntegerVector &weight,
           Rcpp::NumericMatrix &ty,
           int &ncolty,
           int &llog,
           Rcpp::IntegerVector &iscd,
           Rcpp::NumericVector &xbar,
           Rcpp::NumericVector &xbaru,
           Rcpp::NumericVector &sd,
           int &ier){
  
// Need to put in check for log of a non negative number
   int ier1 = 0;
   int ier2 = 0;

// Take logs if needed
   if(llog != 0) {
     
      vtran(y,ncoly,nrownw,2,ier1);
     
      if(ncolty > 0) vtran(ty,ncolty,nrownw,2,ier1);

   }
   
int ncolxp = ncolx + 1;

for(int j = 1; j <= ncolxp; j++){
  
// Make sure of alignment with iscd and ncolx and ncolxp;
   scaled(x,j,weight,xbar.at(j - 1),
          xbaru.at(j - 1),sd.at(j - 1),nrownw,iscd.at(j - 1));
  
   if(debug::kprint >= 4) {
     
      Rcpp::Rcout << "\nTRDAT**4** j = " << j - 1 << std::endl;
      Rcpp::Rcout << "iscd(j) = "  << iscd.at(j - 1) << std::endl;
      Rcpp::Rcout << "xbar(j) = "  << xbar.at(j - 1) << std::endl;
      Rcpp::Rcout << "xbaru(j) = " << xbaru.at(j - 1) << std::endl;
      Rcpp::Rcout << "sd(j) = "    << sd.at(j - 1) << std::endl;
     
   }
     
   if((j > 1) and (sd.at(j - 1) <= zero)) ier2 = 1;
   
   }

// This needs work because ier is not set in vtran
// although an error message is printed
   ier = ier1 + 10 * ier2;

   if(ier > 0) {
      
      Rcpp::stop("\ntrdat: ier error\nier1 = %i\nier2 = %i\nier = %i",ier1,ier2,ier);
      
   }

   if(debug::kprint >= 3) {
      
      Rcpp::Rcout << "\nLEAVING TRDAT\n" << std::endl;
      
   }
   
return;

}
