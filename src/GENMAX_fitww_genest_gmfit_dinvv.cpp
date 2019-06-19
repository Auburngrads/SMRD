#include <base/base.hpp>
#include <genmax/dinvx.hpp>

//' Wrap around for inversion without scratch arrays

void dinvv(Rcpp::NumericMatrix &amat,
           int n,
           double &xtol,
           int &irank,
           int &idim){

int np1 = n + 1;
   
// Get pointers to scratch arrays
   Rcpp::NumericMatrix ianew = Rcpp::NumericMatrix(np1,np1);
   Rcpp::IntegerVector iir   = Rcpp::IntegerVector(np1);
   Rcpp::IntegerVector ijc   = Rcpp::IntegerVector(np1);
   
   for(int i = 1; i <= n; i++){
      
       for(int j = 1; j <= n; j++){
          
           ianew.at(i - 1,j - 1) = amat.at(i - 1,j - 1);
          
       }
       
   }
   
   dinvx(ianew,n,xtol,iir,ijc,irank,np1);
   
   for(int i = 1; i <= n; i++){
      
       for(int j = 1; j <= n; j++){
          
           amat.at(i - 1,j - 1) = ianew.at(i - 1,j - 1);
          
       }
       
   }

return;
   
}