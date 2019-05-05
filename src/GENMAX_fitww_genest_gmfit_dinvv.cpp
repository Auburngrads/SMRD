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
   
   dinvx(ianew,n,xtol,iir,ijc,irank,np1);
   
   amat = clone(ianew);
   
return;
   
}