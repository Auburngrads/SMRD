#include <base/base.hpp>
#include <wqmmlesss/wqm_deign.hpp>

//' Wrap around for computing and printing eigen analysis

void preign(Rcpp::NumericVector &amat,
            int &n,
            int &idim){
   
double xtol = 1.0e-10;
int nblank = 8 * idim;
   
// Get pointers to scratch arrays
   Rcpp::NumericMatrix ianew = Rcpp::NumericMatrix(idim, idim);
   Rcpp::NumericMatrix ievec = Rcpp::NumericMatrix(idim, idim);
   Rcpp::IntegerVector ipl   = Rcpp::IntegerVector(idim * 8);
   Rcpp::NumericVector ieval = Rcpp::NumericVector(idim);
   
// Compute eigenvalues (ret on diag of anew) and eigenvectors (in evec)
   wqm_deign(ianew,ievec,n,xtol,idim);

   if(debug::kprint >= 3) {
      
      Rcpp::Rcout << "\neigenvalues and orthogonal eigenvectors = \n" << ievec << std::endl;
      
   }

   return;
   
}
