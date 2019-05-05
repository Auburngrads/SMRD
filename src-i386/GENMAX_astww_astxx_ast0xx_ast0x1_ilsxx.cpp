#include <base/base.hpp>
#include <genmax/estep.hpp>
#include <genmax/wlsq.hpp>
#include <genmax/ccheck.hpp>
#include <utility/wqm_copyd.hpp>

//' Iterative least squares for normal distribution used 
//' for getting good location parameter regression parameter 
//' estimates;

void ilsxx(Rcpp::NumericVector &thetas,
           Rcpp::NumericVector &thtmp,
           Rcpp::IntegerVector &kodet,
           Rcpp::IntegerVector &ifix,
           int &nter,
           int &nparm,
           Rcpp::NumericMatrix &y,
           int &ncoly,
           Rcpp::NumericMatrix &times,
           Rcpp::IntegerVector &codes,
           Rcpp::IntegerVector &weight,
           int &nrownw,
           Rcpp::NumericMatrix &x,
           int &ncolx,
           Rcpp::IntegerVector &iplab,
           Rcpp::NumericMatrix &vcv,
           Rcpp::NumericMatrix &r,
           Rcpp::NumericMatrix &yhat,
           Rcpp::NumericMatrix &resid,
           int &ier){

double tol = 1.0e-02;
int maxitr = 25;
double sigma = 0.0e00;
int nitr = 0;

// Do some debug printing
   if(debug::kprint > 4) Rcpp::Rcout << "in ils, nrownw = " << nrownw << std::endl;

// Copy over y to times
   times = clone(y);

// Use ols for starting values for the em algorithm
   wlsq(times,weight,x,nter,nrownw,thetas,kodet,
        iplab,vcv,nparm,resid,yhat,sigma,ier);

if(debug::kprint >= 3) {
   
   Rcpp::Rcout << "\nILSXX**3**\n"    << std::endl;
   Rcpp::Rcout << "sigma = "   << sigma << std::endl;
   Rcpp::Rcout << "resid = \n" << resid << std::endl;
}

line1: 
  
// Make copy of thetas for convergence checks
   thtmp = clone(thetas);
   nitr = nitr + 1;

// Perform the e-step of the em algorithm
   estep(nter,nparm,y,sigma,times,yhat,
         codes,ncoly,nrownw,ier);

// Perform the m-step with least squares
   wlsq(times,weight,x,nter,nrownw,thetas,kodet,
        iplab,vcv,nparm,resid,yhat,sigma,ier);
   
if(debug::kprint >= 3) {
  
   Rcpp::Rcout << "nitr = "   << nitr   << std::endl;
   Rcpp::Rcout << "maxitr = " << maxitr << std::endl;
   Rcpp::Rcout << "thetas = " << thetas << std::endl;
   
}

// Check for convergence of the e-m algorithm
   int lrel = 1;
   int lconv = 1;
   ccheck(thetas,thtmp,nter,tol,lrel,lconv);
   if((lconv == 0) and (nitr <= maxitr)) goto line1;
   
   return;

}
