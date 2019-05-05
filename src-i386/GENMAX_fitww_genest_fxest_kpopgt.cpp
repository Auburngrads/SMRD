#include <base/base.hpp>
#include <utility/icheck.hpp>

//' Pick up the population that we use for quantile offset
//' @details Each integer vector includes six elements according to the following
//'      single  lfp   doa   sts   phaz
//' kpgx   0,     1,    0,    0,    0,    0
//' kpcl   0,     0,    0,    0,    0,    0
//' kpcu   0,     1,    1,    0,    0,    0

int kpopgt(int kmod){

int ier = 0;

// Define the estimation quantile population
   Rcpp::IntegerVector kpgx = Rcpp::IntegerVector::create(0,1,0,0,0,0);  
  
// Define the lower limit on a population
   Rcpp::IntegerVector kpcl = Rcpp::IntegerVector::create(0,0,0,0,0,0);  
   
// Define the upper limit on a population
   Rcpp::IntegerVector kpcu = Rcpp::IntegerVector::create(0,1,1,0,0,0);  

   icheck(kmod,0,4,0,0,ier,-50986);
   int kmodp = kmod + 1;

return kpgx.at(kmodp - 1);

}