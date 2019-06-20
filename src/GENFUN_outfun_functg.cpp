#include <base/base.hpp>
#include <genfun/funcg.hpp>
#include <genmax/ptgame.hpp>
#include <genmax/rgamme.hpp>

using namespace genx03;
using namespace genx07;
using namespace genx08;
using namespace genx14;

//' wrapping subroutine to pick up common information and gamma and
//' to compute the requested function so that it looks like a
//' function of only thetas

Rcpp::List functg(Rcpp::List args){

Rcpp::NumericVector thetas;

thetas = Rcpp::as<NumericVector>(Rcpp::as<List>(args)["lt"]);

if(debug::kprint >= 7){
   
   Rcpp::Rcout << "\nFUNCTG thetas check\n" << std::endl;
   Rcpp::Rcout << "thetas = " << thetas << std::endl;
   
}
   
double fun_ctg;
  
// Grab space for the gamma vector
   Rcpp::NumericVector ipgame = Rcpp::NumericVector(genx03::g_ngame);
  
// Set save stuff in gamms since we are not within a loop
   ptgame(thetas);
   
// Recover the gamma vector
   rgamme(genx08::g_kpoint,thetas,ipgame);

// Go to compute the function
   fun_ctg = funcg(ipgame,
                   genx03::g_ngame,
                   genx07::g_kmod,
                   genx07::g_kdist,
                   genx14::g_kfuncf,
                   genx07::g_llog,
                   genx14::g_funarg);
   
return Rcpp::List::create(Named("val") = fun_ctg);
  
}