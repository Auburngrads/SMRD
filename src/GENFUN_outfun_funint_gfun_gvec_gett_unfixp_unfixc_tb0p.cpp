#include <base/base.hpp>
#include <genfun/funcg.hpp>
#include <genmax/ptgame.hpp>
#include <genmax/rgamme.hpp>

using namespace genx03;
using namespace genx07;
using namespace genx08;

//' pick up the value of the standardized intercept value
//' when there is a percentile value in theta(1)

Rcpp::List tb0p(Rcpp::List args){
  
double thold,val;
int kfnow,llogn,nparm;
Rcpp::NumericVector thetas;

thetas = Rcpp::as<NumericVector>(Rcpp::as<List>(args)["lt"]);
nparm  = Rcpp::as<int>(Rcpp::as<List>(args)["ln"]); 
  
// Grab space for gamma vector
   Rcpp::NumericVector ipgame = Rcpp::NumericVector(genx03::g_ngame);
  
// Save the quantile and set theta(1)=0 to get b1x1+b2x2+... in gamma(1)
   thold = thetas.at(0);
   thetas.at(0) = zero;
   ptgame(thetas);
   rgamme(genx08::g_kpoint,thetas,ipgame);
   
// Restore thetas(1) and compute yp-(b1x1+b2x2+...)
   thetas.at(0) = thold;
   
   for(int i = 1; i <= genx03::g_ngame; i++){
     
       ipgame.at(i - 1) = thold - ipgame.at(i - 1);
     
   }
      
// yp=b0+b1x1+b2x2+...+up(x)*sigma(x)
   if(genx08::g_pest <= zero) return ipgame.at(0);

// cxx#  if(lupest.eq.0)go to 50
// cxx#  scale=one
// cxx#  if(kdist.ne.7.and.kdist.ne.8)scale=ds(ipgame+1)
// cxx#  tb0p=ds(ipgame)-upest*scale
// cxx#  go to 99

// Get flat function indicator from population and quantile combination
   kfnow = -1 * (genx07::g_kpopu * 3 + 2);
   llogn = 0;
   
// #b0=(yp(xbar)-b1x1+b2x2+...)-up(xbar)*sigma(xbar)
    val = funcg(ipgame,
                genx03::g_ngame,
                genx07::g_kmod,
                genx07::g_kdist,
                kfnow,
                llogn,
                genx08::g_pest);
    
    return Rcpp::List::create(Named("val") = val);
        
}

#include <base/base.hpp>

//' cxx#dummy for failure prob constraint
//' cxx# 28 aug 94 adding following statement to avoid warning from compiler

Rcpp::List tb0f(Rcpp::List args){

// double thetas;
// int nparm;
//      
// thetas = Rcpp::as<NumericVector>(Rcpp::as<List>(args)["lt"]);
// nparm  = Rcpp::as<int>(Rcpp::as<List>(args)["ln"]); 

  return Rcpp::List::create(Named("val") = 0.0e00);
  
}
