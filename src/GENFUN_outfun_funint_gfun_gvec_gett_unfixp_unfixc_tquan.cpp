#include <base/base.hpp>
#include <genfun/funcg.hpp>
#include <genmax/ptgame.hpp>
#include <genmax/rgamme.hpp>

using namespace genx03;
using namespace genx07;
using namespace genx08;

//'   #return an estimate of a suitable quantile to take the place of the
//'   #usual location parameter.
//' xx  may want to make a simplier run for ordinary model and upest as in genmax2

Rcpp::List tquan(Rcpp::List args){
 
int kfnow,llogn,nparm;
double val;
Rcpp::NumericVector thetas;

thetas = Rcpp::as<NumericVector>(Rcpp::as<List>(args)["lt"]);
//nparm  = Rcpp::as<int>(Rcpp::as<List>(args)["ln"]); 
// cxx#we could use the upest short cut here but we would not save much
//       call gttqua(kmod,kdist,pest,kprint,kpoine,ngame,kpopu)
// cxx#    later we can use this for upest short cut
// cxx#      call gttqua(kmod,kdist,pest,kprint,kpoine,ngame,kpopu,upest,lupest)
  
// Grab space for gamma vector and compute from thetas and kpoine
   Rcpp::NumericVector ipgame = Rcpp::NumericVector(genx03::g_ngame);
   ptgame(thetas);
   rgamme(genx08::g_kpoint,thetas,ipgame);
   
   if(genx08::g_pest <= zero) return ipgame.at(0);
   
// cxx#  if(lupest.eq.0)go to 50
// cxx#  scale=one
// cxx#  if(kdist.ne.7.and.kdist.ne.8)scale=ds(ipgame+1)
// cxx#  tquan=ds(ipgame)+upest*scale
// cxx#  go to 99
// cxxx#perhpas see instead if kscloc has a meaning (e.g.>0)

// Get flat function indicator from population and quantile combination
   kfnow = 3 * genx07::g_kpopu + 2;
   llogn = 0;
   // #yp=xmu(x)+up(x)*sigma(x)
   
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

//' dummy for constraint on failure probability
//'  aug 94 adding following statement to avoid warning from compiler

Rcpp::List tfprob(Rcpp::List args){
   
// int nparm;
// double thetas;
// 
// thetas = Rcpp::as<NumericVector>(Rcpp::as<List>(args)["lt"]);
// nparm  = Rcpp::as<int>(Rcpp::as<List>(args)["ln"]); 

  return Rcpp::List::create(Named("val") = 0.0e00);
  
}