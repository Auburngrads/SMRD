#include <base/base.hpp>
#include <utility/wqm_quant.hpp>
#include <utility/dsqrtc.hpp>
#include <utility/dexpc.hpp>

//' Compute a confidence interval for a parameter with range kodet

void intgen(double &t,
            double &v,
            int &kodet,
            double &conlev,
            double &s,
            double &xl,
            double &xu){
  
double taila,zval,xlen;
int kodep;

taila = (1.0e00 + conlev) / 2.0e00;
zval = wqm_quant(taila,3);
kodep = kodet + 1;
s = dsqrtc(v);

if(debug::kprint >= 5){
  
   Rcpp::Rcout << "\nINTGEN**5**\n" << std::endl;
   Rcpp::Rcout << "kodet = " << kodet << std::endl;
   Rcpp::Rcout << "t = " << t << std::endl;
   Rcpp::Rcout << "v = " << v << std::endl;
   Rcpp::Rcout << "s = " << s << std::endl;
   Rcpp::Rcout << "zval = " << zval << std::endl;
  
}

if(kodep == 1){
  
   s = 0;
   xl = t;
   xu = t;
   return;
   
}

if(kodep == 2){
  
   xlen = s * zval;
   xl = t - xlen;
   xu = t + xlen;
   return;
  
}

if(kodep == 3){
  
   if(t == zero) goto line991;
   xlen = dexpc(zval * s / t);
   xl = t / xlen;
   xu = t * xlen;
   return;
  
}

if(kodep == 4) {
  
   if(t >= one) goto line992;
   if(t <= zero) goto line991;
   xu = t / (t + (one - t) * dexpc(-zval * s / (t * (one - t))));
   xl = t / (t + (one - t) * dexpc(zval * s / (t * (one - t))));
   return;
  
}

if(kodep == 5){
  
   xlen = s * zval;
   xl = t - xlen;
   xu = t + xlen;
   return;
  
}

if(kodep == 6){
  
   xlen = s * zval;
   xl = t - xlen;
   xu = t + xlen;
   return;
  
}

line991: xu = zero;
         xl = zero;
         s = zero;
         return;
         
line992: xu = one;
         xl = one;
         s = zero;
         return;
      
}