#include <base/base.hpp>
#include <sgquan/gquant.hpp>

//' R interface for GENG cdf
//' 
//' @name sgquan
// [[Rcpp::export]]
Rcpp::NumericVector sgquan(Rcpp::NumericVector pvec,
                           Rcpp::NumericMatrix gamme,
                           int maxlen,
                           Rcpp::NumericVector answer){

for(int i = 0; i < maxlen; i++){

    answer.at(i) = gquant(pvec.at(i),gamme.column(i),9);

}

return answer;
  
}

#include <base/base.hpp>
#include <utility/wqm_quant.hpp>
#include <sgquan/qugamm.hpp>
#include <utility/dlogc.hpp>

double gquant(double p,
              Rcpp::NumericVector gamme,
              int kdist){

double res = 6.5;

//  #check for user specified cdf

if((kdist > 0) and (kdist < 7)) {
  
    return gamme.at(0) + wqm_quant(p,kdist) * gamme.at(1);
  
}


if((kdist == 7) or (kdist == 8)) { // exponential
 
    return gamme.at(0) + wqm_quant(p,1);
  
}

if((kdist == 9) or (kdist == 10)) { // generalized gamma
  
  if(gamme.at(3) < 0){
    
     return gamme.at(0) - dlogc(qugamm(one - p,gamme.at(4)) / gamme.at(4)) * (gamme.at(1) * gamme.at(5));
    
  }
  
  if(gamme.at(3) > 0){
    
     return gamme.at(0) + dlogc(qugamm(p,      gamme.at(4)) / gamme.at(4)) * (gamme.at(1) * gamme.at(5));
    
  }
    
     return gamme.at(0) + wqm_quant(p,3) * gamme.at(1);
  
}

if((kdist == 11) or (kdist == 12)) { // regular gamma
  
  if(gamme.at(1) < 0){
    
    return gamme.at(0) - dlogc(qugamm(one - p,gamme.at(2)) / gamme.at(2)) * (gamme.at(3));
    
  }
  
  if(gamme.at(1) > 0){
    
    return gamme.at(0) + dlogc(qugamm(p,      gamme.at(2)) / gamme.at(2)) * (gamme.at(3));
    
  }
  
  return gamme.at(0) + wqm_quant(p,3);
  
}

return res;
      
}

#include <base/base.hpp>
#include <utility/dsign.hpp>
#include <utility/dsqrtc.hpp>
#include <utility/wqm_quant.hpp>
#include <spgeng/gaminc.hpp>
#include <postkp/dlgama.hpp>
#include <utility/dexpc.hpp>

//' Quantile of gamma distribution with shape at probability p

double qugamm(double p,
              double shape){
  
Rcpp::NumericVector a;
double xnine = 9.0e00;
// tol1 is the requested relative accuracy in the quantile
// tol2 is the requested absolute accuracy in the probability
double tol1 = 1.0e-14,tol2 = 1.0e-24;
int maxit = 100;
a = NumericVector::create( 1.264616e-2, -1.425296e-2,
                           1.400483e-2, -5.886090e-3,
                          -1.091214e-2, -2.304527e-2,
                           3.135411e-3, -2.728484e-4, 
                          -9.699681e-3,  1.316872e-2,
                           2.618914e-2, -0.2222222e00,
                           5.406674e-5,  3.483789e-5,
                          -7.274761e-4,  3.292181e-3,
                          -8.729713e-3, 0.4714045e00, 1.0e00);
                          
//  plast was undefined before
double small  = 1.0e-300;
double small2 = small / tol1;
double plast  = zero;
double pr = p, f1, f2, dqgm, p1, pdif, del1, dlast, pabs;
int i = 1;

if(shape <= zero) shape = 1.0e-4;

//  #exponential special case
if(shape ==  one) {
  
  return -1 * std::log(one - pr);
  
}

// setup error message
// if((p.ge.zero).and.(p.lt.one))go to 2

if(fabs(p - zero) < 0.00001) {
  
   // if p=0, return qugamm=0
   return zero;
  
}

if(shape < half) {
  
// small shape, infinite density at 0
// try simplaster wilson-hilferty approx but revert to a small
// number of the first approx is negative

   f1   = one / (xnine * shape);
   dqgm = one - f1 + wqm_quant(pr,3) * dsqrtc(f1);
   dqgm = dqgm * dqgm * dqgm * shape * two;
   
   if(dqgm <= zero) dqgm = 0.0001e00;
       
 } else {

   // high order wilson-hilferty approx
   // use to find a good start value for 
   // the nr iterations
   f1 = half / shape;
   f2 = dsqrtc(f1) * wqm_quant(pr,3);
   
   dqgm = (((a.at(0) + a.at(1) * f2) * f1 + (((a.at(2) + a.at(3) * f2)\
             * f2 + a.at(4)) * f2 + a.at(5))) * f1 +\
               (((((a.at(6) + a.at(7) * f2) * f2 + a.at(8)) * \
               f2 + a.at(9)) * f2 + a.at(10)) * f2 + a.at(11)))\
             * f1 + (((((a.at(12) * f2 + a.at(13)) * f2 + a.at(14))\
             * f2 + a.at(15)) * f2 + a.at(16)) * f2 * f2 + a.at(17))\
             * f2 + a.at(18);
             
   dqgm = dqgm * dqgm * dqgm * shape;

}

// begin nr iterations

while(i <= maxit){
  
      p1   = gaminc(dqgm,shape);
      pdif = pr - p1;
      pabs = std::fabs(pdif);
             
      // check for accuracy at current estimate (abs accuracy in p)
      if(pabs <= tol2) return dqgm;
      
      // if i=1 or if we saw an improvement, go to get next delta
      if(!((pabs < plast) or (i == 1))) {
      
      // otherwise take a half step and try again
      // fcheck (but was not sure) thinks that del1 has not been defined above???
      // perhaps it just got fooled
      
            del1 = -del1 / two;
          
        } else {
        
          del1 = std::log(pabs) - (shape - one) * std::log(dqgm) + dqgm + dlgama(shape);
          del1 = dsign(dexpc(del1), pdif);
            
          // make sure that we do not cross 0
          if(-del1 > dqgm) del1 = -0.9e00 * dqgm;
          
          //  #switch to a half step if we appear to have gone in the wrong direction
          if(dsign(one,pdif) != dsign(one,del1)) {
            
             del1 = dsign(del1 / two,pdif);
          
          }
      }
      // F77 found this 08/28/94 -> del1 = dsign(dl/two,pdif)
      // save old values
      dlast = std::fabs(del1);
      plast = pabs;
      
      // after passing all tests, we can update
      dqgm = std::max(dqgm + del1,small);
      
      // check relative accuracy of dqgm if it is not too close to 0
      if(dqgm >= small2){
        
         if(dlast <= dqgm * tol1) return dqgm;
         
      }
      
      i = i + 1;

}

return dqgm;

}