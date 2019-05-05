#include <base/base.hpp>
#include <utility/wqm_quant.hpp>
#include <utility/dlogc.hpp>
#include <sgquan/qugamm.hpp>


//' Function to return usual location parmaeter when
//' a percentile is sent down in gamme(1)
//' usually returns b0=yo-(b1x1+b2x2+...)-up(x)*sigma(x)
//' since gamme=yp-(b1x1+b2x2+...)  is sent down from tb0p

double gmquan(double p,
              Rcpp::NumericVector gamme,
              int kdist){
   
double gm_quan = 0.0e00;
   
// Location-scale distribution
   if((kdist > 0) and (kdist < 7)) {
     
       gm_quan = gamme.at(0) - wqm_quant(p,kdist) * gamme.at(1);
     
   }

// Exponential distribution
   if((kdist == 7) or (kdist == 8)) {
     
       gm_quan = gamme.at(0) - wqm_quant(p,1);
     
   }

// Perhaps we should modify below to avoid this needed log
// Genealized Gamma distribution
   if((kdist == 9) or (kdist == 10)) {
     
       if(gamme.at(3) < 0) {
         
          gm_quan = gamme.at(0) + dlogc(qugamm(one - p,gamme.at(4)) / gamme.at(4)) * (gamme.at(1) * gamme.at(5));
         
       }
       
       if(gamme.at(3) == 0) {
         
          gm_quan = gamme.at(0) - wqm_quant(p,3) * gamme.at(1);
         
       }
       
       if(gamme.at(3) > 0){
         
          gm_quan = gamme.at(0) - dlogc(qugamm(p,gamme.at(4)) / gamme.at(4)) * (gamme.at(3) * gamme.at(5));
         
       }
     
   }

// Regular Gamma distribution
   if((kdist == 11) or (kdist == 12)) {
     
       if(gamme.at(1) < 0){
         
          gm_quan = gamme.at(0) + dlogc(qugamm(one - p,gamme.at(2)) / gamme.at(2)) * (gamme.at(3));
         
       }
       
       if(gamme.at(1) == 0){
         
          gm_quan = gamme.at(0) - wqm_quant(p,3);
         
       }
       
       if(gamme.at(1) > 0){
         
          gm_quan = gamme.at(0) - dlogc(qugamm(p,gamme.at(2)) / gamme.at(2)) * (gamme.at(3));
         
       }
     
   }

return gm_quan;
   
}