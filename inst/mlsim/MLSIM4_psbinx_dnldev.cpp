#include <base/base.hpp>
#include <mlsim2/bnldev.hpp>

//' Double precision version of bnldev
double dnldev(double rho,
              int num){
  
double rhos = rho;
double dnl_dev = bnldev(rhos,num);

return dnl_dev;

}

#include <base/base.hpp>
#include <mlsim2/algama.hpp>

#define _USE_MATH_DEFINES
 
#include <cmath>

//' Modified from numerical recipes book
double bnldev(double pp,
              double n){
  
double bnl_dev, pold = -1.0e00,p,pc,am,g,t;
double y,sq,em,plog,pclog,oldg;
int nold = -1,npp1,en,j = 1;

if(pp <= 0.5){ p = pp; } else { p = 1.0e00 - pp; }

am = n * p;

if(n < 25){
  
   bnl_dev = 0.0e00;

   for(j = 1; j <= n; j++){
     
       if(R::unif_rand() < p) bnl_dev = bnl_dev + 1.0e00;
     
   }
        
} else if(am < 1) {
  
          g = std::exp(-1 * am);
          t = 1.0e00;
          npp1 = n + 1;
      
          for(j = 1; j <= npp1; j++){
            
              t = t * R::unif_rand();
              if (t < g) goto line1;
            
          }
        
          j = n;
          line1: bnl_dev = j;

} else {
  
  if(n != nold){
          
     en = n;
     oldg = algama(en + 1);
     // xxx nold = n;
           
  }
        
  if(p != pold){
          
     pc = 1 - p;
     plog = std::log(p);
     pclog = std::log(pc);
     // xxx pold = p;
          
  }
        
  sq = std::sqrt(2 * am * pc);
        
  line2: y = std::atan(M_PI * R::unif_rand());
         em = sq * y + am;
         if((em < 0) or (em <= en + 1)) goto line2;
         em = (int)em;
         t = 1.2 * sq * (1 + std::pow(y,2)) * std::exp(oldg - algama(em + 1) - algama(en - em + 1) + em * plog + (en - em) * pclog);
         if(R::unif_rand() > t) goto line2;
         bnl_dev = em;
       
}
      if (p != pp) bnl_dev = n - bnl_dev;
        
return bnl_dev;
      
}

#include <base/base.hpp>
#include <postkp/dlgama.hpp>

//' Single precision log gamma function
double algama(double x){
  
double dx = x;
double al_gama = dlgama(dx);

return al_gama;

}