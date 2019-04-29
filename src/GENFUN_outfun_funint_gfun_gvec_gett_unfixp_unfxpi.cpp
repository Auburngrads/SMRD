#include <base/base.hpp>
#include <utility/dexpc.hpp>

//' untransform unrestricted variable to the
//' (possibly) restricted space using kodet

double unfxpi(double thetat,
              int kode){
  
double big = 69.0e00,thold,dexpt;
int kodep = kode + 1;

if(kodep == 1){ return thetat; }

if(kodep == 2){ return thetat; }

if(kodep == 3){
  
   thold = thetat;
   if(thold > big)  goto line90;
   if(thold < -big) goto line80;
   
   return dexpc(thold);
  
}

if(kodep == 4){
  
   thold = thetat;
   if(thold > big)  goto line70;
   if(thold < -big) goto line80;
   dexpt = dexpc(thold);
   
   return dexpt / (one + dexpt);
  
}

if(kodep == 5){ return thetat; }

if(kodep == 6){ return thetat; }

line70: return 0.999999e00;
line80: return 1.0e-15;
line90: return 1.0e15;

}