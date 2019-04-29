#include <base/base.hpp>

//' Get sd of a single transformed parameter by type

void ufxsdi(double &sigmat,
            double &thetsi,
            int &kode,
            double &sigma){
  
sigma = sigmat;
  
if((kode < 1) or (kode > 5)) return;

if(kode == 1) return;

if(kode == 2) {
  
   sigma = sigmat / thetsi;
   return;
  
}

if(kode == 3) {
  
   sigma = sigmat / (thetsi * (one - thetsi));
   return;
  
}

if(kode == 4) return;

if(kode == 5) return;

return;

}