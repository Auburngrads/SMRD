#include <base/base.hpp>
#include <utility/dcheck.hpp>

//' Translate individual parameters to an unrestricted space
//' @details Updated 28 jan 1987 fixed #3 dcheck;

double fxpi(double thetsi,
            int kode,
            int &ier){

int kodep = kode + 1;
double out = thetsi;

if(kodep == 3) {
  
   dcheck(thetsi,1.0e-25,1.0e25,1.0e-10,1.0e25,ier,-1111);
   out = std::log(thetsi);
   
}

if(kodep == 4) {
  
   dcheck(thetsi,1.0e-25,0.999999999,1.0e-10,0.999999999,ier,-1112);
   out = std::log(thetsi / (one - thetsi));
   
}

   return out;

}
