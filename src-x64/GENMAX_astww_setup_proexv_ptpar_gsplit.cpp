#include <base/base.hpp>

//' Splits a 3 digit number ixyz into 3 separate digits

void gsplit(int &ixyz,
            int &ix,
            int &iy,
            int &iz){
  
  ix = std::floor((ixyz - 0) / 100);
  iy = std::floor((ixyz - 100 * ix) / 10);
  iz = std::floor((ixyz - 100 * ix - 10 * iy) / 1);

  return;

}
