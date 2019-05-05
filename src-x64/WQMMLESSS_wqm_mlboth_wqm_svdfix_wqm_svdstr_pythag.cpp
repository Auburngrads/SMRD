#include <base/base.hpp>

//' finds dsqrt(a**2+b**2) without overflow or destructive underflow

double pythag(double a,
              double b){

double p,r,s,t,u;
  
p = std::max(std::fabs(a),std::fabs(b));

if(p != 0.0e0) {
  
   r = std::pow((std::min(std::fabs(a),std::fabs(b)) / p), 2);
  
   line10: t = 4.0e00 + r;
   
   if(t == 4.0e00) goto line20;
   
   s = r / t;
   u = 1.0e00 + 2.0e00 * s;
   p = u * p;
   r = std::pow((s / u), 2) * r;
   
   goto line10;
   
}

line20: return p;

}

