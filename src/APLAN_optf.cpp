#include <base/base.hpp>

bool close(double x1,
           double x2){
  
    double max12 = std::max(std::abs(x1),std::abs(x2));
    bool test = ((std::abs(x1 - x2) / std::max(max12,1.0e-5)) < 1.0e-7);
    
    return test;
  
}


double optf(double x,
            double xliml,
            double xlimu){
  
return std::log((x - xliml) / (xlimu - x));
  
}

double optfi(double x,
             double xliml,
             double xlimu){
  
double expx = std::exp(x), opt_fi;
  
opt_fi = (xlimu * expx + xliml) / (one + expx);

if(close(xlimu,opt_fi)) opt_fi = xlimu;
if(close(xliml,opt_fi)) opt_fi = xliml;

return opt_fi;

}
