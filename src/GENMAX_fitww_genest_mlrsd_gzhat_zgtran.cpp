#include <base/base.hpp>

using namespace genx04;

//' Transformation of the standardized values to avoid overflow
//' start trans at xk, with an asym limit beyond 2*xk

double zgtran(double z,
              int kdist){
  
double zg_tran,zmxkm,zmxkp;
Rcpp::NumericVector xkp,xkm,txkp,txkm;
  
xkp = Rcpp::NumericVector::create(2.15e00,2.15e00,
                                   4.0e00, 4.0e00,
                                   9.5e00, 9.5e00);

xkm = Rcpp::NumericVector::create(-9.5e00,-9.5e00,
                                  -4.0e00,-4.0e00,
                                  -9.5e00,-9.5e00);
                                  
txkp = Rcpp::NumericVector::create( 4.2e00, 4.2e00,
                                    7.9e00, 7.9e00,
                                   18.9e00,18.9e00);

txkm = Rcpp::NumericVector::create(-18.9e00,-18.9e00,
                                    -7.9e00, -7.9e00,
                                   -18.9e00,-18.9e00);

if(genx04::g_ltp == 0)    goto line1001;

if(z > xkp.at(kdist - 1)) goto line1002;
if(z > xkm.at(kdist - 1)) goto line1001;

// Too small
   zmxkm = z - xkm.at(kdist - 1);
   zg_tran = xkm.at(kdist - 1) + zmxkm / (one + zmxkm / xkm.at(kdist - 1));
   goto line1005;
   
// Too big
   line1002: zmxkp = z - xkp.at(kdist - 1);
             zg_tran = xkp.at(kdist - 1) + zmxkp / (one + zmxkp / xkp.at(kdist - 1));
             goto line1005;
             
   line1001: zg_tran = z;
             if(z > txkp.at(kdist - 1)) zg_tran = txkp.at(kdist - 1);
             if(z < txkm.at(kdist - 1)) zg_tran = txkm.at(kdist - 1);
             
   line1005:  return zg_tran;
   
}