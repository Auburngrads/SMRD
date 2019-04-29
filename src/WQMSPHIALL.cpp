#include <base/base.hpp>
#include <wqmsphiall/wqm_phiall.hpp>

using namespace lstd;

// [[Rcpp::export]]
Rcpp::List wqmsphiall(Rcpp::NumericVector &phib,
                      Rcpp::NumericVector &phibm,
                      Rcpp::NumericVector &phis,
                      Rcpp::NumericVector &phip,
                      int &n,
                      Rcpp::NumericVector &z, 
                      int &idist) {
  
  for(int i = 0; i < n; i++){
    
      wqm_phiall(phib.at(i),
                 phibm.at(i),
                 phis.at(i),
                 phip.at(i),
                 z.at(i),
                 idist);
    
  }
  
  return Rcpp::List::create(Named("phib") = phib,
                            Named("phibm") = phibm,
                            Named("phis") = phis,
                            Named("phip") = phip,
                            Named("n") = n,
                            Named("z") = z,
                            Named("idist") = idist);
}

#include <base/base.hpp>
#include <wqmsphiall/wqm_ztran.hpp>
#include <utility/wqm_dxerc.hpp>

using namespace lstd;

//' Compute the cdf, pdf, and the derivative of the pdf
//' @name wqm_phiall

void wqm_phiall(double &phib,
                double &phibm,
                double &phis,
                double &phip,
                double &z,
                int &kdist){

double dexpz, dexpzx, dexpmz, dxm;  

//  check and correct for extreme values of z
z  =  wqm_ztran(z, kdist);

if((kdist == 1) or (kdist == 2)) { // sev
  
      dexpz = std::exp(z);
      dexpzx = std::exp(-dexpz);
      phibm = dexpzx;
      phis = dexpzx * dexpz;
      phib = one - dexpzx;
      phip = (one - dexpz) * phis;
  
}
if((kdist == 3) or (kdist == 4)) { //  normal
  
      phis = R::dnorm(z, 0, 1, false);
      phib = R::pnorm(z, 0, 1, true, false);
      phibm = half * wqm_dxerc(z * root);
      phip = -z * phis;

}
if((kdist == 5) or (kdist == 6)) { // logisitc

      dexpmz = std::exp(-z);
      dxm = one + dexpmz;
      phib = one / dxm;
      phibm = dexpmz / dxm;
      phis = phib * phibm;
      phip = phis * (one - two * phib);
  
}
if((kdist == 7) or (kdist == 8)) { //  lev

      dexpmz = std::exp(-z);
      phib = std::exp(-dexpmz);
      phibm = one - phib;
      phis = std::exp(-z - dexpmz);
      phip = dexpmz * (phis - phib);
}

return;

}

#include <base/base.hpp>

using namespace lstd;

//' Transform standardized observations to avoid 
//' overflow and underflow problems. Follows the 
//' approach of Nelson (1982), page 394.
//'
//' Modified 5 March 2004 to allow sev and to use 
//' better code structure.

double wqm_ztran(double z, int kdist){

// constants tuned to ieee floating point 64 bit 
// double precision
  
Rcpp::NumericVector xkp, xkm, txkp, txkm;
double wqmztran, zmxkp, rangep, rangem, zmxkm;
  
xkp  = NumericVector::create(2.60e00,2.60e00,7.0e00,7.0e00,
                             33.6e00,33.6e00,33.4e00,33.4e00);

xkm  = NumericVector::create(-33.4e00,-33.4e00,-7.00e00,-7.0e00,
                             -33.6e00,-33.6e00,-2.60e00,-2.60e00);

txkp = NumericVector::create(3.60e00,3.60e00,8.00e00,8.00e00,
                             36.6e00,36.6e00,37.4e00,37.4e00);

txkm = NumericVector::create(-37.4e00,-37.4e00,-8.00e00,-8.00e00,
                             -36.6e00,-36.6e00,-3.60e00,-3.60e00);

// transformation of the standardized values to 
// avoid under/overflow
// if ltp is False, the just jump to the asymptote

if(lstd::g_ltp) {

// too small z < xkm(kdist) ---  asymptote to txkm(kdist)

if(z < xkm.at(kdist - 1)){
  
   zmxkm = z - xkm.at(kdist - 1);
   rangem = txkm.at(kdist - 1) - xkm.at(kdist - 1);
   wqmztran = xkm.at(kdist - 1) + zmxkm / (one + zmxkm / rangem);

// too big--- z > xkp(kdist) --- asymptote to txkp(kdist)

} else {
  
  if(z > xkp.at(kdist - 1)) {
    
     zmxkp = z - xkp.at(kdist - 1);
     rangep = txkp.at(kdist - 1) - xkp.at(kdist - 1);
     wqmztran = xkp.at(kdist - 1) + zmxkp / (one + zmxkp / rangep);
     
   } else {
    
     wqmztran = z;
    
   }
   
}
           
 } else {

   wqmztran = z;
   if(z > txkp.at(kdist - 1)) wqmztran = txkp.at(kdist - 1);
   if(z < txkm.at(kdist - 1)) wqmztran = txkm.at(kdist - 1);

 }
 
  return wqmztran;
 
}