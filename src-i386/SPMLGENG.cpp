#include <base/base.hpp>
#include <spmlgeng/gcdfml.hpp>

//' R interface for gng log(1-cdf)
// [[Rcpp::export]]
Rcpp::List SPMLGENG(Rcpp::NumericVector tvec,
                    Rcpp::NumericMatrix gamme,
                    int maxlen,
                    Rcpp::NumericVector answer){
  
for(int i = 0; i < maxlen; i++){
   
     answer.at(i) = gcdfml(tvec.at(i),gamme.column(i),9);
   
}

return Rcpp::List::create(Named("answer") = answer);
  
}



#include <base/base.hpp>
#include <genmax/zgtran.hpp>
#include <spmlgeng/pblgg.hpp>
#include <spgeng/gcdf.hpp>
// Compute log cdf
double gcdfl(double y,
             Rcpp::NumericVector gamme,
             int kdist){
  
double g_cdfl = 0.0e00, z;
  
if(kdist > 100) g_cdfl = std::log(gcdf(y,gamme,kdist)); 
  
if(kdist == 1)  g_cdfl = std::log(gcdf(y,gamme,kdist));

if(kdist == 2)  g_cdfl = std::log(gcdf(y,gamme,kdist));

if(kdist == 3)  g_cdfl = std::log(gcdf(y,gamme,kdist));

if(kdist == 4)  g_cdfl = std::log(gcdf(y,gamme,kdist));

if(kdist == 5)  g_cdfl = std::log(gcdf(y,gamme,kdist));

if(kdist == 6)  g_cdfl = std::log(gcdf(y,gamme,kdist));

if(kdist == 7)  g_cdfl = std::log(gcdf(y,gamme,kdist));

if(kdist == 8)  g_cdfl = std::log(gcdf(y,gamme,kdist));

// Generalized gamma
// pblgg=f(z,q,sqrtxk,xk)
if((kdist == 9) or (kdist == 10)){
  
   z = zgtran((y - gamme.at(0)) / gamme.at(1),5);
   g_cdfl = pblgg(z,gamme.at(3),gamme.at(5),gamme.at(4));
   
}

// Ordinary gamma
// pblgg=f(z,q,sqrtxk,xk)
if((kdist == 11) or (kdist == 12)){
  
    z = zgtran(y - gamme.at(0),5);
    g_cdfl = pblgg(z,gamme.at(1),gamme.at(3),gamme.at(2));
   
}

return g_cdfl;

}



#include <base/base.hpp>
#include <spmlgeng/gcdfml.hpp>
#include <spgeng/zgtran.hpp>
#include <spgeng/usrcdf.hpp>
#include <utility/dexpc.hpp>
#include <utility/wqm_dxerc.hpp>
#include <spmlgeng/pbmlgg.hpp>
// Compute log(one-cdf)
double gcdfml(double y,
              Rcpp::NumericVector gamme,
              int kdist) {
  
double z = 0.0e00;
  
if(kdist > 100) return std::log(one - usrcdf(y, gamme, kdist));

  
if((kdist > 0) & (kdist < 7)) {
  
    z = zgtran((y - gamme.at(0)) / gamme.at(1), kdist);
  
    // SEV
    if((kdist == 1) or (kdist == 2)) {
      
      return -1 * dexpc(z);
      
    }
    
    // Normal
    if((kdist == 3) or (kdist == 4)) {
      
      return std::log(half * wqm_dxerc(z * root));
      
    }
    
    // Logistic
    if((kdist == 5) or (kdist == 6)) {
      
      return -z - std::log(one + dexpc(-z));
      
    }

    
}

// Exponential
if((kdist == 7) or (kdist == 8)) {
  
    z = zgtran(y - gamme.at(0),1);
    return -1 * dexpc(z);
  
}


// Generalized Gamma
if((kdist == 9) or (kdist == 10)) {
  
    z = zgtran((y - gamme.at(0)) / gamme.at(1),5);
    return pbmlgg(z,gamme.at(3),gamme.at(5),gamme.at(4));
  
}

// Ordinary Gamma
if((kdist == 11) or (kdist == 12)) {
  
    // #gcdfml=f(z,q,sqrtxk,xk);
    z = zgtran(y - gamme.at(0),5);
    return pbmlgg(z, gamme.at(1),gamme.at(3),gamme.at(2));

}

return 0.0e00;
  
}



#include <base/base.hpp>
#include <utility/dexpc.hpp>
#include <utility/wqm_dxerc.hpp>
#include <spgeng/gaminc.hpp>
//
// Log of 1-cdf for the generalized gamma with shape xk
//
double pbmlgg(double z,
              double qq,
              double sqrtxk,
              double xk){

double small = 1.0e-30;
    
if(qq < 0) {
  
  double zt = xk * dexpc(-z / sqrtxk);
  
  return std::log(std::max(gaminc(zt,xk),small));
  
}

if(qq > 0) {
  
  double zt = xk * dexpc(z / sqrtxk);
  return std::log(std::max(one - gaminc(zt,xk),small));
  
}

  return std::log(half * wqm_dxerc(-z * root));
  
}



#include <base/base.hpp>
#include <utility/dexpc.hpp>
#include <sbvn/derfc.hpp>
#include <spgeng/gaminc.hpp>
//
// log cdf(z) for the generalized gamma with shape xk
//
double pblgg(double z,
              double qq,
              double sqrtxk,
              double xk){

double small = 1.0e-30;
    
if(qq < 0) {
  
  double zt = xk * dexpc(-z / sqrtxk);
  
  return std::log(std::max(one - gaminc(zt,xk),small));
  
}

if(qq > 0) {
  
  double zt = xk * dexpc(z / sqrtxk);
  return std::log(std::max(gaminc(zt,xk),small));
  
}

  return std::log(half * derfc(-z * root));
  
}



#include <base/base.hpp>
#include <utility/dexpc.hpp>
#include <sbvn/derfc.hpp>
#include <spgeng/gaminc.hpp>
//
// 1-cdf(z) for the generalized gamma with shape xk
//
double pbmgg(double z,
             double qq,
             double sqrtxk,
             double xk){

if(qq < 0) {
  
  double zt = xk * dexpc(-z / sqrtxk);
  
  return gaminc(zt,xk);
  
}

if(qq > 0) {
  
  double zt = xk * dexpc(z / sqrtxk);
  return one - gaminc(zt,xk);
  
}

return half * derfc(z * root);
  
}