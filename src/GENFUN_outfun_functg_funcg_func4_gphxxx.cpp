#include <base/base.hpp>
#include <genfun/powerc.hpp>
#include <genfun/gcdfm.hpp>

// CDF of the general proportional hazard function

double gphcdf(double t,
              Rcpp::NumericVector gamme,
              double power,
              int kdist){
  
  return one - powerc(gcdfm(t,gamme,kdist),power,101);
  
}

#include <base/base.hpp>
#include <genfun/powerc.hpp>
#include <genfun/gcdfm.hpp>

// Complementary CDF of the general proportional hazard function

double gphcdm(double t,
              Rcpp::NumericVector gamme,
              double power,
              int kdist){
  
  return powerc(gcdfm(t,gamme,kdist),power,102);
  
}

#include <base/base.hpp>
#include <spmlgeng/gcdfml.hpp>

// Log of the complementary CDF of the general proportional hazard function

double gphcml(double t,
              Rcpp::NumericVector gamme,
              double power,
              int kdist){
  
  return power * gcdfml(t,gamme,kdist);
  
}

#include <base/base.hpp>
#include <sgquan/gquant.hpp>
#include <genfun/powerc.hpp>

// General proportional hazard quantile function

double gphqu(double p,
             Rcpp::NumericVector gamme,
             double power,
             int kdist){
  
   double p0 = one - powerc((one - p),(one / power),103);
  
   return gquant(p0,gamme,kdist);
  
}

#include <base/base.hpp>
#include <genfun/gmquan.hpp>
#include <genfun/powerc.hpp>

// Return the location parameter from general proportional hazard quantile function

double gmphqu(double p,
              Rcpp::NumericVector gamme,
              double power,
              int kdist){
  
  double p0 = one - powerc((one - p),(one / power),104);
  
  return gmquan(p0,gamme,kdist);
  
}

#include <base/base.hpp>
#include <sgpdfl/gpdf.hpp>
#include <genfun/gcdfm.hpp>

// General proportional hazard model hazard function

double gphhaz(double t,
              Rcpp::NumericVector gamme,
              double power,
              int kdist){
  
  double xt = one,rpdf,rcdfm,gph_haz;
  int llog;
  
  // Factor for log dist
     llog = 0;
     if((kdist % 2) == 0) llog = 1;
     if(llog == 1) xt = std::exp(t);
     
     rpdf = gpdf(t,gamme,kdist) / xt;
     rcdfm = gcdfm(t,gamme,kdist);
     gph_haz = zero;
     
     if((rpdf <= zero) or (power <= zero)) return gph_haz;
     
     gph_haz = 1.0e15;
     
     if(rcdfm <= zero) return gph_haz;
     
     return power * rpdf / rcdfm;
  
}

#include <base/base.hpp>
#include <sgpdfl/gpdf.hpp>
#include <genfun/gcdfm.hpp>
#include <genfun/powerc.hpp>

// PDF of the general proportional hazard model

double gphpdf(double t,
              Rcpp::NumericVector gamme,
              double power,
              int kdist){
  
  return power * gpdf(t,gamme,kdist) * powerc(gcdfm(t,gamme,kdist),(power - one),105);
  
}

#include <base/base.hpp>
#include <sgpdfl/gpdfl.hpp>
#include <spmlgeng/gcdfml.hpp>

// Log of tht PDF of the general proportional hazard model

double gphpfl(double t,
              Rcpp::NumericVector gamme,
              double power,
              int kdist){
  
  return std::log(power) + gpdfl(t,gamme,kdist) + (gcdfml(t,gamme,kdist)) * (power - one);
  
}
