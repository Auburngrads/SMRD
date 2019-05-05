#include <base/base.hpp>

//' Compute the log likelihood for standard (no mixtures) model
//' with possibly truncated observation
//' the explanitory variable stuff was filtered out above

double flkg(double (*flkgx)(int,double,double,Rcpp::NumericVector,int),
            int kdist,
            double yl,
            double yu,
            int kccode,
            int ncolty,
            double tryl,
            double tryu,
            int ktcode,
            Rcpp::NumericVector gamme,
            int ngame){
   
double fl_kg = 0.0e00;
   
// Return 0.0 for dummy observation
   fl_kg = flkgx(kccode,yl,yu,gamme,kdist);
   
// We are done if there is no truncation
   if(ncolty == 0) return fl_kg;
   if(ktcode == 1) return fl_kg;
   
// Subtract out the truncation log probability for the observation
   fl_kg = fl_kg - flkgx(-ktcode,tryl,tryu,gamme,kdist);
   
   return fl_kg;
   
}

#include <base/base.hpp>
#include <sgpdfl/gpdfl.hpp>
#include <spmlgeng/gcdfml.hpp>
#include <spmlgeng/gcdfl.hpp>
#include <spgeng/gcdf.hpp>

//' Compute the log likelihood for a single observation

double flkg0(int kcodep,
             double yl,
             double yu,
             Rcpp::NumericVector gamme,
             int kdist){
   
double epslik = 1.0e-3,flkg_0 = 0.0e00,ygl,ygu;
int kccode;

kccode = std::abs(kcodep);

// if kcodep<0 here, we want to compute the conditioning
// probability for truncated data
   if(kcodep < 0){
      
      if(kccode == 1) flkg_0 = gpdfl(yl,gamme,kdist);
      if(kccode == 2) flkg_0 = gcdfl(yl,gamme,kdist);
      if(kccode == 3) flkg_0 = gcdfml(yl,gamme,kdist);
      if(kccode == 4) {
         
         ygl = yl;
         ygu = yu;
         
         flkg_0 = std::log(std::max(gcdf(ygu,gamme,kdist) - gcdf(ygl,gamme,kdist),1.0e-30));
         
      }
      
    } else {
      
      if(kccode == 1) flkg_0 = gpdfl(yl,gamme,kdist);
      if(kccode == 2) flkg_0 = gcdfml(yl,gamme,kdist);
      if(kccode == 3) flkg_0 = gcdfl(yl,gamme,kdist);
      if(kccode == 4) {
         
         ygl = yl;
         ygu = yu;
         
         flkg_0 = std::log(std::max(gcdf(ygu,gamme,kdist) - gcdf(ygl,gamme,kdist),1.0e-30));
         
      }
      
      if(kccode == 5) {
         
         ygl = yl - epslik;
         ygu = yl + epslik;
         
         flkg_0 = std::log(std::max(gcdf(ygu,gamme,kdist) - gcdf(ygl,gamme,kdist),1.0e-30));
         
      }
      
   }
   
return flkg_0;

}

#include <base/base.hpp>
#include <sgpdfl/gpdfl.hpp>
#include <spmlgeng/gcdfml.hpp>
#include <spmlgeng/gcdfl.hpp>
#include <spgeng/gcdf.hpp>
#include <utility/dlogc.hpp>

using namespace genx09;

//' Compute the log likelihood for a single observation

double flkg1(int kcodep,
             double yl,
             double yu,
             Rcpp::NumericVector gamme,
             int kdist){
   
double epslik = 1.0e-3,flkg_1 = 0.0e00,p,plog,ygl,ygu;
int kccode;

p = gamme.at(genx09::g_kprloc - 1);

// And get log p in next position
   plog = gamme.at(genx09::g_kprloc);
   kccode = std::abs(kcodep);
   
// if kcodep<0 here, we want to compute the conditioning
// probability for truncated data
   if(kcodep < 0){
      
      if(kccode == 1) goto line1;
      if(kccode == 2) goto line3;
      if(kccode == 3) goto line2;
      if(kccode == 4) goto line4;
      
   } else {
      
      if(kccode == 1) goto line1;
      if(kccode == 2) goto line2;
      if(kccode == 3) goto line3;
      if(kccode == 4) goto line4;
      if(kccode == 5) goto line5;
      
   }
   
line1: flkg_1 = gpdfl(yl,gamme,kdist) + plog;
       goto line199;
       
line2: flkg_1 = dlogc(one - p * gcdf(yl,gamme,kdist));
       goto line199;
       
line3: flkg_1 = gcdfl(yl,gamme,kdist) + plog;
       goto line199;
       
// Use a small interval for the 'true likelihood'
line5:  ygl = yl - epslik;
        ygu = yl + epslik;
        goto line45;
        
line4:  ygl = yl;
        ygu = yu;
        
line45: flkg_1 = std::log(std::max(gcdf(ygu,gamme,kdist) - gcdf(ygl,gamme,kdist),1.0e-30)) + plog;

line199: return flkg_1;
   
}

#include <base/base.hpp>
#include <sgpdfl/gpdfl.hpp>
#include <spmlgeng/gcdfml.hpp>
#include <spmlgeng/gcdfl.hpp>
#include <spgeng/gcdf.hpp>
#include <utility/dlogc.hpp>

using namespace genx09;

//' Compute the log likelihood for a single observation - doa model

double flkg2(int kcodep,
             double yl,
             double yu,
             Rcpp::NumericVector gamme,
             int kdist){
   
double epslik = 1.0e-3,flkg_2 = 0.0e00,p,plog,ygl,ygu;
int kccode;

// Pick up the location of the p parameter in gamme
   p = one - gamme.at(genx09::g_kprloc - 1);
   
// And get log p in next position
   plog = gamme.at(genx09::g_kprloc);
   kccode = std::abs(kcodep);
   
// If kcodep<0 here, we want to compute the conditioning
// probability for truncated data
   if(kcodep < 0){
      
      if(kccode == 1) goto line1;
      if(kccode == 2) goto line3;
      if(kccode == 3) goto line2;
      if(kccode == 4) goto line4;
      
    } else {
      
      if(kccode == 1) goto line1;
      if(kccode == 2) goto line2;
      if(kccode == 3) goto line3;
      if(kccode == 4) goto line4;
      if(kccode == 5) goto line5;
 
   }
    
line1: flkg_2 = gpdfl(yl,gamme,kdist) + plog;
       goto line199;
       
line2: flkg_2 = gcdfml(yl,gamme,kdist) + plog;
       goto line199;
       
line3: flkg_2 = dlogc(p + (one - p) * gcdf(yl,gamme,kdist));
       goto line199;
       
// Use a small interval for the 'true likelihood'
line5: ygl = yl - epslik;
       ygu = yl + epslik;
       goto line45;
       
line4: ygl = yl;
       ygu = yu;
       
line45: flkg_2 = std::log(std::max(gcdf(ygu,gamme,kdist) - gcdf(ygl,gamme,kdist), 1.0e-30)) + plog;

line199: return flkg_2;

}

#include <base/base.hpp>
#include <sgpdfl/gpdfl.hpp>
#include <sgpdfl/gpdf.hpp>
#include <spmlgeng/gcdfml.hpp>
#include <spmlgeng/gcdfl.hpp>
#include <spgeng/gcdf.hpp>
#include <genfun/gcdfm.hpp>

using namespace genx09;

//' Compute the log likelihood for a single observation - stst special competing risk model

double flkg3(int kcodep,
             double yl,
             double yu,
             Rcpp::NumericVector gamme,
             int kdist){
   
double epslik = 1.0e-3,flkg_3 = 0.0e00,ygl,ygu;
int kccode;

// Pick up the location of the sta parameter in gammp
   kccode = std::abs(kcodep);
   
// if kcodep<0 here, we want to compute the conditioning
// probability for truncated data
   if(kcodep < 0){
      
      if(kccode == 1) goto line1;
      if(kccode == 2) goto line3;
      if(kccode == 3) goto line2;
      if(kccode == 4) goto line4;
      
    } else {
      
      if(kccode == 1) goto line1;
      if(kccode == 2) goto line2;
      if(kccode == 3) goto line3;
      if(kccode == 4) goto line4;
      if(kccode == 5) goto line5;
 
   }
    
// Exact failure time
line1: flkg_3 = std::log((gpdf(yl,gamme,kdist) / (gcdfm(yl,gamme,kdist)) + gpdf(yl,gamme.at(genx09::g_kpwloc - 1),7) / (gcdfm(yl,gamme.at(genx09::g_kpwloc - 1),7))) * gcdfm(yl,gamme,kdist) * gcdfm(yl,gamme.at(genx09::g_kpwloc - 1),7));
       goto line199;
       
// Right censored observation
line2: flkg_3 = gcdfml(yl,gamme,kdist) + gcdfml(yl,gamme.at(genx09::g_kpwloc - 1),7);
       goto line199;
       
// Left censored observation
line3: flkg_3 = std::log(std::max(1.0e00 - gcdfm(yl,gamme,kdist) * gcdfm(yl,gamme.at(genx09::g_kpwloc - 1),7), 1.0e-30));
       goto line199;
       
// Use a small interval for the 'true likelihood'
line5: ygl = yl - epslik;
       ygu = yl + epslik;
       goto line45;
       
line4: ygl = yl;
       ygu = yu;
       
line45: flkg_3 = std::log(std::max(gcdfm(ygl,gamme,kdist) * gcdfm(ygl,gamme.at(genx09::g_kpwloc),7) - gcdfm(ygu,gamme,kdist) * gcdfm(ygu,gamme.at(genx09::g_kpwloc - 1),7),1.0e-30));

line199: return flkg_3;

}

#include <base/base.hpp>
#include <genfun/gphpfl.hpp>
#include <genfun/gphcml.hpp>
#include <genfun/gphcdf.hpp>

using namespace genx09;

//' Compute the log likelihood for a single observation - parametric proportional hazards model

double flkg4(int kcodep,
             double yl,
             double yu,
             Rcpp::NumericVector gamme,
             int kdist){
   
double epslik = 1.0e-3,flkg_4 = 0.0e00,ygl,ygu,power;
int kccode;

power = gamme.at(genx09::g_kpwloc - 1);
kccode = std::abs(kcodep);

// if kcodep<0 here, we want to compute the conditioning
// probability for truncated data
   if(kcodep < 0){
      
      if(kccode == 1) goto line1;
      if(kccode == 2) goto line3;
      if(kccode == 3) goto line2;
      if(kccode == 4) goto line4;
      
    } else {
      
      if(kccode == 1) goto line1;
      if(kccode == 2) goto line2;
      if(kccode == 3) goto line3;
      if(kccode == 4) goto line4;
      if(kccode == 5) goto line5;
 
   }
    
line1: flkg_4 = gphpfl(yl,gamme,power,kdist);
       goto line99;
       
line2: flkg_4 = gphcml(yl,gamme,power,kdist);
       goto line99;
       
line3: flkg_4 = std::log(gphcdf(yl,gamme,power,kdist));
       goto line99;
       
// Use a small interval for the 'true likelihood'
line5: ygl = yl - epslik;
       ygu = yl + epslik;
       goto line45;
       
line4: ygl = yl;
       ygu = yu;
       
line45: flkg_4 = std::log(std::max(gphcdf(ygu,gamme,power,kdist) - gphcdf(ygl,gamme,power,kdist), 1.0e-30));

line99: return flkg_4;

}

#include <base/base.hpp>
#include <sgpdfl/gpdfl.hpp>
#include <spmlgeng/gcdfml.hpp>
#include <spmlgeng/gcdfl.hpp>
#include <spgeng/gcdf.hpp>

//' Compute the log likelihood for a single observation

double flkg5(int kcodep,
             double yl,
             double yu,
             Rcpp::NumericVector gamme,
             int kdist){
   
double epslik = 1.0e-3,flkg_5 = 0.0e00,ygl,ygu;
int kccode;

kccode = std::abs(kcodep);

// if kcodep<0 here, we want to compute the conditioning
// probability for truncated data
   if(kcodep < 0){
      
      if(kccode == 1) flkg_5 = gpdfl(yl,gamme,kdist);
      if(kccode == 2) flkg_5 = gcdfl(yl,gamme,kdist);
      if(kccode == 3) flkg_5 = gcdfml(yl,gamme,kdist);
      if(kccode == 4) {
         
         ygl = yl;
         ygu = yu;
         
         flkg_5 = std::log(std::max(gcdf(ygu,gamme,kdist) - gcdf(ygl,gamme,kdist),1.0e-30));
         
      }
      
    } else {
      
      if(kccode == 1) flkg_5 = gpdfl(yl,gamme,kdist);
      if(kccode == 2) flkg_5 = gcdfml(yl,gamme,kdist);
      if(kccode == 3) flkg_5 = gcdfl(yl,gamme,kdist);
      if(kccode == 4) {
         
         ygl = yl;
         ygu = yu;
         
         flkg_5 = std::log(std::max(gcdf(ygu,gamme,kdist) - gcdf(ygl,gamme,kdist),1.0e-30));
         
      }
      
      if(kccode == 5) {
         
         ygl = yl - epslik;
         ygu = yl + epslik;
         
         flkg_5 = std::log(std::max(gcdf(ygu,gamme,kdist) - gcdf(ygl,gamme,kdist),1.0e-30));
         
      }
      
   }
   
return flkg_5;

}
