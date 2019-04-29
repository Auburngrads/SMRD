#include <base/base.hpp>
#include <sgpdfl/gpdfl.hpp>

//' R interface for GENG cdf;
//' @name sgpdfl
// [[Rcpp::export]]
Rcpp::NumericVector sgpdfl(Rcpp::NumericVector tvec,
                           Rcpp::NumericMatrix gamme,
                           int maxlen,
                           Rcpp::NumericVector answer){

for(int i = 0; i < maxlen; i++){

    answer.at(i) = gpdfl(tvec.at(i),gamme.column(i),9);

}

return answer;
  
}

#include <base/base.hpp>
#include <sgpdfl/usrpdf.hpp>
#include <spgeng/zgtran.hpp>
#include <utility/dexpc.hpp>
#include <sgpdfl/pdfggl.hpp>

//'  PDF of specified general distribution
//'  
//'  gamme(i) for user-specified distributions
//'  
//'  dist      i:   1    2    3    4    5    6    7    8    9    10    11
//'----------------------------------------------------------------------
//'  sev            m    s    ls
//'  normal         m    s    ls
//'  logistic       m    s    ls
//'  log-exponent   m
//'  log-gengamma   m    s    ls   q    k    sk   lk  lgk
//'  log-gamma      m    q    k    sk   lk   gk

double gpdfl(double y,
             Rcpp::NumericVector gamme,
             int kdist){

double z;
double smalls = 1.0e-08, smpdfl = -1.0e05;
double hlntp = -0.9189385332046794e00;

//  #check for user specified cdf
if(kdist > 100) { return std::log(usrpdf(y,gamme,kdist)); }

if((kdist > 0) and (kdist < 7)) {
  
    // standardize for location-scale distributions
    z = zgtran((y - gamme.at(0)) / gamme.at(1),kdist);
  
    if(gamme.at(1) <= smalls) return smpdfl;
  
    if((kdist == 1) or (kdist == 2)) { // sev
      
        return -1 * dexpc(z) + z - gamme.at(2);
      
    } 
    if((kdist == 3) or (kdist == 4)) { // normal
      
        return hlntp - gamme.at(2) - half * z * z;
      
    } 
    if((kdist == 5) or (kdist == 6)) { // logistic
      
        return -z - two * std::log(one + dexpc(-z)) - gamme.at(2);
      
    } 
}


if((kdist == 7) or (kdist == 8)) { // exponential
 
    z = zgtran(y - gamme.at(0), 1);
    return -1 * dexpc(z) + z;
  
}

if((kdist == 9) or (kdist == 10)) { // generalized gamma
  
    z = zgtran((y - gamme.at(0)) / gamme.at(1),5);
    if(gamme.at(1) <= smalls)  return smpdfl;
    
    return pdfggl(z,gamme.at(2),gamme.at(3),gamme.at(4),
                    gamme.at(5),gamme.at(6),gamme.at(7));
  
}

if((kdist == 11) or (kdist == 12)) { // regular gamma
  
    z = zgtran(y - gamme.at(0),5);
  
    return pdfggl(z,0.0e00,gamme.at(1),gamme.at(2),
                           gamme.at(3),gamme.at(4),gamme.at(5));
  
}

return smpdfl;
      
}

#include <base/base.hpp>
#include <utility/dexpc.hpp>
#include <sgpdfl/gpdfl.hpp>
#include <sgpdfl/usrpdf.hpp>

//' Compute pdf

double gpdf(double y,
            Rcpp::NumericVector gamme,
            int kdist){

// User specifed
   if(kdist > 100) return usrpdf(y,gamme,kdist);
   
   return dexpc(gpdfl(y,gamme,kdist));
   
}

#include <base/base.hpp>
#include <utility/dexpc.hpp>

//' PDF(z) for the generalized gamma with shape xk

double pdfggl(double z,
              double sigmal,
              double qq,
              double xk,
              double sqrtxk,
              double xklog,
              double xlgam){

double zmin  = -30.0e00, zmax = 30.0e00;
double hlntp = -0.9189385332046794e00;

if(qq < 0) {
  
  double zt = -z;
  double zs = zt / sqrtxk;
  if(zs > zmax) return zero;
  if(zs < zmin) return zero;
  
  return (xk - half) * xklog - xlgam - sigmal + sqrtxk * zt - xk * dexpc(zs);
  
}

if(qq > 0) {
  
  double zt = z;
  double zs = zt / sqrtxk;
  if(zs > zmax) return zero;
  if(zs < zmin) return zero;
  
  return (xk - half) * xklog - xlgam - sigmal + sqrtxk * zt - xk * dexpc(zs);
  
}

  // pdf(z) for the normal
  return hlntp - sigmal - half * z * z;
  
}

#include <base/base.hpp>

//' User specified pdf

double usrpdf(double y,
              Rcpp::NumericVector gamme,
              int kdist){
  
 Rcpp::warning("\nThis is unfinished");
  
 return 0.0e00;
 
}