#include <base/base.hpp>
#include <spgeng/gcdf.hpp>

//' R interface for GENG cdf;
//' @name spgeng
// [[Rcpp::export]]
Rcpp::NumericVector spgeng(Rcpp::NumericVector tvec,
                           Rcpp::NumericMatrix gamme,
                           int maxlen,
                           Rcpp::NumericVector answer){

for(int i = 0; i < maxlen; i++){

    answer.at(i) = gcdf(tvec.at(i),gamme.column(i),9);

}

return answer;
  
}

#include <base/base.hpp>
#include <spgeng/usrcdf.hpp>
#include <genmax/zgtran.hpp>
#include <utility/dexpc.hpp>
#include <spgeng/pbgg.hpp>
#include <spgeng/usrcdf.hpp>
#include <utility/wqm_dxerc.hpp>

//'  CDF of specified general distribution
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

double gcdf(double y,
            Rcpp::NumericVector gamme,
            int kdist){

double res = 6.5, z;

//  #check for user specified cdf
if(kdist > 100) { return usrcdf(y,gamme,kdist); }

if((kdist > 0) and (kdist < 7)) {
  
    // standardize for location-scale distributions
    z = zgtran((y - gamme.at(0)) / gamme.at(1),kdist);
  
    if((kdist == 1) or (kdist == 2)) { // sev
      
        return one - dexpc(-1 * dexpc(z));
      
    } 
    if((kdist == 3) or (kdist == 4)) { // normal
      
        return half * wqm_dxerc(-z * root);
      
    } 
    if((kdist == 5) or (kdist == 6)) { // logistic
      
        return one / (one + dexpc(-z));
      
    } 
}


if((kdist == 7) or (kdist == 8)) { // exponential
 
    z = zgtran(y - gamme.at(0), 1);
  
    return one - dexpc(-1 * dexpc(z));
  
}

if((kdist == 9) or (kdist == 10)) { // generalized gamma
  
    z = zgtran((y - gamme.at(0)) / gamme.at(1),5);
    res = pbgg(z,gamme.at(3),gamme.at(5),gamme.at(4));
  
}

if((kdist == 11) or (kdist == 12)) { // regular gamma
  
    z = zgtran(y - gamme.at(0),5);
  
    return pbgg(z, gamme.at(1),gamme.at(3),gamme.at(2));
  
}

return res;
      
}

#include <base/base.hpp>
#include <utility/dexpc.hpp>
#include <utility/wqm_dxerc.hpp>
#include <spgeng/gaminc.hpp>

//' CDF(z) for the generalized gamma with shape xk

double pbgg(double z,
            double qq,
            double sqrtxk,
            double xk){

  
if(qq < 0) {
  
  double zt = xk * dexpc(-z / sqrtxk);
  return one - gaminc(zt,xk);
  
}

if(qq > 0) {
  
  double zt = xk * dexpc(z / sqrtxk);
  return gaminc(zt,xk);
  
}

  // cdf(z) for the normal
  return half * wqm_dxerc(-z * root);
  
}

#include <base/base.hpp>
#include <postkp/dlgama.hpp>
#include <utility/dexpc.hpp>

double gaminc(double x,
              double p){
  
//  incomplete gamma function
double tol = 1.0e-16;
double expmin = -70.0e00,expmax = 70.0e00;
double big30 = 1.0e20, bigx = 1.0e10;
double prob, fact, factl, ratio, xi;
double top1, top2, top3, bot1, bot2, bot3;
double ximp, zzadd, zzsub, diff;

prob = zero;
int ier = 0;

if(p <= zero) ier = 1;
if(x <  zero) ier = 2;

if((ier > 0) or (x == zero)) return prob;

fact  = zero;
factl = p * std::log(x) - x - dlgama(p);

if((factl >= expmax) or (x >= bigx)) {

  prob = one;
  return prob;
}

if(factl > expmin) fact = dexpc(factl);

if((x <= one) or (x < p)) {

// series expansion
        prob  = one;
        xi    = one;
        ratio = p;
line20: ratio = ratio + one;
        xi    = xi * x / ratio;
        prob = prob + xi;
        if(xi > tol) goto line20;
        prob = prob * fact / p;
        return prob;
}

//  calculation by continued fraction
        ximp  = one - p;
        zzadd = ximp + x + one;
        xi = zero;
        top1 = one;
        bot1 = x;
        top2 = x + one;
        bot2 = x * zzadd;
        prob = top2 / bot2;
line32: ximp = ximp + one;
        zzadd = zzadd + two;
        xi    = xi + one;
        zzsub = ximp * xi;
        top3  = zzadd * top2 - zzsub * top1;
        bot3  = zzadd * bot2 - zzsub * bot1;

        if(fabs(bot3 - zero) > 1.0e-6) {

           ratio = top3 / bot3;
           diff  = fabs(prob - ratio);

           if(diff <= tol) {

              if(diff <= tol * ratio) {

                goto exit;

              }

           }

           prob = ratio;

        }

        top1 = top2;
        bot1 = bot2;
        top2 = top3;
        bot2 = bot3;

        if(std::fabs(top3) < big30) goto line32;

        top1 = top1 / big30;
        bot1 = bot1 / big30;
        top2 = top2 / big30;
        bot2 = bot2 / big30;

        goto line32;

exit: prob = one - prob * fact;

      return prob;
}

#include <base/base.hpp>

//' user-specified cdf

double usrcdf(double y,
              Rcpp::NumericVector gamme,
              int kdist){ 

return 0.0e00;
  
}
