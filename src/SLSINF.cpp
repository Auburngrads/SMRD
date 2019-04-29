#include <base/base.hpp>
#include <slsinf/lsinf.hpp>

//' Does good stuff
//' @name slsinf
// [[Rcpp::export]]
Rcpp::List slsinf(int idist,
                  int itype,
                  Rcpp::NumericVector zlv,
                  Rcpp::NumericVector zrv,
                  Rcpp::NumericVector f11,
                  Rcpp::NumericVector f12,
                  Rcpp::NumericVector f22,
                  int nrows,
                  int ifault,
                  int irow){

for(irow = 0; irow < nrows; irow++){

    ifault = 111;

    Rcpp::List LSINF = lsinf(idist,
                             itype,
                             zlv.at(irow),
                             zrv.at(irow),
                             f11.at(irow),
                             f12.at(irow),
                             f22.at(irow),
                             ifault);

    ifault = Rcpp::as<int>(Rcpp::as<Rcpp::List>(LSINF)["ifault"]);
    f11.at(irow) = Rcpp::as<double>(Rcpp::as<Rcpp::List>(LSINF)["f11"]);
    f12.at(irow) = Rcpp::as<double>(Rcpp::as<Rcpp::List>(LSINF)["f12"]);
    f22.at(irow) = Rcpp::as<double>(Rcpp::as<Rcpp::List>(LSINF)["f22"]);

    if(ifault >= 4) goto exit;
    
}

exit: return Rcpp::List::create(Named("f11") = f11,
                                Named("f12") = f12,
                                Named("f22") = f22,
                                Named("ifault") = ifault);
}

#include <base/base.hpp>
#include <slsinf/lsint.hpp>

//'    computes:  fisher information matrix elements for time (type i)
//'               or failure (type ii) censored units from the smallest
//'               extreme value (sev), largest extreme value (lev),
//'               normal, or logistic distribution
//'
//'   parameters:
//'
//'    idist - integer - input:  idist=1 if the distribution is sev
//'                                   =2 if the distribution is lev
//'                                   =3 if the distribution is normal
//'                                   =4 if the distribution is logistic
//'
//'    itype - integer - input:  itype=1 for no censoring
//'                                   =2 right censoring
//'                                   =3 left censoring
//'                                   =4 interval censoring
//'
//'       zl - real -    input:  standardized left censoring point. zl is
//'                              defined by zl=q(pl), where pl is the
//'                              (expected) proportion of left censored
//'                              units and q(p) is the pth quantile of
//'                              the standardized distribution specified
//'                              by idist.  zl is not referenced when
//'                              itype=2
//'
//'       zr - real -    input:  standardized right censoring point.
//'                              zr=q(1-pr), where pr is the (expected)
//'                              proportion of right censored units.
//'                              zr is not referenced when itype=3
//'
//'      f11 - real -    output: c*(entry(1,1) of the fisher matrix)
//'      f12 - real -    output: c*(entry(1,2) of the fisher matrix)
//'      f22 - real -    output: c*(entry(2,2) of the fisher matrix)
//'                              where c=sigma*sigma/n
//'
//'   ifault - integer - output: ifault=0 indicates successful completion
//'                                    =1 indicates idist is other than
//'                                       1, 2, 3, 4
//'                                    =2 indicates itype is other than
//'                                       1, 2, 3, or 4
//'                                    =3 indicates itype=4 and zr.lt.zl
//'                                    =4 indicates that the series
//'                                       expansions in sevint failed to
//'                                       converge
//'                                    =5 indicates the euler's transfor-
//'                                       mation in logint failed to
//'                                       converge

Rcpp::List lsinf(int idist,
                 int itype,
                 double zl,
                 double zr,
                 double f11,
                 double f12,
                 double f22,
                 int ifault){

double big, small, zupp, zlow;
double thet0l = 0.0;
double thet0r = 0.0;
double thet1l = 0.0;
double thet1r = 0.0;
double thet2l = 0.0;
double thet2r = 0.0;
double eta    = 0.0;

big = 1.0e10;
small = -1.0e10;

// Check for illegal idist
   if((idist < 1) or (idist > 4)) {

       ifault = 1;
       goto exit;

}

// Check for illegal itype
   if((itype < 1) or (itype > 4)) {

       ifault = 2;
       goto exit;
       
}

// set censoring bounds for itype = 1, 2, 3, 4
// corresponding to complete, right, left, and
// interval cases, respectively

if(itype == 1) { // uncensored case

  zupp = big;
  zlow = small;

}

if(itype == 2) { // right censored at zr

   zupp = zr;
   zlow = small;

}

if(itype == 3) { // left censored at zl

  zupp = big;
  zlow = zl;

}

// interval censored - first check that the left
// censoring bound is not greater than the right bound
   if(itype == 4) {

      if(zl > zr) {

         ifault = 3;
         goto exit;

      }
   
      zupp = zr;
      zlow = zl;

}

// fisher matrix elements
   lsint(idist,zupp,thet0r,thet1r,thet2r,eta,ifault);

   if((ifault == 4) or (ifault == 5)) goto exit;

   lsint(idist,zlow,thet0l,thet1l,thet2l,eta,ifault);

   if((ifault == 4) or (ifault == 5)) goto exit;

   f11 = thet0r - thet0l + eta;
   f12 = thet1r - thet1l + zlow * eta;
   f22 = thet2r - thet2l + zlow * zlow * eta;

exit: return Rcpp::List::create(Named("f11") = f11,
                                Named("f12") = f12,
                                Named("f22") = f22,
                                Named("ifault") = ifault);

}

#include <base/base.hpp>
#include <slsinf/sevint.hpp>
#include <slsinf/levint.hpp>
#include <slsinf/logint.hpp>
#include <slsinf/norint.hpp>

//' @details Routes the computation of theta0,
//'          theta1, theta2, and eta to sevint,
//'          levint, norint, or logint according
//'          to the value of \code{idist}.

void lsint(int &idist,
           double &z,
           double &theta0,
           double &theta1,
           double &theta2,
           double &eta,
           int &ifault){

if(idist == 1) {
  
   sevint(z,theta0,theta1,theta2,eta,ifault);

}

if(idist == 2) {

   levint(z,theta0,theta1,theta2,eta,ifault);

}

if(idist == 3) {

   norint(z,theta0,theta1,theta2,eta,ifault);

}

if(idist == 4) {

   logint(z,theta0,theta1,theta2,eta,ifault);

}

return;

}

#include <base/base.hpp>
#include <slsinf/sevint.hpp>

//' computes: quantities needed to obtain the elements of the fisher;
//' information matrix from a largest extreme value;
//' distribution and censored data;
//' theta0, theta1, and theta2 are up to an additive constant the;
//' integrals over (-infinity,z) of the functions:;
//' g0(x)=h(x)*h(x)*g(x), g1(x)=(1+x*h(x))*g(x),;
//' and g2(x)=g(x)*(1+x*h(x))**2, where h(x)=1. g(x) and bg(x) are;
//' pdf and the cdf for a largest extreme value distribution;
//' theta0, theta1, theta2, and eta all are computed using;
//' sevint and the relationship between a largest and a smallest;
//' extreme value distribution;

void levint(double &z,
            double &theta0,
            double &theta1,
            double &theta2,
            double &eta,
            int &ifault){

double asymp1, asymp2, one, zsev;
double t0sev  = 0.0;
double t1sev  = 0.0;
double t2sev  = 0.0;
double etasev = 0.0;

// asymp1 and asymp2 are the integrals for g1 and g2;
// when z=+infinity;
   asymp1 = -0.4227843350984671e0;
   asymp2 =  0.1823680660852879e1;
   one    = 1.0e0;

// call sevint to compute the fisher matrix elements;
   zsev = -z;
   
   sevint(zsev,t0sev,t1sev,t2sev,etasev,ifault);

// compute thetas and eta for the lev;
   theta0 = one - t0sev + etasev;
   theta1 = asymp1 + t1sev + z * etasev;
   theta2 = asymp2 - t2sev + z * z * etasev;
   eta    = etasev;

return;

}

#include <base/base.hpp>

//' logistic pdf
double pdflog(double x){

  double one = 1.0e0;
  return one / (std::exp(x) * std::pow(one + std::exp(-x),2));
}
//' logistic cdf
double cdflog(double x){

  double one = 1.0e0;
  return one / (one + std::exp(-x));

}

//' computes: quantities needed to obtain the elements of the fisher;
//' information matrix from a logistic distribution and;
//' censored data;
//' theta0, theta1, and theta2 are the integrals over (-infinity,z);
//' of the following functions: g0(x)=h(x)*h(x)*g(x), g1(x)=(one+;
//' x*h(x))*g(x), and g2(x)=g(x)*(one+x*h(x))**2, where;
//' h(x)=1-bg(x). g(x) and bg(x) are the pdf and cdf for a standard;
//' logistic;
//' theta0 and theta1 have closed form formulas. theta2 is computed;
//' using euler's transformation on a powers series expansion of the;
//' integral over (-infinity,z) of log(1+exp(x));
//' eta=g(x)*g(x)/((1-bg(x))*bg(x));
//' ifault= 5 if the euler's transformation used to accelerate the;
//' convergence of s3 does not converge within tol of the;
//' true value after including jmax terms in the expansion.;
//' tol and jmax are set to 40 and 10**(-11) in the data;
//' statements;

void logint(double &z,
            double &theta0,
            double &theta1,
            double &theta2,
            double &eta,
            int &ifault){

double anuz, asymp0, asymp2, bgz, const0;
double emabsz,ez, gz, g3z, half, one, sz, s3, s3old;
double three, tmp, tol, two, x1, x2, x3, x4;
double zero, zmax, zmin;
double dum = 0.0;
Rcpp::NumericVector wksp(40,0.0);

// asymp0, asymp2 are the integrals for g0 and g2 when;
// z=+infinity. const0=pi*pi/6, where pi=3.14159....;
   asymp0 = 0.3333333333333333e0;
   asymp2 = 0.14299560445654842e1;
   const0 = 0.1644934066848226e1;
   half = 0.5e0;
   int jmax = 40;
   one = 1.0e0;
   three = 3.0e0;
   tol   = 1.0e-11;
   two   = 2.0e0;
   zero  = 0.0e0;
   zmax  = 34.0e0;
   zmin  = -34.0e0;

// initial values;
   ifault = 0;
   eta = zero;

// check for extreme z value;
   if(z >= zmax) {
   
     // asymptotic values. z is greater than or equal to zmax;
        theta0 = asymp0;
        theta1 = zero;
        theta2 = asymp2;
        return;
   
   }

if(z <= zmin) {

  // asymptotic values. z is smaller than or equal to zmin;
     theta0 = zero;
     theta1 = zero;
     theta2 = zero;
     return;

}

// compute theta0=integral of g0(x) over (-infinity,z);
   ez = std::exp(z);
   sz = one / (one + ez);
   theta0 = (one - std::pow(sz,3)) / three;

// compute theta1=integral of g1(x) over (-infinity,z) and eta;
   bgz = R::plogis(z, 0,1, true, false);
   gz = R::dlogis(z, 0,1, false);
   g3z = log(one + ez);
   theta1 = z * theta0 + (gz - g3z) / three;
   eta = gz;

// compute theta2=integral of g2(x) over (-infinity,z);
// s3 is computed using a power series expansion;
// with accelerated convergence from euler transformation;
   emabsz = std::exp(-1 * std::fabs(z));
   x1 = emabsz;
   s3 = zero;
   
// euler partial sum based on the first term;
   int n = 0;
   wksp.at(0) = x1;
   s3 = half * x1;
   s3old = s3;

for(int jterm = 2; jterm <= jmax; jterm++){

    x2 = std::pow((double)jterm, 2);
    x3 = std::pow((double)(jterm - 1), 2);
    x1 = -x1 * emabsz * x3 / x2;
    
    // euler partial sum based on two or more terms;
       tmp = wksp.at(0);
       wksp.at(0) = x1;

    for(int j = 0; j <= n; j++){

        if(j < n) dum = wksp.at(j + 1);
        wksp.at(j + 1) = half * (wksp.at(j) + tmp);
        if(j < n) tmp = dum;

    }

    // euler improved partial sums;

    if(std::fabs(wksp.at(n + 1)) > std::fabs(wksp.at(n))) {

       s3 = s3 + wksp.at(n + 1);

    } else {

       s3 = s3 + half * wksp.at(n + 1);
       n = n + 1;

    }

    // tests for convergence of the series - the summation stops when;
    // the absolute difference between two consecutive partial sums is;
    // less than tol - a fault is declared if convergence is not;
    // reached in a maximum of jmax terms;
       x4 = std::fabs(s3old - s3);
       s3old = s3;

    if(x4 < tol) {

      // add terms to obtain the integral;
         anuz = s3;
         if(z > zero) anuz = const0 + z * z / two - s3;
         theta2 = z * theta1 + (bgz - z * g3z + z * gz + two * anuz) / three;
         return;

    }
}

ifault = 5;

return;

}

#include <base/base.hpp>
#include <utility/wqm_dxerc.hpp>

//' normal pdf
double pdfnor(double x){

  // cval = 1 / sqrt(2 * pi) where pi=3.14159....
  double cval = 0.3989422804014326e0;
  double half = 0.5e0;
  return cval * std::exp(-half * x * x);

}

//' normal cdf
double cdfnor(double x){

  // root = 1 / sqrt(2)
  double root = 0.7071067811865475e0;
  double half = 0.5e0;

  return half * wqm_dxerc(-x * root);

}

//' computes: quantities needed to obtain the elements of the fisher;
//' information matrix from a normal distribution and;
//' censored data;
//' theta0, theta1, and theta2 are the integrals over (-infinity,z);
//' of the following functions: g0(x)=h(x)*h(x)*g(x), g1(x)=(one+;
//' x*h(x))*g(x), and g2(x)=g(x)*(one+x*h(x))**2, where h(x)=psi(x);
//' +g(x)/(one-bg(x)) and psi(x)=-x. g(x) and bg(x) are the pdf and;
//' cdf for a standard normal;
//' theta0, theta1, theta2, and eta depend all on bg(z) and they;
//' are obtained using the complementary error function;
//' eta=g(x)*g(x)/((1-bg(x))*bg(x));

void norint(double &z,
            double &theta0,
            double &theta1,
            double &theta2,
            double &eta,
            int &ifault){

double bgz, denom, gz, gzs;
double one, sz, two, zero;
double zmax, zmin;

one  = 1.0e0;
two  = 2.0e0;
zero = 0.0e0;
zmax = 8.0e0;
zmin = -8.0e0;

// initial values;
   ifault = 0;
   eta = zero;
   
// check for extreme z value;
   if(z >= zmax) {
   
     // asymptotic values. z is greater than or equal to zmax;
        theta0 = one;
        theta1 = zero;
        theta2 = two;
        return;
   
   }

if(z <= zmin) {

  // asymptotic values. z is smaller than or equal to zmin;
     theta0 = zero;
     theta1 = zero;
     theta2 = zero;
     return;

}

// compute theta0=integral of g0(x) over (-infinity,z);
   sz = R::pnorm(-z, 0,1, true, false);
   gz = R::dnorm(z, 0,1, false);
   gzs = gz * gz;
   bgz = R::pnorm(z, 0,1, true, false);
   theta0 = bgz - z * gz + gzs / sz;
   denom = bgz * sz;

if(denom > zero) eta = gzs / denom;

// compute theta1=integral of g1(x) over (-infinity,z);
   theta1 = z * theta0 - z * bgz - gz;

// compute theta2=integral of g2(x) over (-infinity,z);
   theta2 = z * theta1 + two * bgz;

return;

}

#include <base/base.hpp>

//' smallest extreme value pdf
double pdfsev(double x){ return std::exp(x - std::exp(x)); }

//' smallest extreme value cdf
double cdfsev(double x){ double one = 1.0e0; return one - std::exp(-1 * std::exp(x)); }

//' function needed in the gaussian quadrature
double g1(double x) { double one = 1.0e0; return (one + x) * std::exp(x - std::exp(x));}


//' computes: quantities needed to obtain the elements of the fisher;
//' information matrix from a smallest extreme value;
//' distribution and censored data;
//' theta0, theta1, and theta2 are the integrals over (-infinity,z);
//' of the functions: g0(x)=h(x)*h(x)*g(x), g1(x)=(1+x*h(x))*g(x),;
//' g2(x)=g(x)*(1+x*h(x))**2, where h(x)=1. g(x) and bg(x) are;
//' pdf and the cdf for a smallest extreme value distribution;
//' theta0 has a closed form formula. g1(x) and g2(x) are integrated;
//' by using power series expansions when z <= 1, and by power;
//' series expansions through z=1 plus a gaussian quadrature from;
//' 1 to z when z > 1;
//' eta=g(x)*g(x)/((1-bg(x))*bg(x));
//' ifault= 4 if the series expansions to compute s1 and s2 do not;
//' converge within tol of the true values after including;
//' jmax terms in the expansion. tol and jmax are set to 25;
//' and 10**(-11) in the data statements;

void sevint(double &z,
            double &theta0,
            double &theta1,
            double &theta2,
            double &eta,
            int &ifault){

double asymp1, asymp2, bgz, denom, ez;
double factor, gz, gzs, g1xl, g1xr, g2xl;
double sum1, sum2, sz, s1, s2, thet11, thet21;
double tol, two, x1, x2, x3, x4, x5, zero;
double g2xr, half, one, zmax, zmin, jmax ;

// p and w are constants used in the gaussian quadratures
NumericVector p = NumericVector::create(0.4947004674958250e0,
                                        0.4722875115366163e0,
                                        0.4328156011939159e0,
                                        0.3777022041775015e0,
                                        0.3089381222013219e0,
                                        0.2290083888286137e0,
                                        0.1408017753896295e0,
                                        0.4750625491881872e-1);

NumericVector w = NumericVector::create(0.1357622970587705e-1,
                                        0.3112676196932395e-1,
                                        0.4757925584124639e-1,
                                        0.6231448562776694e-1,
                                        0.7479799440828837e-1,
                                        0.8457825969750127e-1,
                                        0.9130170752246179e-1,
                                        0.9472530522753425e-1);

// asymp1, asymp2, thet11, and thet21 are the integrals;
// for g1 and g2 when z=+infinity and z=1, respectively;
   asymp1 = 0.4227843350984671e0;
   asymp2 = 0.1823680660852879e1;
   half   = 0.5e0;
   jmax   = 25.0;
   one    = 1.0e0;
   tol    = 1.0e-11;
   thet11 = 0.2720757938345342e0;
   thet21 = 0.1475933122158450e1;
   two    = 2.0e0;
   zero   = 0.0e0;
   zmax   = 3.6e0;
   zmin   = -34.0e0;

// initial values
   ifault = 0;
   eta = zero;
   
// check for extreme z value;
   if(z >= zmax) {
   
     // asymptotic values. z is greater than or equal to zmax;
        theta0 = one;
        theta1 = asymp1;
        theta2 = asymp2;
        return;
   
   }

if(z <= zmin) {

  // asymptotic values. z is smaller than or equal to zmin;
     theta0 = zero;
     theta1 = zero;
     theta2 = zero;
     return;
     
}

// compute theta0=integral of g0(x)=dexp(x-dexp(x)) and eta
// over (-infinity,z)
   ez  = std::exp(z);
   sz  = std::exp(-ez);
   gz  = pdfsev(z);
   gzs = gz * gz;
   bgz = cdfsev(z);
   theta0 = bgz;
   denom = bgz * sz;
   if(denom > zero) eta = gzs / denom;

// computation of theta1 and theta2;
// select integration by power series expansions or;
// by power series expansions plus a gaussian quadrature;
if(z < one) {

   // z is less than 1 -- integration by power series expansions;
      factor = -one;
      s1 = zero;
      s2 = zero;

   for(int j = 1; j <= jmax; j++){

       // terms to evaluate theta1=integral of g1(x)=(1+x)g(x);
       // over (-infinity,z);
          x5 = j;
          factor = -factor * ez / x5;
          x1 = z - one / x5;
          x3 = factor * x1;
          s1 = s1 + x3;

       // terms to evaluate theta2=integral of g2(x)=(1+x)g1(x);
       // over (-infinity,z);
          x2 = x1 * x1 + one / (x5*x5);
          x4 = factor * x2;
          s2 = s2 + x4;

       // tests for convergence of the series - the summations stop when;
       // the absolute value of the last added term is smaller than tol;
       // for both series - a fault is declared if convergence is not;
       // reached in a maximum of jmax terms;
          x5 = std::max(std::fabs(x3),std::fabs(x4));

          if(x5 < tol) {
   
             // add terms to obtain integrals;
                theta1 = theta0 + s1;
                theta2 = two * theta1 - theta0 + s2;
                return;
   
          }
   }

   ifault = 4;
   return;

} else {

  // z is between 1 and 3.6.;
  // thet11 and thet12 contain the integrals of g1(x) and g2(x);
  // over (-infinity,1) and they are defined in the data statements;
  // gaussian quadrature to integrate g1(x) and g2(x) over (1,z);
  // sums are storaged in sum1 and sum2, respectively;
     sum1 = zero;
     sum2 = zero;
     x1 = half * (z + one);
     x2 = z - one;

     for(int iquad = 0; iquad < 8; iquad++){
   
         x3   = x1 - p.at(iquad) * x2;
         x4   = x1 + p.at(iquad) * x2;
         g1xl = g1(x3);
         g1xr = g1(x4);
         g2xl = (one + x3) * g1xl;
         g2xr = (one + x4) * g1xr;
         sum1 = sum1 + w.at(iquad) * (g1xl + g1xr);
         sum2 = sum2 + w.at(iquad) * (g2xl + g2xr);
   
     }

     sum1 = sum1 * x2;
     sum2 = sum2 * x2;
   
     // add terms to obtain integrals;
        theta1 = sum1 + thet11;
        theta2 = sum2 + thet21;

}

return;

}
