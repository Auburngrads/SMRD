#include <base/base.hpp>
#include <ssft2gr1/wqm_dqagse.hpp>

//**begin prologue dqags;
//**date written 800101 (yymmdd);
//**revision date 830518 (yymmdd);
//**category no. h2a1a1;
//**keywords automatic integrator, general-purpose,;
// (end-point) singularities, extrapolation,;
// globally adaptive;
//**author piessens,robert,appl. math. & progr. div. - k.u.leuven;
// de doncker,elise,appl. math. & prog. div. - k.u.leuven;
//**purpose the routine calculates an approximation result to a given;
// definite integral i = integral of f over (a,b),;
// hopefully satisfying following claim for accuracy;
// abs(i-result) <= max(epsabs,epsrel*abs(i)).;
//**description;
// computation of a definite integral;
// standard fortran subroutine;
// double precision version;
// parameters;
// on entry;
// f - double precision;
// function subprogram defining the integrand;
// function f(x). the actual name for f needs to be;
// declared e x t e r n a l in the driver program.;
// a - double precision;
// lower limit of integration;
// b - double precision;
// upper limit of integration;
// epsabs - double precision;
// absolute accuracy requested;
// epsrel - double precision;
// relative accuracy requested;
// if epsabs <= 0;
// and epsrel < max(50*rel.mach.acc.,0.5d-28),;
// the routine will end with ier = 6.;
// on return;
// result - double precision;
// approximation to the integral;
// abserr - double precision;
// estimate of the modulus of the absolute error,;
// which should equal or exceed abs(i-result);
// neval - integer;
// number of integrand evaluations;
// ier - integer;
// ier = 0 normal and reliable termination of the;
// routine. it is assumed that the requested;
// accuracy has been achieved.;
// ier > 0 abnormal termination of the routine;
// the estimates for integral and error are;
// less reliable. it is assumed that the;
// requested accuracy has not been achieved.;
// error messages;
// ier = 1 maximum number of subdivisions allowed;
// has been achieved. one can allow more sub-;
// divisions by increasing the value of limit;
// (and taking the according dimension;
// adjustments into account. however, if;
// this yields no improvement it is advised;
// to analyze the integrand in order to;
// determine the integration difficulties. if;
// the position of a local difficulty can be;
// determined (e.g. singularity,;
// discontinuity within the interval) one;
// will probably gain from splitting up the;
// interval at this point and calling the;
// integrator on the subranges. if possible,;
// an appropriate special-purpose integrator;
// should be used, which is designed for;
// handling the type of difficulty involved.;
// = 2 the occurrence of roundoff error is detec-;
// ted, which prevents the requested;
// tolerance from being achieved.;
// the error may be under-estimated.;
// = 3 extremely bad integrand behaviour;
// occurs at some points of the integration;
// interval.;
// = 4 the algorithm does not converge.;
// roundoff error is detected in the;
// extrapolation table. it is presumed that;
// the requested tolerance cannot be;
// achieved, and that the returned result is;
// the best which can be obtained.;
// = 5 the integral is probably divergent, or;
// slowly convergent. it must be noted that;
// divergence can occur with any other value;
// of ier.;
// = 6 the input is invalid, because;
// (epsabs <= 0 and;
// epsrel < max(50*rel.mach.acc.,0.5d-28);
// or limit < 1 or lenw < limit*4.;
// result, abserr, neval, last are set to;
// zero.except when limit or lenw is invalid,;
// iwork(1), work(limit*2+1) and;
// work(limit*3+1) are set to zero, work(1);
// is set to a and work(limit+1) to b.;
// dimensioning parameters;
// limit - integer;
// dimensioning parameter for iwork;
// limit determines the maximum number of subintervals;
// in the partition of the given integration interval;
// (a,b), limit >= 1.;
// if limit < 1, the routine will end with ier = 6.;
// lenw - integer;
// dimensioning parameter for work;
// lenw must be at least limit*4.;
// if lenw < limit*4, the routine will end;
// with ier = 6.;
// last - integer;
// on return, last equals the number of subintervals;
// produced in the subdivision process, detemines the;
// number of significant elements actually in the work;
// arrays.;
// work arrays;
// iwork - integer;
// vector of dimension at least limit, the first k;
// elements of which contain pointers;
// to the error estimates over the subintervals;
// such that work(limit*3+iwork(1)),... ,;
// work(limit*3+iwork(k)) form a decreasing;
// sequence, with k = last if last <= (limit/2+2),;
// and k = limit+1-last otherwise;
// work - double precision;
// vector of dimension at least lenw;
// on return;
// work(1), ..., work(last) contain the left;
// end-points of the subintervals in the;
// partition of (a,b),;
// work(limit+1), ..., work(limit+last) contain;
// the right end-points,;
// work(limit*2+1), ..., work(limit*2+last) contain;
// the integral approximations over the subintervals,;
// work(limit*3+1), ..., work(limit*3+last);
// contain the error estimates.;
//**references (none);
//**routines called dqagse,xerror;

void wqm_dqags(double (*f)(double),
               double &a,
               double &b,
               double &epsabs,
               double &epsrel,
               double &result,
               double &abserr,
               int &neval,
               int &ier,
               int &limit,
               int &lenw,
               int &last,
               Rcpp::IntegerVector &iwork,
               Rcpp::NumericVector &work){
  
int lvl,l1,l2,l3;
  
// Check validity of limit and lenw
   ier = 6;
   neval = 0;
   last = 0;
   result = 0.0e+00;
   abserr = 0.0e+00;
   if((limit < 1) or (lenw < (limit * 4))) goto line10;
   
// Prepare call for dqagse
   l1 = limit + 1;
   l2 = limit + l1;
   l3 = limit + l2;
   wqm_dqagse(f,a,b,epsabs,epsrel,limit,result,abserr,neval,
              ier,work,l1,l2,l3,iwork,last);
   
// Call error handler if necessary
   lvl = 0;
   line10: if(ier == 6) lvl = 1;
   
   if(ier != 0){
     
      if(lvl == 1) Rcpp::stop("\nAbnormal return from wqm_dqags -- ier = %i", ier);
      Rcpp::warning("\nAbnormal return from wqm_dqags -- ier = %i", ier);
     
   } 

return;

}
