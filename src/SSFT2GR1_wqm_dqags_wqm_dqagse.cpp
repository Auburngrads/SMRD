#include <base/base.hpp>
#include <utility/dmachine.hpp>
#include <ssft2gr1/wqm_dqk21.hpp>
#include <ssft2gr1/wqm_dqpsrt.hpp>
#include <sfteval/dqelg.hpp>

//**begin prologue dqagse;
//**date written 800101 (yymmdd);
//**revision date 830518 (yymmdd);
//**category no. h2a1a1;
//**keywords automatic integrator, general-purpose,;
// (end point) singularities, extrapolation,;
// globally adaptive;
//**author piessens,robert,appl. math. & progr. div. - k.u.leuven;
// de doncker,elise,appl. math. & progr. div. - k.u.leuven;
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
// limit - integer;
// gives an upperbound on the number of subintervals;
// in the partition of (a,b);
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
// = 1 maximum number of subdivisions allowed;
// has been achieved. one can allow more sub-;
// divisions by increasing the value of limit;
// (and taking the according dimension;
// adjustments into account). however, if;
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
// extrapolation table.;
// it is presumed that the requested;
// tolerance cannot be achieved, and that the;
// returned result is the best which can be;
// obtained.;
// = 5 the integral is probably divergent, or;
// slowly convergent. it must be noted that;
// divergence can occur with any other value;
// of ier.;
// = 6 the input is invalid, because;
// epsabs <= 0 and;
// epsrel < max(50*rel.mach.acc.,0.5d-28).;
// result, abserr, neval, last, rlist(1),;
// iord(1) and elist(1) are set to zero.;
// alist(1) and blist(1) are set to a and b;
// respectively.;
// alist - double precision;
// vector of dimension at least limit, the first;
// last elements of which are the left end points;
// of the subintervals in the partition of the;
// given integration range (a,b);
// blist - double precision;
// vector of dimension at least limit, the first;
// last elements of which are the right end points;
// of the subintervals in the partition of the given;
// integration range (a,b);
// rlist - double precision;
// vector of dimension at least limit, the first;
// last elements of which are the integral;
// approximations on the subintervals;
// elist - double precision;
// vector of dimension at least limit, the first;
// last elements of which are the moduli of the;
// absolute error estimates on the subintervals;
// iord - integer;
// vector of dimension at least limit, the first k;
// elements of which are pointers to the;
// error estimates over the subintervals,;
// such that elist(iord(1)), ..., elist(iord(k));
// form a decreasing sequence, with k = last;
// if last <= (limit/2+2), and k = limit+1-last;
// otherwise;
// last - integer;
// number of subintervals actually produced in the;
// subdivision process;
//**references (none);
//**routines called d1mach,dqelg,dqk21,dqpsrt;
// the dimension of rlist2 is determined by the value of;
// limexp in subroutine dqelg (rlist2 should be of dimension;
// (limexp+2) at least).;
// list of major variables;
// -----------------------;
// alist - list of left end points of all subintervals;
// considered up to now;
// blist - list of right end points of all subintervals;
// considered up to now;
// rlist(i) - approximation to the integral over;
// (alist(i),blist(i));
// rlist2 - array of dimension at least limexp+2 containing;
// the part of the epsilon table which is still;
// needed for further computations;
// elist(i) - error estimate applying to rlist(i);
// maxerr - pointer to the interval with largest error;
// estimate;
// errmax - elist(maxerr);
// erlast - error on the interval currently subdivided;
// (before that subdivision has taken place);
// area - sum of the integrals over the subintervals;
// errsum - sum of the errors over the subintervals;
// errbnd - requested accuracy max(epsabs,epsrel*;
// abs(result));
// *****1 - variable for the left interval;
// *****2 - variable for the right interval;
// last - index for subdivision;
// nres - number of calls to the extrapolation routine;
// numrl2 - number of elements currently in rlist2. if an;
// appropriate approximation to the compounded;
// integral has been obtained it is put in;
// rlist2(numrl2) after numrl2 has been increased;
// by one.;
// small - length of the smallest interval considered up;
// to now, multiplied by 1.5;
// erlarg - sum of the errors over the intervals larger;
// than the smallest interval considered up to now;
// extrap - logical variable denoting that the routine is;
// attempting to perform extrapolation i.e. before;
// subdividing the smallest interval we try to;
// decrease the value of erlarg.;
// noext - logical variable denoting that extrapolation;
// is no longer allowed (true value);
// machine dependent constants;
// ---------------------------;
// epmach is the largest relative spacing.;
// uflow is the smallest positive magnitude.;
// oflow is the largest positive magnitude.;

void wqm_dqagse(double (*f)(double),
                double &a,
                double &b,
                double &epsabs,
                double &epsrel,
                int &limit,
                double &result,
                double &abserr,
                int &neval,
                int &ier,
                Rcpp::NumericVector &work,
                int &l1,
                int &l2,
                int &l3,
                Rcpp::IntegerVector &iord,
                int &last){
  
double abseps,area,area1,area12,area2,a1,a2;
double b1,b2,correc = 0.0e00,defabs,defab1,defab2,dres;
double epmach,erlarg = 0.0e00,erlast,errbnd,errmax;
double error1,error2,erro12,errsum,ertest = 0.0e00;
double oflow,resabs,reseps,small = 0.0e00,uflow;
int id,ierro,iroff1,iroff2,iroff3,jupbnd,ksgn;
int ktmin,maxerr,nres,nrmax,numrl2;
bool extrap, noext;
Rcpp::NumericVector rlist2 = Rcpp::NumericVector(52);
Rcpp::NumericVector res3la = Rcpp::NumericVector(3);

epmach = D1mach(4);

// test on validity of parameters
   ier = 0;
   neval = 0;
   last = 0;
   result = 0.0e+00;
   abserr = 0.0e+00;
   work.at(0) = a;
   work.at((l1 - 1) + 0) = b;
   work.at((l2 - 1) + 0) = 0.0e+00;
   work.at((l3 - 1) + 0) = 0.0e+00;
   if((epsabs <= 0.0e+00) & (epsrel < std::max(0.5e+02 * epmach,0.5e-28))){
     
       ier = 6;
   }
   
   if(ier == 6) return;
   
// First appromation to the integral
   uflow = D1mach(1);
   oflow = D1mach(2);
   ierro = 0;
   wqm_dqk21(f,a,b,result,abserr,defabs,resabs);
   
// Test on accuracy
   dres = std::abs(result);
   errbnd = std::max(epsabs,epsrel * dres);
   last = 1;
   work.at((l2 - 1) + 0) = result;
   work.at((l3 - 1) + 0) = abserr;
   iord.at(0) = 1;
   if((abserr <= 1.0e+02 * epmach * defabs) & (abserr > errbnd)){
     
       ier = 2;
     
   } 
   
   if(limit == 1) ier = 1;
   if((ier != 0) or ((abserr <= errbnd) and (abserr != resabs)) or (abserr == 0.0e+00)){
     
       goto line140;
     
   }
   
// Initialization
   rlist2.at(0) = result;
   errmax = abserr;
   maxerr = 1;
   area = result;
   errsum = abserr;
   abserr = oflow;
   nrmax = 1;
   nres = 0;
   numrl2 = 2;
   ktmin = 0;
   extrap = false;
   noext = false;
   iroff1 = 0;
   iroff2 = 0;
   iroff3 = 0;
   ksgn = -1;
   if(dres >= ((0.1e+01 - 0.5e+02 * epmach) * defabs)) {
     
      ksgn = 1;
     
   }
   
// Main do-loop
   for(last = 2; last <= limit; last++){
   
    // Bisect the subinterval with the nrmax-th largest error estimate
       a1 = work.at(maxerr - 1);
       b1 = 0.5e+00 * (work.at(maxerr - 1) + work.at((l1 - 1) + maxerr - 1));
       a2 = b1;
       b2 = work.at((l1 - 1) + maxerr - 1);
       erlast = errmax;
       wqm_dqk21(f,a1,b1,area1,error1,resabs,defab1);
       wqm_dqk21(f,a2,b2,area2,error2,resabs,defab2);
       
    // Improve previous approximations to integral and error and test for accuracy.;
       area12 = area1 + area2;
       erro12 = error1 + error2;
       errsum = errsum + erro12 - errmax;
       area = area + area12 - work.at((l2 - 1) + maxerr - 1);
       if((defab1 == error1) or (defab2 == error2)) goto line15;
       if((std::abs(work.at((l2 - 1) + maxerr - 1) - area12) > (0.1e-04 * std::abs(area12))) or (erro12 < (0.99e+00 * errmax))){
         
          goto line10;
         
       }
       if(extrap)  iroff2 = iroff2 + 1;
       if(!extrap) iroff1 = iroff1 + 1;
       line10: if((last > 10) and (erro12 > errmax)) iroff3 = iroff3 + 1;
       line15: work.at((l2 - 1) + maxerr - 1) = area1;
               work.at((l2 - 1) + last - 1) = area2;
               errbnd = std::max(epsabs,epsrel * std::abs(area));
       
    // Test for roundoff error and eventually set error flag
       if(((iroff1 + iroff2) >= 10) or (iroff3 >= 20)) ier = 2;
       if(iroff2 >= 5) ierro = 3;
       
    // Set error flag in the case that the number of subintervals equals limit
       if(last == limit) ier = 1;
       
    // set error flag in the case of bad integrand behaviour;
    // at a point of the integration range.;
       if(std::max(std::abs(a1),std::abs(b2)) <= ((0.1e+01 + 0.1e+03 * epmach) * (std::abs(a2) + 0.1e+04 * uflow))){
         
          ier = 4;
         
       }
       
    // Append the newly-created intervals to the list
       if(error2 > error1) goto line20;
       work.at(last - 1) = a2;
       work.at((l1 - 1) + maxerr - 1) = b1;
       work.at((l1 - 1) + last - 1) = b2;
       work.at((l3 - 1) + maxerr - 1) = error1;
       work.at((l3 - 1) + last - 1) = error2;
       goto line30;
       
       line20: work.at(maxerr - 1) = a2;
               work.at(last - 1) = a1;
               work.at((l1 - 1) + last - 1) = b1;
               work.at((l2 - 1) + maxerr - 1) = area2;
               work.at((l2 - 1) + last - 1) = area1;
               work.at((l3 - 1) + maxerr - 1) = error2;
               work.at((l3 - 1) + last - 1) = error1;
               
    // Call subroutine dqpsrt to maintain the descending ordering
    // in the list of error estimates and select the subinterval
    // with nrmax-th largest error estimate (to be bisected next)
       line30: wqm_dqpsrt(limit,last,maxerr,errmax,work,l3,iord,nrmax);
       
    // Jump out of do-loop***
       if(errsum <= errbnd) goto line115;
       
    // Jump out of do-loop***
       if(ier != 0) goto line100;
       if(last == 2) goto line80;
       if(noext) goto line90;
       erlarg = erlarg - erlast;
       if(std::abs(b1 - a1) > small) erlarg = erlarg + erro12;
       if(extrap) goto line40;
       
    // Test whether the interval to be bisected next is the smallest interval
       if(std::abs(work.at((l1 - 1) + maxerr - 1) - work.at(maxerr - 1)) > small) goto line90;
       extrap = true;
       nrmax = 2;
       line40: if((ierro == 3) or (erlarg <= ertest)) goto line60;
   
    // The smallest interval has the largest error
    // Before bisecting decrease the sum of the errors over the
    // larger intervals (erlarg) and perform extrapolation
       id = nrmax; 
       jupbnd = last;
       if(last > (2 + std::floor(limit / 2))) jupbnd = limit + 3 - last;
       
       for(int k = id; k <= jupbnd; k++){
         
           maxerr = iord.at(nrmax - 1);
           errmax = work.at((l3 - 1) + maxerr - 1);
           
           // Jump out of this loop;
           if(std::abs(work.at((l1 - 1) + maxerr - 1) - work.at(maxerr - 1)) > small) goto line90;
           nrmax = nrmax + 1;

       }

    // Perform extrapolation
       line60: numrl2 = numrl2 + 1;
               rlist2.at(numrl2 - 1) = area;
               dqelg(numrl2,rlist2,reseps,abseps,res3la,nres);
               ktmin = ktmin + 1;
               
       if((ktmin > 5) and (abserr < (0.1e-02 * errsum))) ier = 5;
       if(abseps >= abserr) goto line70;
       ktmin = 0;
       abserr = abseps;
       result = reseps;
       correc = erlarg;
       ertest = std::max(epsabs,epsrel * std::abs(reseps));
       
    // Jump out of do-loop
       if(abserr <= ertest) goto line100;
       
    // Prepare bisection of the smallest interval
       line70: if(numrl2 == 1) noext = true;
       if(ier == 5) goto line100;
       maxerr = iord.at(0);
       errmax = work.at((l3 - 1) + maxerr - 1);
       nrmax = 1;
       extrap = false;
       small = small * 0.5e+00;
       erlarg = errsum;
       goto line90;
       line80: small = std::abs(b - a) * 0.375e+00;
       erlarg = errsum;
       ertest = errbnd;
       rlist2.at(1) = area;

line90: ;

   }

// Set final result and error estimate
   line100: if(abserr == oflow) goto line115;
   if((ier + ierro) == 0) goto line110;
   if(ierro == 3) abserr = abserr + correc;
   if(ier == 0) ier = 3;
   if((result != 0.0e+00) and (area != 0.0e+00)) goto line105;
   if(abserr > errsum) goto line115;
   if(area == 0.0e+00) goto line130;
   goto line110;
   line105: if(abserr / std::abs(result) > errsum / std::abs(area)) {
     
               goto line115;
     
            }
   
// Test on divergence
   line110: if((ksgn == (-1)) and (std::max(std::abs(result),std::abs(area)) <= (defabs * 0.1e-01))) {
     
                goto line130;
     
            }
   
   if((0.1e-01 > (result / area)) or ((result/area) > 0.1e+03) or (errsum > std::abs(area))){
     
       ier = 6;
     
   } 
   
   goto line130;
   
// Compute global integral sum
   line115: result = 0.0e+00;
   
   for(int k = 1; k <= last; k++){
     
       result = result + work.at((l2 - 1) + k - 1);
      
   }
   
   abserr = errsum;
   line130: if(ier > 2) ier = ier - 1;
   line140: neval = 42 * last - 21;
   
return;

}
