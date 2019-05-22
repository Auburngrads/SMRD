#include <base/base.hpp>
#include <utility/dmachine.hpp>
#include <sfteval/dqelg.hpp>
#include <sxcdf/dqk15i.hpp>
#include <sfteval/dqpsrt.hpp>

//**date written 800101 (yymmdd);
//**revision date 830518 (yymmdd);
//**category no. h2a3a1,h2a4a1;
//**keywords automatic integrator, infinite intervals,;
// general-purpose, transformation, extrapolation,;
// globally adaptive;
//**author piessens,robert,appl. math & progr. div - k.u.leuven;
// de doncker,elise,appl. math & progr. div - k.u.leuven;
//**purpose the routine calculates an approximation result to a given;
// integral i = integral of f over (bound,+infinity);
// or i = integral of f over (-infinity,bound);
// or i = integral of f over (-infinity,+infinity),;
// hopefully satisfying following claim for accuracy;
// abs(i-result) <= max(epsabs,epsrel*abs(i));
//**description;
//integration over infinite intervals;
//standard fortran subroutine;
// f - double precision;
// function subprogram defining the integrand;
// function f(x). the actual name for f needs to be;
// declared e x t e r n a l in the driver program.;
// bound - double precision;
// finite bound of integration range;
// (has no meaning if interval is doubly-infinite);
// inf - double precision;
// indicating the kind of integration range involved;
// inf = 1 corresponds to (bound,+infinity),;
// inf = -1 to (-infinity,bound),;
// inf = 2 to (-infinity,+infinity).;
// epsabs - double precision;
// absolute accuracy requested;
// epsrel - double precision;
// relative accuracy requested;
// if epsabs <= 0;
// and epsrel < max(50*rel.mach.acc.,0.5d-28),;
// the routine will end with ier = 6.;
// limit - integer;
// gives an upper bound on the number of subintervals;
// in the partition of (a,b), limit >= 1;
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
// - ier > 0 abnormal termination of the routine. the;
// estimates for result and error are less;
// reliable. it is assumed that the requested;
// accuracy has not been achieved.;
// error messages;
// ier = 1 maximum number of subdivisions allowed;
// has been achieved. one can allow more;
// subdivisions by increasing the value of;
// limit (and taking the according dimension;
// adjustments into account). however,if;
// this yields no improvement it is advised;
// to analyze the integrand in order to;
// determine the integration difficulties.;
// if the position of a local difficulty can;
// be determined (e.g. singularity,;
// discontinuity within the interval) one;
// will probably gain from splitting up the;
// interval at this point and calling the;
// integrator on the subranges. if possible,;
// an appropriate special-purpose integrator;
// should be used, which is designed for;
// handling the type of difficulty involved.;
// = 2 the occurrence of roundoff error is;
// detected, which prevents the requested;
// tolerance from being achieved.;
// the error may be under-estimated.;
// = 3 extremely bad integrand behaviour occurs;
// at some points of the integration;
// interval.;
// = 4 the algorithm does not converge.;
// roundoff error is detected in the;
// extrapolation table.;
// it is assumed that the requested tolerance;
// cannot be achieved, and that the returned;
// result is the best which can be obtained.;
// = 5 the integral is probably divergent, or;
// slowly convergent. it must be noted that;
// divergence can occur with any other value;
// of ier.;
// = 6 the input is invalid, because;
// (epsabs <= 0 and;
// epsrel < max(50*rel.mach.acc.,0.5d-28),;
// result, abserr, neval, last, rlist(1),;
// elist(1) and iord(1) are set to zero.;
// alist(1) and blist(1) are set to 0;
// and 1 respectively.;
// alist - double precision;
// vector of dimension at least limit, the first;
// last elements of which are the left;
// end points of the subintervals in the partition;
// of the transformed integration range (0,1).;
// blist - double precision;
// vector of dimension at least limit, the first;
// last elements of which are the right;
// end points of the subintervals in the partition;
// of the transformed integration range (0,1).;
// rlist - double precision;
// vector of dimension at least limit, the first;
// last elements of which are the integral;
// approximations on the subintervals;
// elist - double precision;
// vector of dimension at least limit, the first;
// last elements of which are the moduli of the;
// absolute error estimates on the subintervals;
// iord - integer;
// vector of dimension limit, the first k;
// elements of which are pointers to the;
// error estimates over the subintervals,;
// such that elist(iord(1)), ..., elist(iord(k));
// form a decreasing sequence, with k = last;
// if last <= (limit/2+2), and k = limit+1-last;
// otherwise;
// last - integer;
// number of subintervals actually produced;
// in the subdivision process;
//**references (none);
//**routines called d1mach,dqelg,dqk15i,dqpsrt;

void dqagie(double (*f)(double),
            double &bound,
            int &inf,
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

double abseps,area,area1,area12,area2,a1,a2,a15;
double boun,b1,b2,b15,correc,defabs,defab1,defab2;
double dres,epmach,erlarg = 0.0e00,erlast,ertest = 0.0e00;
double errbnd,errmax,error1,error2,erro12,errsum;
double oflow,resabs,reseps,small = 0.0e00,uflow;
int id,ierro,iroff1,iroff2,iroff3,jupbnd,ksgn;
int ktmin,maxerr,nres,nrmax,numrl2;
bool extrap,noext;

Rcpp::NumericVector rlist2(52), res3la(3);

// The dimension of rlist2 is determined by the value of limexp in subroutine dqelg

// List of major variables
// ----------------------------------------------------
// alist - list of left end points of all subintervals
// considered up to now
// blist - list of right end points of all subintervals
// considered up to now
// rlist(i) - approximation to the integral over
// (alist(i),blist(i))
// rlist2 - array of dimension at least (limexp+2),
// containing the part of the epsilon table
// wich is still needed for further computations
// elist(i) - error estimate applying to rlist(i)
// maxerr - pointer to the interval with largest error estimate
// errmax - elist(maxerr)
// erlast - error on the interval currently subdivided
// (before that subdivision has taken place)
// area - sum of the integrals over the subintervals
// errsum - sum of the errors over the subintervals
// errbnd - requested accuracy max(epsabs,epsrel* abs(result))
// *****1 - variable for the left subinterval
// *****2 - variable for the right subinterval
// last - index for subdivision
// nres - number of calls to the extrapolation routine
// numrl2 - number of elements currently in rlist2. if an
// appropriate approximation to the compounded
// integral has been obtained, it is put in
// rlist2(numrl2) after numrl2 has been increased by one.
// small - length of the smallest interval considered up to now, multiplied by 1.5
// erlarg - sum of the errors over the intervals larger
// than the smallest interval considered up to now
// extrap - logical variable denoting that the routine
// is attempting to perform extrapolation. i.e.
// before subdividing the smallest interval we
// try to decrease the value of erlarg.
// noext - logical variable denoting that extrapolation
// is no longer allowed (true-value)

// Machine dependent constants
// -----------------------------------------------------
// epmach is the largest relative spacing.
// uflow is the smallest positive magnitude.
// oflow is the largest positive magnitude.
   epmach = D1mach(4);
   
// Test on validity of parameters
   ier = 0;
   neval = 0;
   last = 0;
   result = 0.0e+00;
   abserr = 0.0e+00;
   work.at(0) = 0.0e+00;
   work.at((l1 - 1) + 0) = 0.1e+01;
   work.at((l2 - 1) + 0) = 0.0e+00;
   work.at((l3 - 1) + 0) = 0.0e+00;
   iord.at(0) = 0;
   if(debug::kprint > 0){
     
      Rcpp::Rcout << "\nDQAGIE TEST 1\n" << std::endl;
      Rcpp::Rcout << "epmach = "             << ier    << std::endl;
      Rcpp::Rcout << "epsabs = "             << epsabs << std::endl;
      Rcpp::Rcout << "epsrel = "             << epsrel << std::endl;
      Rcpp::Rcout << "(epsabs <= 0.0e00) = " << (epsabs <= 0.0e00)    << std::endl;
      Rcpp::Rcout << "(epsrel < std::max(0.5e02 * epmach,0.5e28)) = " << (epsrel < std::max(0.5e+02 * epmach,0.5e-28)) << std::endl;
      
   }
   if((epsabs <= 0.0e00) and (epsrel < std::max(0.5e02 * epmach,0.5e-28))){
     
       ier = 6;
     
   }
   
   if(ier == 6) return;

// First approximation to the integral
// -----------------------------------;
// determine the interval to be mapped onto (0,1).;
// if inf = 2 the integral is computed as i = i1+i2, where;
// i1 = integral of f over (-infinity,0),;
// i2 = integral of f over (0,+infinity).;
   boun = bound;
   if(inf == 2) boun = 0.0e00;
   a15 = 0.0e00;
   b15 = 0.1e01;
   dqk15i(f,boun,inf,a15,b15,result,abserr,defabs,resabs);
   
   if(debug::kprint >= 4){
      
      Rcpp::Rcout << "\nDQAGIE AFTER DQK15I **1**\n" << std::endl;
      Rcpp::Rcout << "boun   = " << boun << std::endl;
      Rcpp::Rcout << "a15    = " << a15   << std::endl;
      Rcpp::Rcout << "b15    = " << b15 << std::endl;
      Rcpp::Rcout << "defabs = " << defabs << std::endl;
      Rcpp::Rcout << "result = " << result << std::endl;
      Rcpp::Rcout << "abserr = " << abserr << std::endl;
      Rcpp::Rcout << "resabs = " << resabs << std::endl;
      
   }

   
// Test on accuracy
   last = 1;
   work.at((l2 - 1) + 0) = result;
   work.at((l3 - 1) + 0) = abserr;
   iord.at(0) = 1;
   dres = std::abs(result);
   errbnd = std::max(epsabs,epsrel * dres);
   
   if(debug::kprint > 0){
     
      Rcpp::Rcout << "\nDQAGIE TEST 2\n" << std::endl;
      Rcpp::Rcout << "epmach = "                << ier << std::endl;
      Rcpp::Rcout << "abserr = "             << abserr << std::endl;
      Rcpp::Rcout << "defabs = "             << defabs << std::endl;
      Rcpp::Rcout << "errbnd = "             << errbnd << std::endl;
      Rcpp::Rcout << "(abserr <= (1.0e02 * epmach * defabs)) = " << (abserr <= (1.0e+02 * epmach * defabs)) << std::endl;
      Rcpp::Rcout << "(abserr > errbnd) = " << (abserr > errbnd) << std::endl;
      
   }
   if((abserr <= (1.0e02 * epmach * defabs)) and (abserr > errbnd)){
     
       ier = 2;
     
   }

   if(limit == 1) ier = 1;
   
      if(debug::kprint > 0){
     
         Rcpp::Rcout << "\nDQAGIE TEST 3\n" << std::endl;
         Rcpp::Rcout << "ier = "                << ier << std::endl;
         Rcpp::Rcout << "abserr = "             << abserr << std::endl;
         Rcpp::Rcout << "resabs = "             << resabs << std::endl;
         Rcpp::Rcout << "errbnd = "             << errbnd << std::endl;
         Rcpp::Rcout << "(abserr != resabs) = " << (abserr != resabs) << std::endl;
         Rcpp::Rcout << "(abserr <= errbnd) = " << (abserr <= errbnd) << std::endl;
      
      }
   if((ier != 0) or ((abserr <= errbnd) and (abserr != resabs)) or (abserr == 0.0e+00)){
     
       goto line130;
     
   }
   
// Initialization
   uflow = D1mach(1);
   oflow = D1mach(2);
   rlist2.at(0) = result;
   errmax = abserr;
   maxerr = 1;
   area = result;
   errsum = abserr;
   abserr = oflow;
   nrmax = 1;
   nres = 0;
   ktmin = 0;
   numrl2 = 2;
   extrap = false;
   noext = false;
   ierro = 0;
   iroff1 = 0;
   iroff2 = 0;
   iroff3 = 0;
   ksgn = -1;
   if(dres >= ((0.1e+01 - 0.5e+02 * epmach) * defabs)) ksgn = 1;
   
// Main do-loop
   for(last = 2; last <= limit; last++){
   
   // Bisect the subinterval with nrmax-th largest error estimate
      a1 = work.at(maxerr - 1);
      b1 = 0.5e+00 * (work.at(maxerr - 1) + work.at((l1 - 1) + maxerr - 1));
      a2 = b1;
      b2 = work.at((l1 - 1) + maxerr - 1);
      erlast = errmax;
      
      dqk15i(f,boun,inf,a1,b1,area1,error1,resabs,defab1);
      if(debug::kprint >= 4){
      
         Rcpp::Rcout << "\nDQAGIE AFTER DQK15I **2**\n" << std::endl;
         Rcpp::Rcout << "last   = " << last   << std::endl; 
         Rcpp::Rcout << "boun   = " << boun   << std::endl;
         Rcpp::Rcout << "a1     = " << a1     << std::endl;
         Rcpp::Rcout << "b1     = " << b1     << std::endl;
         Rcpp::Rcout << "defab1 = " << defab1 << std::endl;
         Rcpp::Rcout << "area1  = " << area1  << std::endl;
         Rcpp::Rcout << "error1 = " << error2 << std::endl;
         Rcpp::Rcout << "resabs = " << resabs << std::endl;
      
      }

      dqk15i(f,boun,inf,a2,b2,area2,error2,resabs,defab2);
      if(debug::kprint >= 4){
      
         Rcpp::Rcout << "\nDQAGIE AFTER DQK15I **3**\n" << std::endl;
         Rcpp::Rcout << "last   = " << last   << std::endl; 
         Rcpp::Rcout << "boun   = " << boun   << std::endl;
         Rcpp::Rcout << "a2     = " << a2     << std::endl;
         Rcpp::Rcout << "b2     = " << b2     << std::endl;
         Rcpp::Rcout << "defab2 = " << defab2 << std::endl;
         Rcpp::Rcout << "area2  = " << area2  << std::endl;
         Rcpp::Rcout << "error2 = " << error2 << std::endl;
         Rcpp::Rcout << "resabs = " << resabs << std::endl;
      
      }
      
   // Improve previous approximations to integral and error and test for accuracy
      area12 = area1 + area2;
      erro12 = error1 + error2;
      errsum = errsum + erro12 - errmax;
      area = area + area12 - work.at((l2 - 1) + maxerr - 1);
      if((defab1 == error1) or (defab2 == error2)) goto line15;
      if((std::abs(work.at((l2 - 1) + maxerr - 1) - area12) > (0.1e-04 * std::abs(area12))) or (erro12 < (0.99e+00 * errmax))){
        
          goto line10;
        
      }
      
      if(extrap) iroff2 = iroff2 + 1;
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
      
   // Set error flag in the case of bad integrand behaviour
   // at some points of the integration range.
      if(std::max(std::abs(a1),std::abs(b2)) <= ((0.1e+01 + 0.1e+03 * epmach) * (std::abs(a2) + 0.1e+04 * uflow))){
        
         ier = 4;
        
      }
      
   // Append the newly-created intervals to the list
      if(debug::kprint > 4){
         
         Rcpp::Rcout << "\nDQAGIE: work check\n" << std::endl;
         Rcpp::Rcout << "last   = " << last   << std::endl;
         Rcpp::Rcout << "maxerr = " << maxerr << std::endl;
         Rcpp::Rcout << "l1 = " << l1 << std::endl;
         Rcpp::Rcout << "l2 = " << l2 << std::endl;
         Rcpp::Rcout << "l3 = " << l3 << std::endl;
         
      }
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
              
   // Call dqpsrt to maintain the descending ordering
   // in the list of error estimates and select the subinterval
   // with nrmax-th largest error estimate (to be bisected next)
      line30: dqpsrt(limit,last,maxerr,errmax,work,l3,iord,nrmax);
      
              if(errsum <= errbnd) goto line115;
              if(ier != 0) goto line100;
              if(last == 2) goto line80;
              if(noext) continue;
              erlarg = erlarg - erlast;
              if(std::abs(b1 - a1) > small) erlarg = erlarg + erro12;
              if(extrap) goto line40;
              
   // Test whether the interval to be bisected next is the smallest interval
      if(std::abs(work.at((l1 - 1) + maxerr - 1) - work.at(maxerr - 1)) > small) continue;
      extrap = true;
      nrmax = 2;
      
      line40: if((ierro == 3) or (erlarg <= ertest)) goto line60;
      
   // The smallest interval has the largest error.
   // Before bisecting decrease the sum of the errors over the
   // larger intervals (erlarg) and perform extrapolation
      id = nrmax;
      jupbnd = last;
      if(last > (2 + std::floor(limit / 2))) jupbnd = limit + 3 - last;
      
      for(int k = id; k <= jupbnd; k++){
        
          maxerr = iord.at(nrmax - 1);
          errmax = work.at((l3 - 1) + maxerr - 1);
          if(std::abs(work.at((l1 - 1) + maxerr - 1) - work.at(maxerr - 1)) > small) continue;
          nrmax = nrmax + 1;
      
      }
      
   // Perform extrapolation
      line60: numrl2 = numrl2 + 1;
              rlist2.at(numrl2 - 1) = area;
              
              dqelg(numrl2,rlist2,reseps,abseps,res3la,nres);
              if(debug::kprint >= 4){
      
                 Rcpp::Rcout << "\nDQAGIE AFTER DQELG\n" << std::endl;
                 Rcpp::Rcout << "last   = " << last   << std::endl; 
                 Rcpp::Rcout << "numrl2 = " << numrl2 << std::endl;
                 Rcpp::Rcout << "rlist2 = " << rlist2 << std::endl;
                 Rcpp::Rcout << "abseps = " << abseps << std::endl;
                 Rcpp::Rcout << "res3la = " << res3la << std::endl;
                 Rcpp::Rcout << "reseps = " << reseps << std::endl;
      
             }

              
              ktmin = ktmin + 1;
              if((ktmin > 5) and (abserr < (0.1e-02 * errsum))) ier = 5;
              if(abseps >= abserr) goto line70;
              ktmin = 0;
              abserr = abseps;
              result = reseps;
              correc = erlarg;
              ertest = std::max(epsabs,epsrel * std::abs(reseps));
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
              continue;
              
      line80: small = 0.375e+00;
              erlarg = errsum;
              ertest = errbnd;
              rlist2.at(1) = area;
      
}
Rcpp::Rcout << "\nHere\n" << std::endl;
// Set final result and error estimate
   line100: if(abserr == oflow) goto line115;
            if((ier + ierro) == 0) goto line110;
            if(ierro == 3) abserr = abserr + correc;
            if(ier == 0) ier = 3;
            if((result != 0.0e+00) and (area != 0.0e+00)) goto line105;
            if(abserr > errsum) goto line115;
            if(area == 0.0e+00) goto line130;
            goto line110;
            
   line105: if(abserr/std::abs(result) > errsum/std::abs(area)) goto line115;
   
// Test on divergence
   line110: if((ksgn == (-1)) and (std::max(std::abs(result),std::abs(area)) <= (defabs * 0.1e-01))){
     
                goto line130;
     
            }
                
            if((0.1e-01 > (result / area)) or ((result / area) > 0.1e+03) or (errsum > std::abs(area))){
             
                ier = 6;
             
            }
                
           goto line130;
                
// Compute global integral sum
   line115: result = 0.0e+00;
   
            for(int k = 1; k <= last; k++){
              
                result = result + work.at((l2 - 1) + k - 1);
              
            }
            
            abserr = errsum;
            
   line130: neval = 30 * last - 15;
            if(inf == 2) neval = 2 * neval;
            if(ier > 2) ier = ier - 1;
            
return;

}
