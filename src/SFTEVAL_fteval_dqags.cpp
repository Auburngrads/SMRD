#include <base/base.hpp>
#include <sfteval/dqagse.hpp>

//' Calculates an approximate result to a given definite integral
//'
//' @keyword automatic-integrator, general-purpose,
//'          (end-point) singularities, extrapolation,
//'          globally adaptive
//' @author Robert Piessens, Applied Math & Programming
//'         Division - K.U. Leuven
//'         Elise De Doncker, Applied Math & Programming
//'         Division - K.U. Leuven
//' @description Computation of a double precision
//'              definite integral of the function
//'              f over (a,b). Hopefully meeting
//'              accuracy:
//'              \code{abs(i-result) <= max(epsabs,epsrel*abs(i))}.
//' @param f \code{numeric} Function defining the
//'        integrand function f(x). The actual name
//'        for f needs to be declared in the driver
//'        program.
//' @param a \code{numeric} Lower limit of integration
//' @param b \code{numeric} Upper limit of integration
//' @param epsabs \code{numeric} Absolute accuracy requested
//' @param epsrel \code{numeric} Relative accuracy requested
//' @param result \code{numeric} Approximation to the integral
//' @param abserr \code{numeric} Estimate of the modulus of the
//'               absolute error, which should equal or exceed
//'               \code{abs(i-result)}
//' @param neval \code{integer} Number of integrand evaluations
//' @param ier \code{integer} Error message code (see Details)
//' @param limit
//'   dimensioning parameters;
//' @param limit \code{integer} Dimensioning parameter for iwork
//'  limit determines the maximum number of subintervals
//'  in the partition of the given integration interval
//'  (a,b), limit >= 1. if limit < 1, the routine will end with
//'  \code{ier = 6}.
//' @return \code{result, abserr, neval, ier}
//' @param lenw \code{integer} Dimensioning parameter for work
//'  lenw must be at least limit*4. If lenw < limit*4, the
//'  routine will end with ier = 6.;
//' @param last  On return, last equals the number
//'        of subintervals produced in the subdivision process,
//'        detemines the number of significant elements actually
//'        in the work arrays.
//'  work arrays;
//' @param iwork \code{integer} Vector of dimension at least
//'       limit, the first k elements of which contain pointers
//'       to the error estimates over the subintervals such that
//'       work(limit*3+iwork(1)),...,work(limit*3+iwork(k)) form
//'       a decreasing sequence, with k = last if last <= (limit/2+2),
//'       and k = limit+1-last otherwise
//' @param work \code{numeric} vector of dimension at least lenw.
//'        On return, work(1),...,work(last) contain the left
//'        end-points of the subintervals in the partition of (a,b),
//'        work(limit+1),...,work(limit+last) contain the right
//'        end-points, work(limit*2+1),...,work(limit*2+last) contain
//'        the integral approximations over the subintervals,
//'        work(limit*3+1),...,work(limit*3+last); contain the error
//'        estimates.
//'
//' @details If \code{epsabs <= 0} and
//'          \code{epsrel < max(50*rel.mach.acc.,0.5d-28)},
//'          the routine will return \code{ier = 6}.
//'
//'          ier = 0 implies normal and reliable termination of the
//'          routine where it is assumed that the requested accuracy
//'          has been achieved.
//'
//'          ier > 0 implies an abnormal termination of the routine.
//'          The estimates for integral and error are less reliable.
//'          It is assumed that the requested accuracy has not been
//'          achieved. For more see Section ier error messages.
//'
//'          Routines called: dqagse,xerror.
//'
//'          Date written 1 Jan 1980, Revision date 18 May 1983.
//'          Category no. h2a1a1.
//'
//'  @section ier error messages:
//'  ier = 1 implies that the maximum number of subdivisions
//'  allowed has been achieved. One can allow more subdivisions
//'  by increasing the value of limit (and taking the according
//'  dimension adjustments into account). However, if this yields
//'  no improvement it is advised to analyze the integrand to
//'  determine the integration difficulties. If the position of
//'  a local difficulty can be determined (e.g. singularity,
//'  discontinuity within the interval) one will probably gain
//'  from splitting up the interval at this point and calling
//'  the integrator on the subranges. If possible, an appropriate
//'  special-purpose integrator should be used, which is designed
//'  for handling the type of difficulty involved.
//'
//'  ier = 2 implies that the occurrence of roundoff error is
//'  detected, which prevents the requested tolerance from being
//'  achieved.  The error may be under-estimated.
//'
//'  ier = 3 implies that an extremely bad integrand behaviour
//'  occurs at some points of the integration interval.
//'
//'  ier = 4 implies that the algorithm does not converge.
//'  Roundoff error is detected in the extrapolation table.
//'  It is presumed that the requested tolerance cannot be
//'  achieved, and that the returned result is the best which
//'  can be obtained.
//'
//'  ier = 5 implies that the integral is probably divergent,
//'  or slowly convergent. It must be noted that divergence
//'  can occur with any other value of ier.
//'
//'  ier = 6 the input is invalid, because either
//'  \code{(epsabs <= 0 and epsrel < max(50*rel.mach.acc.,0.5d-28)}
//'  or \code{limit < 1} or \code{lenw < limit*4}.
//'  The values result, abserr, neval, last are set to
//'  zero - except when limit or lenw is invalid,
//'  iwork(1), work(limit*2+1) and  work(limit*3+1) are set to zero,
//'  work(1)is set to a and work(limit+1) to b.

void dqags(double (*f)(double),
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
  
int l1,l2,l3;

// Check validity of limit and lenw
   ier = 6;
   neval = 0;
   last = 0;
   result = 0.0e+00;
   abserr = 0.0e+00;

if((limit < 1) or (lenw < (limit * 4))) goto line10;

l1 = limit + 1;
l2 = limit + l1;
l3 = limit + l2;


// Prepare call for dqagse
   dqagse(f,a,b,epsabs,epsrel,limit,result,abserr,neval,
          ier,work,l1,l2,l3,iwork,last);

// Call error handler if necessary
   line10: if(ier != 0){
        
              Rcpp::stop("\nAbnormal return from dqags -- ier = %i",ier);
                 
           } 


return;
   
}
