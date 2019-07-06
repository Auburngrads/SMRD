#include <base/base.hpp>
#include <sbq/beta2quan.hpp>

//' Wrapper for mapping beta to a quantile??? not clear why needed
// [[Rcpp::export]]
Rcpp::List SBQ(int ndist1,
               int ndist2,
               double stress,
               double alpha,
               double beta0,
               double beta1,
               double sigma,
               double ugamma,
               double sdgamma,
               double bd1,
               double bd2,
               double quan,
               int kprint){

debug::kprint = kprint;
  
 beta2quan(ndist1,ndist2,stress,alpha,beta0,
           beta1,sigma,ugamma,sdgamma,bd1,bd2,quan);
  
 return Rcpp::List::create(Named("ndist1") = ndist1,
                           Named("ndist2") = ndist2,
                           Named("stress") = stress,
                           Named("alpha") = alpha,
                           Named("beta0") = beta0,
                           Named("beta1") = beta1,
                           Named("sigma") = sigma,
                           Named("ugamma") = ugamma,
                           Named("sdgamma") = sdgamma,
                           Named("bd1") = bd1,
                           Named("bd2") = bd2,
                           Named("quan") = quan);
 
}

#include <base/base.hpp>
#include <sbq/diff.hpp>
#include <sbq/zeroin.hpp>
#include <sxcdf/rflcdf.hpp>
#include <sxcdf/xcdf.hpp>

using namespace passer5;

// Compute a quantile given the values of beta0, beta1, sigma, ugamma, sdgamma
// 
// @param ndist1 Integer codes for distribution (1=sev, 2=normal, 3=logistic)
// @param ndist2 Same as \code{ndist1}  
// @param beta0       intercept of mean log(time) model eqn
// @param beta1       slope of mean log(time) model eqn
// @param xstr        stress level
// @param sigma       std devn of log(time)
// @param ugamma      mean of the fatigue limits
// @param sdgamma     std devn of the fatigue limits
// @param stress      stress level
// @param alpha       proportion corresponding to quantile
// @param b1 lower bound for beta0
// @param b2 upper bound for beta0
// @param quan output quantile value
// @name beta2quan
// @return Quantile value \code{quan}
// @noRd
void beta2quan(int &ndist1,
               int &ndist2,
               double &stress,
               double &alpha,
               double &beta0,
               double &beta1,
               double &sigma,
               double &ugamma,
               double &sdgamma,
               double &bd1,
               double &bd2,
               double &quan){

double tol, top, bottom, xlog, z;
double check, check1 = 0.0e00,check2 = 0.0e00;
int ier;

// Constants that one might want to change to
// achieve a higher degree of accuracy from the algorithm
   tol = 1.0e-8;
   top = 1000.e00;
   bottom = -1000.e00;
   xlog = std::log(stress);

// Global namespace stuff
   passer5::g_beta0p = beta0;
   passer5::g_beta1p = beta1;
   passer5::g_sigmap = sigma;
   passer5::g_ugammap = ugamma;
   passer5::g_sdgammap = sdgamma;
   passer5::g_stressp = stress;
   passer5::g_alphap = alpha;
   passer5::g_ndist1p = ndist1;
   passer5::g_ndist2p = ndist2;
   bd1 = bd1 + 2.0e00;
   bd2 = bd2 - 2.0e00;

// Check if quantile is possible
   z = (xlog - ugamma) / sdgamma;
   check = rflcdf(z,ndist2);
   
   if(check > alpha) goto line200;
   
   Rcpp::warning("Quantile is not possible - value reset");
   quan = bottom;
   return;

// Check if bounds are okay
line200: bd1 = bd1 - 2.0e00;
         if(bd1 <= bottom) {
           
            quan = bottom;
            return;
            
         }
         
         xcdf(ndist1,ndist2,beta0,beta1,stress,sigma,ugamma,
              sdgamma,bd1,check1,ier);
         
         if(check1 > alpha) goto line200;
         
line201: bd2 = bd2 + 2.e00;
         if(bd2 >= top){
           
            quan = top;
            return;
           
         }
         
         xcdf(ndist1,ndist2,beta0,beta1,stress,sigma,ugamma,
              sdgamma,bd2,check2,ier);
         
         if(check2 < alpha) goto line201;

// Zero in on quantile value
   if(debug::kprint > 0){
     
      Rcpp::Rcout << "\nEnd of beta2quan\n" << std::endl;
      Rcpp::Rcout << "   bd1 = " << bd1     << std::endl;
      Rcpp::Rcout << "   bd2 = " << bd2     << std::endl;
      Rcpp::Rcout << "check1 = " << check1  << std::endl;
      Rcpp::Rcout << "check2 = " << check2  << std::endl;
     
   }      
   
   quan = zeroin(bd1,bd2,diff,tol);
   
   return;
   
}

#include <base/base.hpp>
#include <sxcdf/xcdf.hpp>

using namespace passer5;

// Function to compute difference between cdf and alpha
double diff(double x){

  double DIFF,answer = 0.0e00;
  int ier = 0;
  
  xcdf(passer5::g_ndist1p,passer5::g_ndist2p,
       passer5::g_beta0p,passer5::g_beta1p,
       passer5::g_stressp,passer5::g_sigmap,
       passer5::g_ugammap,passer5::g_sdgammap,
       x,answer,ier);
 
  DIFF = answer - passer5::g_alphap;
  
  return DIFF;
  
}

#include <base/base.hpp>
#include <utility/dmachine.hpp>

// A zero of the function \code{f(x)} is computed in the interval \code{ax,bx}.
// 
// @param ax     left endpoint of initial interval
// @param bx     right endpoint of initial interval
// @param f      function subprogram which evaluates \code{f(x)} for any x in the interval  ax,bx
// @param tol    desired length of the interval of uncertainty of the final result (> 0)
// @return zeroin abscissa approximating a zero of  f  in the interval ax,bx
// @name zeroin
// @details It is assumed that \code{f(ax)} and \code{f(bx)} have opposite signs
//          this is checked, and an error message is printed if this is not
//          satisfied. \code{zeroin}  returns a zero x in the given interval
//          ax,bx to within a tolerance \code{4 * macheps * abs(x) + tol}, where 
//          \code{macheps} is the relative machine precision defined as the smallest representable
//          number such that \code{1.0 + macheps > 1}.
//         
//         This function subprogram is a slightly  modified  translation  of
//         the algol 60 procedure  zero  given in  richard brent, algorithms for
//         minimization without derivatives, prentice-hall, inc. (1973).
//         Modified 29 sept to return 0 in the error condition to avoid compile warn
double zeroin(double ax,
              double bx,
              double (*f)(double),
              double tol){

double a,b,c,d,e,eps,fa,fb,fc,tol1,xm,p,q,r,s;
double zero_in;
  
eps = D1mach(4);
tol1 = eps + one;
a = ax;
b = bx;
fa = f(a);
fb = f(b);
        
// Check that f(ax) and f(bx) have different signs
   if((fa == zero) or (fb == zero)) goto line20;
   if((fa * (fb / std::abs(fb))) <= zero) goto line20;
   
   Rcpp::warning("f(ax) and f(bx) do not have different signs -- zeroin is aborting");
   zero_in = zero;
   return zero_in;
   
line20: c = a;
        fc = fa;
        d = b - a;
        e = d;
        
line30: if(std::abs(fc) >= std::abs(fb)) goto line40;
        a = b;
        b = c;
        c = a;
        fa = fb;
        fb = fc;
        fc = fa;
        
line40: tol1 = two * eps * std::abs(b) + half * tol;
        xm = half * (c - b);
        if((std::abs(xm) <= tol1) or(fb == zero)) goto line150;
        
// See if a bisection is forced
   if((std::abs(e) >= tol1) and (std::abs(fa) > std::abs(fb))){
     
       goto line50;
     
   } 
   d = xm;
   e = d;
   goto line110;
   
line50: s = fb / fa;
        if(a != c) goto line60;

// Linear interpolation
   p = two * xm * s;
   q = one - s;
   goto line70;

// Inverse quadratic interpolation
   line60: q = fa / fc;
           r = fb / fc;
           p = s * (two * xm * q * (q - r) - (b - a) * (r - one));
           q = (q - one) * (r - one) * (s - one);
           
   line70: if(p <= zero) goto line80;
           q = -q;
           goto line90;
           
   line80: p = -p;
     
   line90: s = e;
           e = d;
           if(((two * p) >= (3.0e0 * xm * q - std::abs(tol1 * q))) or (p >= std::abs(half * s * q))){
             
                goto line100;
             
           }
           d = p / q;
           goto line110;
           
   line100: d = xm;
            e = d;
            
   line110: a = b;
            fa = fb;
            if(std::abs(d) <= tol1) goto line120;
            b = b + d;
            goto line140;
            
   line120: if(xm <= zero) goto line130;
            b = b + tol1;
            goto line140;
            
   line130: b = b - tol1;
   
   line140: fb = f(b);
            if((fb * (fc / std::abs(fc))) > zero) goto line20;
            goto line30;
            
   line150: zero_in = b;
   
   return zero_in;
   
}