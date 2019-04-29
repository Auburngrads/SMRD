#include <base/base.hpp>
#include <genmax/estim1.hpp>

void estim(Rcpp::NumericVector &thetat,
           Rcpp::IntegerVector &kodet,
           int &nparm,
           int &nparmm,
           double &tol,
           double &stepmx,
           int &maxit,
           double &xlogl,
           double (*func)(Rcpp::NumericVector, int),
           int &iprpow,
           Rcpp::IntegerVector &ilabp,
           int &ier){
   
int icon = 1;
   
Rcpp::NumericVector ie = Rcpp::NumericVector(nparm);
Rcpp::NumericVector ithetm = Rcpp::NumericVector(nparm);
Rcpp::NumericVector iwork = Rcpp::NumericVector(nparm * nparm + 4 * nparm);

estim1(thetat,ithetm,kodet,nparm,nparmm,ilabp,tol,
       stepmx,icon,maxit,xlogl,func,ie,iwork,iprpow,ier);

return;
   
}

#include <base/base.hpp>
#include <genmax/vecsqu.hpp>
#include <genmax/vecexp.hpp>
#include <genmax/powelm.hpp>
#include <genmax/gpowp.hpp>

//' perform basic estimation once the general setup has been performed
//' modified   11 may 1987 to get error code from powell so that
//' we can control the propogation of bad start values when convergence fails

void estim1(Rcpp::NumericVector &thetat,
            Rcpp::NumericVector &thetam,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            int &nparmm,
            Rcpp::IntegerVector &ilabp,
            double &tol,
            double &stepmx,
            int &icon,
            int &maxit,
            double &xlogl,
            double (*func)(Rcpp::NumericVector, int),
            Rcpp::NumericVector &e,
            Rcpp::NumericVector &work,
            int iprpow,
            int &ier){
   
double escale;

// Squish the parameter vector to eliminate fixed parameters
   vecsqu(thetat,kodet,nparm,thetam,nparmm);
   ier = 0;
   if(nparmm == 0) goto line72;
   
   gpowp(e,escale,stepmx,tol,nparm);
   ier = 0;
   
   if(iprpow >= 5){
      
      Rcpp::Rcout << "\nESTIM1 BEFORE POWELM\n" << std::endl;
   }
   
   powelm(thetam,e,nparmm,xlogl,escale,iprpow,icon,maxit,func,work,ier);
   xlogl = -1 * xlogl;
   goto line73;
   
line72: xlogl = -1 * func(thetam,nparmm);

// Bring back the fixed values to their original places
   line73: vecexp(thetam,kodet,nparm,thetat,nparmm);
   
return;
      
}