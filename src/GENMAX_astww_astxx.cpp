#include <base/base.hpp>
#include <utility/icheck.hpp>
#include <genmax/ast0xx.hpp>
#include <genmax/ast1xx.hpp>
#include <genmax/ast2xx.hpp>
#include <genmax/ast3xx.hpp>
#include <genmax/ast4xx.hpp>

//' genmax automatic start value routine for astart command 
//' finds start values for models 1/5 possibly with regression 
//' for location

void astxx(int &kmod,
           int &kdist,
           Rcpp::IntegerVector &intd,
           Rcpp::IntegerVector &nxd,
           Rcpp::List &ipxcd,
           Rcpp::IntegerVector &irelad,
           int &npard,
           Rcpp::NumericVector &theta,
           Rcpp::NumericVector &thetas,
           Rcpp::IntegerVector &kodet,
           Rcpp::IntegerVector &ifix,
           int &nparm,
           Rcpp::NumericMatrix &y,
           int &ncoly,
           int &nrownw,
           Rcpp::NumericMatrix &x,
           int &ncolx,
           Rcpp::IntegerVector &codes,
           Rcpp::IntegerVector &pcodes,
           Rcpp::IntegerVector &weight,
           Rcpp::IntegerVector &pweigh,
           Rcpp::NumericMatrix &ty,
           int &ncolty,
           Rcpp::IntegerVector &tc,
           int &kcentr,
           Rcpp::IntegerVector &iplab,
           int &maxit,
           Rcpp::NumericMatrix &vcvs,
           Rcpp::NumericMatrix &vcv,
           Rcpp::NumericMatrix &r,
           int &ier,
           int &nstart,
           int &maxmsd,
           double &tol,
           int &lsd,
           double &pchmax){
  
// Choose method according to model;
   icheck(kmod,0,4,5,5,ier,-8000);

if(kmod == 5) return;
  
if(kmod == 0) { // simple model

   // set pseudo weights to 0 if we have a dummy observation
   // move pseudo codes
      for(int i = 1; i <= nrownw; i++){

          pcodes.at(i - 1) = codes.at(i - 1);
          pweigh.at(i - 1) = weight.at(i - 1);

          if(pcodes.at(i - 1) > 0) continue;

          pweigh.at(i - 1) = 0;

      }

   ast0xx(kmod,kdist,intd,nxd,ipxcd,irelad,npard,
          theta,thetas,kodet,ifix,nparm,y,ncoly,nrownw,
          x,ncolx,pcodes,pweigh,ty,ncolty,tc,kcentr,iplab,
          maxit,vcvs,vcv,r,ier,
          nstart,maxmsd,tol,lsd,pchmax);

}

 if(kmod == 1) {  // lfp model
 
    ast1xx(kmod,kdist,intd,nxd,ipxcd,irelad,npard,
           theta,thetas,kodet,ifix,nparm,y,ncoly,nrownw,x,ncolx,
           codes,pcodes,weight,pweigh,ty,ncolty,tc,
           kcentr,iplab,maxit,vcvs,vcv,r,ier,
           nstart,maxmsd,tol,lsd,pchmax);
 
 }
 
 if(kmod == 2) { // doa model
 
    ast2xx(kmod,kdist,intd,nxd,ipxcd,irelad,npard,
           theta,thetas,kodet,ifix,nparm,y,ncoly,nrownw,
           x,ncolx,codes,pcodes,weight,pweigh,ty,ncolty,tc,
           kcentr,iplab,maxit,vcvs,vcv,r,ier,
           nstart,maxmsd,tol,lsd,pchmax);
   
 }
 
 if(kmod == 3) { // sts model
 
    ast3xx(kmod,kdist,intd,nxd,ipxcd,irelad,npard,theta,
           thetas,kodet,ifix,nparm,y,ncoly,nrownw,x,ncolx,
           codes,pcodes,weight,pweigh,ty,ncolty,tc,
           kcentr,iplab,maxit,vcvs,vcv,r,ier,
           nstart,maxmsd,tol,lsd,pchmax);
   
 }
 
 if(kmod == 4) { // parametric proportional hazards model
 
    ast4xx(kmod,kdist,intd,nxd,ipxcd,irelad,npard,
           theta,thetas,kodet,ifix,nparm,y,ncoly,nrownw,
           x,ncolx,codes,pcodes,weight,pweigh,ty,ncolty,tc,
           kcentr,iplab,maxit,vcvs,vcv,r,ier,
           nstart,maxmsd,tol,lsd,pchmax);
   
 }

return;

}
