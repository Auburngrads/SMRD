#include <base/base.hpp>
#include <gensiz/fdnprd.hpp>
#include <genmax/setup.hpp>
#include <genmax/astxx.hpp>
#include <genmax/unscpx.hpp>

//' Genmax automatic start value routine for astart command 
//' finds start values for models 1/5 possibly with regression 
//' for location

void astww(int &kmod,
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
           Rcpp::NumericMatrix &ipy, // genmax_g::ipynew
           int &ncoly,
           int &nrownw,
           Rcpp::NumericMatrix &ipx,    // genmax_g::ipxnew
           int &ncolx, 
           Rcpp::IntegerVector &ipcode, // genmax_g::ipcode
           Rcpp::IntegerVector &ipweig, // genmax_g::ipweig
           Rcpp::NumericMatrix &ipty,   // genmax_g::iptynw
           int &ncolty,
           Rcpp::IntegerVector &iptc,   // genmax_g::iptc 
           int &kcentr,
           Rcpp::IntegerVector &ipplab, // genmax_g::iplabp
           double &pest,
           int &maxit,
           Rcpp::NumericMatrix &vcvs,
           Rcpp::NumericMatrix &vcv,
           Rcpp::NumericMatrix &r,
           int &maxpp,
           Rcpp::IntegerVector &ilabp,
           Rcpp::IntegerVector &ilabd,
           int &ier,
           int &kgtall,
           int &nregr,
           int &llog,
           int &kmodp,
           int &maxpd,
           double &pfail,
           int &kmccde,
           int &nstart,
           int &maxmsd,
           double &tol,
           int &lsd,
           double &pchmax){

// Call setup to center and standardize data
// set likelihood communication variables below
// transform theta values for standardized and centered variables

// set up parameter vector;
   fdnprd(kmod,kdist,intd,nxd,nregr,
          npard,kgtall,nparm,ilabp,
          ilabd,llog,kmodp,ier,maxpd);
  
   setup(kmod,kdist,intd,nxd,ipxcd,irelad,npard,
         theta,thetas,kodet,ifix,nparm,ipy,ncoly,
         nrownw,ipx,ncolx,ipcode,ipweig,ipty,ncolty,
         iptc,kcentr,ipplab,kmodp,pfail,kmccde,llog,
         nregr,ier);
  
for(int i = 1; i <= nparm; i++){

    kodet.at(i - 1) = 1;
    if(ifix.at(i - 1) > 0) kodet.at(i - 1) = 0;

}

// Grab space for pseudo codes and weights
   Rcpp::IntegerVector iipcod = Rcpp::IntegerVector(nrownw);
   Rcpp::IntegerVector iipwei = Rcpp::IntegerVector(nrownw);

   astxx(kmod,kdist,intd,nxd,ipxcd,irelad,npard,
         theta,thetas,kodet,ifix,nparm,ipy,ncoly,
         nrownw,ipx,ncolx,ipcode,iipcod,
         ipweig,iipwei,ipty,ncolty,iptc,
         kcentr,ipplab,maxit,vcvs,vcv,r,ier,
         nstart,maxmsd,tol,lsd,pchmax);

   if(kcentr != 2) {
     
      unscpx(thetas,theta);
     
   }

     return;
}
