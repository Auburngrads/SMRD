#include <base/base.hpp>
#include <gensiz/fdnprd.hpp>
//#include <genmax/dgends.hpp>
#include <genmax/genest.hpp>
//#include <genmax/outpar.hpp>

void fitww(int &kmod,
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
           Rcpp::NumericMatrix &ipy,
           int &ncoly,
           int &nrownw,
           Rcpp::NumericMatrix &ipx,
           int &ncolx,
           Rcpp::IntegerVector &ipcode,
           Rcpp::IntegerVector &ipweig,
           Rcpp::NumericMatrix &ipty,
           int &ncolty,
           Rcpp::IntegerVector &iptc,
           int &kcentr,
           Rcpp::IntegerVector &iplabp,
           int &ifit,
           double &pest,
           int &maxit,
           double &epsx,
           Rcpp::NumericVector &fstder,
           double &xlogl,
           Rcpp::NumericMatrix &iyhat,
           Rcpp::NumericMatrix &iresid,
           Rcpp::NumericMatrix &vcvs,
           Rcpp::NumericMatrix &vcv,
           Rcpp::NumericMatrix &r,
           Rcpp::NumericVector &start,
           int &lstar,
           int &maxpp,
           double &conlev,
           Rcpp::IntegerVector &ilabp,
           Rcpp::IntegerVector &ilabd,
           int &ier){
  
int kgtall = 1, nregr = 0;
int llog,kmodp,maxpd;
  
      fdnprd(kmod,kdist,intd,nxd,nregr,npard,
             kgtall,nparm,ilabp,ilabd,
             llog,kmodp,ier,maxpd);
  
      if(debug::kprint >= 4){
        
         // dgends(kmod,kdist,intd,nxd,nregr,ipxcd,irelad,npard,
         //        theta,thetas,kodet,ifix,nparm,ipy,ncoly,nrownw,
         //        ipx,ncolx,ipcode,ipweig,ipty,ncolty,iptc,kcentr,
         //        iplabp);
        
      }
      
      if(lstar == 1) theta = clone(start);
      lstar = 0;
      
      genest(kmod,kdist,intd,nxd,ipxcd,irelad,npard,
             theta,thetas,kodet,ifix,nparm,ipy,ncoly,
             nrownw,ipx,ncolx,ipcode,ipweig,ipty,ncolty,
             iptc,kcentr,iplabp,ifit,pest,maxit,
             epsx,fstder,xlogl,vcvs,vcv,r,iyhat,
             iresid,ier);
      
      if(debug::kprint >= 1){
        
         // outpar(ifit,kmod,kdist,xlogl,theta,kodet,
         //        iplabp,vcv,r,nparm,conlev,nparm);
        
      }
      
return;
      
}
