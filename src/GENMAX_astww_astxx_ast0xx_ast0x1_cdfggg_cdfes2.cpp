#include <base/base.hpp>
#include <wqmcdfest/wqm_cdfckd.hpp> // done
#include <wqmcdfest/wqm_cdfstr.hpp>
#include <wqmcdfest/wqm_cdftru.hpp>
//#include "heads/wqm_cdfest/wqm_cdfgra.h"
#include <wqmcdfest/wqm_cdfgkm.hpp>
#include <wqmcdfest/wqm_cdfema.hpp>
#include <wqmcdfest/wqm_cdfesi.hpp>
#include <wqmcdfest/wqm_cdfegr.hpp>
#include <genmax/cdfxlk.hpp>

//' @description Basic routine for computing the nonparametric maximum 
//' likelihood estimate of a cumulative distribution function called 
//' only by wqm_cdfest and cdfggg
//' 
//' @details 
//'  Big values used:
//'  \describe{
//'    \item{big32}{(1.0e32) The truncation infinity endpoint}
//'    \item{big30}{(1.0e30) The censoring infinity endpoint}
//'    \item{big29}{(1.0e29) The p-q infinity endpoint}
//'    \item{big31}{(1.0e31) Mark dummy obs in y(i) and y(ip)}
//'  
//'  }

void cdfes2(Rcpp::NumericMatrix &y,
            int &ny,
            Rcpp::IntegerVector &codes,
            Rcpp::IntegerVector &weight,
            Rcpp::NumericMatrix &ty,
            int &nty,
            Rcpp::IntegerVector &tcodes,
            int &n,
            int &nstart,
            int &maxit,
            double &tol,
            int &maxmsd,
            Rcpp::NumericVector &xlcen,
            Rcpp::NumericVector &xrcen,
            Rcpp::NumericVector &fail,
            Rcpp::NumericVector &xltru,
            Rcpp::NumericVector &xrtru,
            Rcpp::NumericVector &ys,
            Rcpp::NumericVector &p,
            Rcpp::NumericVector &q,
            Rcpp::NumericVector &pgrad,
            Rcpp::NumericVector &prob,
            Rcpp::NumericVector &sd,
            Rcpp::NumericVector &s,
            Rcpp::NumericVector &probd,
            int &m,
            double &xllgkm,
            Rcpp::IntegerVector &ilcv,
            Rcpp::IntegerVector &iucv,
            Rcpp::IntegerVector &iltv,
            Rcpp::IntegerVector &iutv,
            Rcpp::IntegerVector &iorder,
            double &pchmax,
            int &lsd,
            int &ier){
  
int ipndx,iqndx,ippndx,icode,ms = 0;
lsd = 1;
pchmax = 0.0e00;

// Check input:
//  This was originally needed for S-PLUS version compatability
// 
// Keep DEC Fortran happy, the kprint argument is not included in the
// definition of wqm_cdfckd in wqm_cdfest.f (in slocal/censorreg)
// call wqm_cdfckd(y,ny,ty,nty,codes,weight,tcodes,n,tol,kprint,ier)

 wqm_cdfckd(y,ny,ty,nty,codes,weight,tcodes,n,tol,ier);
 
 if(ier > 0) return;
 
// find the reduced set of intervals and data structure;
   wqm_cdfstr(y,ny,ys,ty,nty,tcodes,
              iorder,ilcv,iucv,codes,weight,n,p,q,m);
 
 if(debug::kprint >= 3) {
   
    Rcpp::Rcout << "\nCDFES2**3**\n" << std::endl;
    Rcpp::Rcout << "m = " << m << std::endl;
    Rcpp::Rcout << "nstart = " << nstart << std::endl;
   
 }
 
if(m == 0) {  // m = 0 implies some bad data
   
   ier = 14;
   return;
  
}

if((nstart > 0) and (nstart != m)) { // wrong number of start values
  
    ier = 15;
    return;
  
}

// find the truncation indices
 wqm_cdftru(ys,ty,nty,tcodes,n,p,q,m,iltv,iutv,ier);

 if(debug::kprint >= 4){
   
    Rcpp::Rcout << "\nCDFES2**STRUCTURE**\n" << std::endl;
    Rcpp::Rcout << "y = \n" << y << std::endl;
    Rcpp::Rcout << "codes = "  << codes << std::endl;
    Rcpp::Rcout << "weight = " << weight << std::endl;
    Rcpp::Rcout << "ilcv = " << ilcv << std::endl;
    Rcpp::Rcout << "iucv = " << iucv << std::endl;
    Rcpp::Rcout << "iltv = " << iltv << std::endl;
    Rcpp::Rcout << "iutv = " << iutv << std::endl;
   
 }

// if ier > 0, an error was detected in the truncation codes
if(ier > 0) return;

// call the graphic routine;
// wqm_cdfgra(iltv,iutv,ilcv,iucv,m,n,nty,weight);

// skip generalized kaplan meier if starting values were sent down
if(nstart <= 0) {
  
//  goto linetry the generalized kaplan-meier;
 wqm_cdfgkm(weight,ilcv,iucv,iltv,iutv,fail,xrcen,xlcen,
            xltru,xrtru,prob,sd,n,m,nty,ier);
  
// if ier = 0, we have a good solution, so skip to cleanup
if(ier == 0) goto line98;

// #if ier>0, we have a data inconsistancy or other serious error;
if(ier > 0) return;
// #if ier<1, we can try trunbull's algorithm;

}

 wqm_cdfema(weight,codes,ilcv,iucv,iltv,iutv,pgrad,
            prob,s,probd,n,m,nty,nstart,maxit,tol,pchmax);

// #call the information matrix standard error routine;
 wqm_cdfesi(weight,ilcv,iucv,iltv,iutv,probd, 
            sd,m,n,1.0e-15,nty,ier,maxmsd);
 
if((ier >= 21) and (ier <= 23)) lsd = 0;

// compute and print the first derivative of the loglikelihood 
// with respect to s(j)

 wqm_cdfegr(ilcv,iucv,iltv,iutv,weight,nty,n,probd,m,pgrad);

goto line988;
line98: xllgkm=0.;
probd.at(0) = prob.at(0);

if(m != 1) {
  
   for(int i = 2; i <= m; i++){
     
       probd.at(i - 1) = prob.at(i - 1) - prob.at(i - 2);
     
   }

}
line988: cdfxlk(ilcv,iucv,iltv,iutv,weight,nty,n,probd,m,xllgkm);

// fix end effects, and recover end points;
// ignore last interval if it came from type 2 observation;

for(int j = 1; j <= m; j++){
  
    ipndx = iorder.at(j - 1);
    iqndx = iorder.at(j - 1 + m);
    
    if(j != m) {
      
       ippndx  = iorder.at(j);
       p.at(j - 1) = y.at(iqndx - 1,ny - 1);
       q.at(j - 1) = y(ippndx - 1,0);
       
       continue;
    
    }
    
    ms = m - 1;
    if(q.at(m - 1) >= big29) continue;
    
    ms = m;
    p.at(j - 1) = y.at(iqndx - 1,ny - 1);
    q.at(j - 1) = big29;
    
}

m = ms;
ipndx = iorder.at(0);
icode = codes.at(ipndx - 1);

if((icode != 1) & (icode != 4) & (icode != 5)) return;

// Set up 0 estimate of cdf if first inter. 
// came from type 1 or 4 or 5

int jrev;

for(int j = 1; j <= m; j++) {

    jrev = m - j + 2;
    p.at(jrev - 1) = p.at(jrev - 2);
    q.at(jrev - 1) = q.at(jrev - 2);
    prob.at(jrev - 1) = prob.at(jrev - 2);
    sd.at(jrev - 1) = sd.at(jrev - 2);
  
}

m = m + 1;
prob.at(0) = zero;
  sd.at(0) = zero;
   p.at(0) = -big29;
   q.at(0) = y.at(ipndx - 1, 0);

return;

}
