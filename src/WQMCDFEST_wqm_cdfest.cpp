#include <base/base.hpp>
#include <wqmcdfest/wqm_cdfes1.hpp>

void wqm_cdfest(NumericMatrix &y,
                int &ny,
                IntegerVector &codes,
                IntegerVector &weight,
                NumericMatrix &ty,
                int &nty,
                IntegerVector &tcodes,
                int &n,
                int &nstart,
                NumericVector &dscrat,
                NumericVector &scrat,
                IntegerVector &iscrat,
                int &maxit,
                double &tol,
                int &maxmsd,
                NumericVector &p,
                NumericVector &q,
                NumericVector &prob,
                NumericVector &sd,
                int &m,
                double &pchmax,
                bool &lsd,
                int &ier,
                Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                Rcpp::IntegerVector &iorder,
                Rcpp::NumericVector &xlcen,
                Rcpp::NumericVector &xrcen,
                Rcpp::NumericVector &fail,
                Rcpp::NumericVector &xltru,
                Rcpp::NumericVector &xrtru,
                Rcpp::NumericVector &ys,
                Rcpp::NumericVector &pgrad,
                Rcpp::NumericVector &s,
                Rcpp::NumericVector &probd){

wqm_cdfes1(y,ny,codes,weight,ty,nty,tcodes,n,nstart,
           dscrat,scrat,iscrat,
           maxit,tol,maxmsd,xlcen,xrcen,fail,xltru,xrtru,
           ys,p,q,pgrad,prob,sd,s,probd,m,ilcv,iucv,iltv,iutv,
           iorder,pchmax,lsd,ier);
  
}

#include <base/base.hpp>
#include <wqmcdfest/wqm_cdfckd.hpp>
#include <wqmcdfest/wqm_cdfstr.hpp>
#include <wqmcdfest/wqm_cdftru.hpp>
//#include "heads/wqm_cdfest/wqm_cdfgra.h"
#include <wqmcdfest/wqm_cdfgkm.hpp>
#include <wqmcdfest/wqm_cdfema.hpp>
#include <wqmcdfest/wqm_cdfesi.hpp>
#include <wqmcdfest/wqm_cdfegr.hpp>

//' @description Basic routine for computing the
//'              nonparametric maximum likelihood
//'              estimate of a cumulative distribution
//'              function. Called only by wqm_cdfest

void wqm_cdfes1(Rcpp::NumericMatrix &y,
                int &ny,
                Rcpp::IntegerVector &codes,
                Rcpp::IntegerVector &weight,
                Rcpp::NumericMatrix &ty,
                int &nty,
                Rcpp::IntegerVector &tcodes,
                int &n,
                int &nstart,
                Rcpp::NumericVector &dscrat,
                Rcpp::NumericVector &scrat,
                Rcpp::IntegerVector &iscrat,
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
                Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                Rcpp::IntegerVector &iorder,
                double &pchmax,
                bool &lsd,
                int &ier){

lsd = true;
int ms = 0;
int ipndx;
int iqndx;
int ippndx = 0;
int itype;
// check the data

  if(debug::kprint >= 3){
    
     Rcpp::NumericVector ycol0 = y.column(0);
     Rcpp::NumericVector ycol1 = y.column(ny - 1);
     Rcpp::Rcout << "\nWQM_CDFES1\n " << std::endl;
     Rcpp::Rcout << "         n = " << n      << std::endl;
     Rcpp::Rcout << "        ny = " << ny     << std::endl;
     Rcpp::Rcout << "       nty = " << nty    << std::endl;
     Rcpp::Rcout << "    nstart = " << nstart << std::endl;
     Rcpp::Rcout << "     maxit = " << maxit  << std::endl;
     Rcpp::Rcout << "     y(,0) = " << ycol0  << std::endl;
     Rcpp::Rcout << "y(,ny - 1) = " << ycol1  << std::endl;
     Rcpp::Rcout << "     codes = " << codes  << std::endl;
     Rcpp::Rcout << "    weight = " << weight << std::endl;
     
  }

 wqm_cdfckd(y,ny,ty,nty,codes,weight,tcodes,n,tol,ier);
  
  if(debug::kprint > 0) {
    
     Rcpp::Rcout << "\nafter cdfckd\n" << std::endl;
     Rcpp::Rcout << "ier = " << ier << std::endl;
     
  }

if(ier <= 0) {

// find the reduced set of intervals and data structure
   wqm_cdfstr(y,ny,ys,ty,nty,tcodes,iorder,ilcv,iucv,
              codes,weight,n,p,q,m);
  
  if(debug::kprint > 0) {
    
     Rcpp::Rcout << "\nAFTER CDFSTR**\n"  << std::endl;
     Rcpp::Rcout << "     m = " << m      << std::endl;
     Rcpp::Rcout << "nstart = " << nstart << std::endl;
    
  }
  
if(m == 0) { ier = 14; return; }

if((nstart > 0) & (nstart != m)) { ier = 15; return; }

// find the truncation indices
   wqm_cdftru(ys,ty,nty,tcodes,n,p,q,m,
              iltv,iutv,ier);

  if(debug::kprint >= 1){
    
     for(int i = 1; i <= n; i++) {
       
         Rcpp::Rcout << "\nWQM_CDFES1 DATA STRUCTURE\n"        << std::endl;
         Rcpp::Rcout << "          i = " << i - 1              << std::endl;
         Rcpp::Rcout << "     y(i,0) = " << y.at(i - 1,0)      << std::endl;
         Rcpp::Rcout << "y(i,ny - 1) = " << y.at(i - 1,ny - 1) << std::endl;
         Rcpp::Rcout << "   codes(i) = " << codes.at(i - 1)    << std::endl;
         Rcpp::Rcout << "  weight(i) = " << weight.at(i - 1)   << std::endl;
         Rcpp::Rcout << "    ilcv(i) = " << ilcv.at(i - 1)     << std::endl;
         Rcpp::Rcout << "    iucv(i) = " << iucv.at(i - 1)     << std::endl;
         Rcpp::Rcout << "    iltv(i) = " << iltv.at(i - 1)     << std::endl;
         Rcpp::Rcout << "    iutv(i) = " << iutv.at(i - 1)     << std::endl;

     }
  }

// if ier > 0, an error was detected
// in the truncation codes
   if(ier > 0) return;

// skip generalized kaplan meier if starting values were sent down

if(nstart <= 0) {

   // Go to try the generalized kaplan-meier
      wqm_cdfgkm(weight,ilcv,iucv,iltv,iutv,fail,xrcen,xlcen,
                 xltru,xrtru,prob,sd,n,m,nty,ier);

   if(debug::kprint >= 3) { 
     
      Rcpp::Rcout << "\nier after WQM_CDFGKM\n" << std::endl;
      Rcpp::Rcout << "ier = "                   << ier << std::endl;
      
   }
    
   // Force through turnbull for checking by setting ier = -1
   // ier = -1;
   // If ier = 0, we have a good solution, so skip to cleanup
      if(ier == 0) goto loop1;

   // If ier > 0, we have a data inconsistancy or other serious error
      if(ier > 0) return;

   //   if ier < 1, we will try trunbull's algorithm

}

lsd = false;

 wqm_cdfema(weight,codes,ilcv,iucv,iltv,iutv,pgrad,
            prob,s,probd,n,m,nty,nstart,maxit,tol,
            pchmax);
        
//  Call the information matrix standard error routine
    wqm_cdfesi(weight,ilcv,iucv,iltv,iutv,probd,
               sd,m,n,1.0e-15,nty,ier,maxmsd);
 
lsd = true;

if((ier >= 21) and (ier < 23)) lsd = false;

if(debug::kprint > 1) {

   wqm_cdfegr(ilcv,iucv,iltv,iutv,weight,nty,n,probd,m,pgrad);

   //  Go to compute the standard errors through the 
   //  information matrix
   //
   //  Compute the first derivative of the loglikelihood wrt s(j)

   for(int j = 1; j <= m; j++){
     
       Rcpp::Rcout << "\nWQM_CDFEGR iter = " << j - 1   << std::endl;
       Rcpp::Rcout << "probd(j) = " << probd.at(j - 1) << std::endl;
       Rcpp::Rcout << "pgrad(j) = " << pgrad.at(j - 1) << std::endl;
       Rcpp::Rcout << " prob(j) = " << prob.at(j - 1)  << std::endl;
       Rcpp::Rcout << "   sd(j) = " << sd.at(j - 1)    << std::endl;

    }
   
}

// Fix end effects, and recover end points. 
// Ignore last interval if it came from type 2  
// (Right censored) observation.

loop1: for(int j = 1; j <= m; j++) {

           ipndx = iorder.at(j - 1);
           iqndx = iorder.at(j + m - 1);

           if(j != m) {

              ippndx = iorder.at(j + 1 - 1);

              p.at(j - 1) = y.at(iqndx - 1, ny - 1);
              q.at(j - 1) = y.at(ippndx - 1, 0);
     
              if(debug::kprint >= 10){
        
                 Rcpp::Rcout << "\nWQM_CDFES1 loop 1\n"        << std::endl;
                 Rcpp::Rcout << " iter j = " << j - 1          << std::endl;
                 Rcpp::Rcout << "      m = " << m              << std::endl;
                 Rcpp::Rcout << "  ipndx = " << ipndx          << std::endl;
                 Rcpp::Rcout << "  iqndx = " << iqndx          << std::endl;
                 Rcpp::Rcout << " ippndx = " << ippndx         << std::endl;
                 Rcpp::Rcout << "   p(j) = " << p.at(j - 1)    << std::endl;
                 Rcpp::Rcout << "   q(j) = " << q.at(j - 1)    << std::endl;
                 Rcpp::Rcout << "prob(j) = " << prob.at(j - 1) << std::endl;

              }

              continue;

           }

           ms = m - 1;

           if(q.at(m - 1) >= big29) continue;

           ms = m;
           p.at(j - 1) = y.at(iqndx - 1, ny - 1);
           q.at(j - 1) = big29;
        
           if(debug::kprint >= 10){
             
              Rcpp::Rcout << "\nWQM_CDFES1 loop 2\n"      << std::endl;
              Rcpp::Rcout << " iter j = " << j          << std::endl;
              Rcpp::Rcout << "      m = " << m          << std::endl;
              Rcpp::Rcout << "  ipndx = " << ipndx      << std::endl;
              Rcpp::Rcout << "  iqndx = " << iqndx      << std::endl;
              Rcpp::Rcout << " ippndx = " << ippndx     << std::endl;
              Rcpp::Rcout << "   p(j) = " << p.at(j)    << std::endl;
              Rcpp::Rcout << "   q(j) = " << q.at(j)    << std::endl;
              Rcpp::Rcout << "prob(j) = " << prob.at(j) << std::endl;
     
           }
        
       }

        m = ms;
        ipndx = iorder.at(0);
        itype = codes.at(ipndx - 1);
        if((itype != 1) & (itype != 4) & (itype != 5)) return;

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

//   m=0, must be some bad data
}

return;

}

