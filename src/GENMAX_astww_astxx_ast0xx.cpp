#include <base/base.hpp>
#include <genmax/ast0x1.hpp>
// #include <genmax/outpar.hpp>

using namespace ast0xx_g;

// Start values for the aft model
void ast0xx(int &kmod,
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
            Rcpp::IntegerVector &weight,
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
  
// Grab space for modified data sets, residuals, etc.
   ast0xx_g::iresid = Rcpp::NumericMatrix(nrownw, ncoly); // created in GENMAX, create again?
   ast0xx_g::iyhat  = Rcpp::NumericMatrix(nrownw, ncoly);  // created in GENMAX, create again?
   ast0xx_g::itimes = Rcpp::NumericMatrix(nrownw, ncoly);
   ast0xx_g::ip     = Rcpp::NumericVector(nrownw + 3);
   ast0xx_g::iq     = Rcpp::NumericVector(nrownw + 3);
   ast0xx_g::iprob  = Rcpp::NumericVector(nrownw + 3);
   ast0xx_g::ithtmp = Rcpp::NumericVector(nparm);
   ast0xx_g::iypoin = Rcpp::NumericVector(nrownw + 3);
   ast0xx_g::ippoin = Rcpp::NumericVector(nrownw + 3);
   ast0xx_g::ipsd   = Rcpp::NumericVector(nrownw + 3);
   ast0xx_g::ippsd  = Rcpp::NumericVector(nrownw + 3);
   
   if(debug::kprint >= 2){
      
      Rcpp::Rcout << "\nAST0XX**3**\n"                 << std::endl;
      Rcpp::Rcout << "iresid = \n" << ast0xx_g::iresid << std::endl;
      Rcpp::Rcout << "iyhat = \n"  << ast0xx_g::iyhat  << std::endl;
      Rcpp::Rcout << "itimes = \n" << ast0xx_g::itimes << std::endl;
      Rcpp::Rcout << "ip = "     << ast0xx_g::ip     << std::endl;
      Rcpp::Rcout << "iq = "     << ast0xx_g::iq     << std::endl;
      Rcpp::Rcout << "iprob = "  << ast0xx_g::iprob  << std::endl;
      Rcpp::Rcout << "ithtmp = " << ast0xx_g::ithtmp << std::endl;
      Rcpp::Rcout << "iypoin = " << ast0xx_g::iypoin << std::endl;
      Rcpp::Rcout << "ippoin = " << ast0xx_g::ippoin << std::endl;
      Rcpp::Rcout << "ipsd = "   << ast0xx_g::ipsd   << std::endl;
      Rcpp::Rcout << "ippsd = "  << ast0xx_g::ippsd  << std::endl;
      
   }

   ast0x1(kmod,kdist,intd,nxd,ipxcd,irelad,npard,
          theta,thetas,kodet,ifix,nparm,y,ncoly,nrownw,
          x,ncolx,codes,weight,ty,ncolty,tc,kcentr,iplab,
          maxit,vcvs,vcv,r,ier,
          ast0xx_g::iresid,
          ast0xx_g::iyhat,
          ast0xx_g::itimes,
          ast0xx_g::iq,
          ast0xx_g::ip,
          ast0xx_g::iprob,
          ast0xx_g::ipsd,
          ast0xx_g::ithtmp,
          ast0xx_g::iypoin,
          ast0xx_g::ippoin,
          ast0xx_g::ippsd,
          nstart,maxmsd,tol,lsd,pchmax);
 
if(debug::kprint >= 3){
   
   double conlev = 0.95;
   double xlogl = 0.0;
   
   // outpar(2,kmod,kdist,xlogl,thetas,
   //        kodet,iplab,vcv,r,nparm,conlev,nparm,3)
   
}

return;

}

#include <base/base.hpp>
#include <genmax/ccount.hpp>
#include <genmax/regprp.hpp>
#include <genmax/ast0xx.hpp>
#include <utility/vmaxr.hpp>
#include <utility/icheck.hpp>

//' Function to compute start values for the limited failure 
//' population (lfp) model

void ast1xx(int &kmod,
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

int nc145 = 0, nc2 = 0, nc3 = 0, ntotal = 0,kodeti;
double pdef = 0;
double ycmax = 0;
  
// get modified codes and compute simple estimate of p(def);
   ccount(codes,weight,nrownw,nc145,nc2,nc3,ntotal,ier);
   pdef  = (double)nc145 / (double)ntotal;
   vmaxr(y,nrownw,ycmax); // DONT DO ANYTHING WITH THIS

for(int i = 1; i <= nrownw; i++){

    icheck(codes.at(i - 1),0,5,0,0,ier,-3498);
  
    if((codes.at(i - 1) != 2) and (codes.at(i - 1) != 5)){
      
        pcodes.at(i - 1) = codes.at(i - 1);
        pweigh.at(i - 1) = weight.at(i - 1);
        continue;
      
    }
    
    if(codes.at(i - 1) == 5){
      
        pcodes.at(i - 1) = 1;
        pweigh.at(i - 1) = weight.at(i - 1);
        continue;
      
    }
    
    if(codes.at(i - 1) == 2){
      
        pcodes.at(i - 1) = 0;
        pweigh.at(i - 1) = 0;
        continue;
      
    }
    
}

 ast0xx(kmod,kdist,intd,nxd,ipxcd,irelad,npard,theta,
        thetas,kodet,ifix,nparm,y,ncoly,nrownw,x,ncolx,
        pcodes,pweigh,ty,ncolty,tc,kcentr,iplab,maxit,
        vcvs,vcv,r,ier,nstart,maxmsd,tol,lsd,pchmax);
 
  if(debug::kprint >= 3){
   
     Rcpp::Rcout << "\nAST1XX AFTER AST0XX\n" << std::endl;
     Rcpp::Rcout << "pdef = " << pdef << std::endl;
     Rcpp::Rcout << "nc145 = " << nc145 << std::endl;
     Rcpp::Rcout << "nc2 = " << nc2 << std::endl;
     Rcpp::Rcout << "nc3 = " << nc3 << std::endl;
     Rcpp::Rcout << "npard = " << npard << std::endl;
     Rcpp::Rcout << "thetas = " << thetas << std::endl;
     Rcpp::Rcout << "intd = " << intd << std::endl;
     Rcpp::Rcout << "nxd = " << nxd << std::endl;
   
  }
 
// clean up thetas vector with dummys for proportion regression, if needed;
   kodeti = 3;
   regprp(pdef,npard,thetas,intd,nxd,kodeti);

return;

}

#include <base/base.hpp>
#include <utility/icheck.hpp>

//' Count the number of different types of observations

void ccount(Rcpp::IntegerVector &codes,
            Rcpp::IntegerVector &weight,
            int &nrownw,
            int &nc145,
            int &nc2,
            int &nc3,
            int &ntotal,
            int &ier){
  
for(int i = 1; i <= nrownw; i++){
  
    icheck(codes.at(i - 1),0,5,0,0,ier,-7621);
  
    if(codes.at(i - 1) == 0) continue;
    
    // right censored observations
       if(codes.at(i - 1) == 2) {
         
          nc2 = nc2 + weight.at(i - 1); 
          continue;
          
       }
    
    // left censored observations
       if(codes.at(i - 1) == 3) {
         
          nc3 = nc3 + weight.at(i - 1); 
          continue;
          
       }
    
    // failures
       nc145 = nc145 + weight.at(i - 1);

}

ntotal = nc145 + nc2 + nc3;
  
if(debug::kprint >= 3){
  
   Rcpp::Rcout << "\nCCOUNT**3**\n" << std::endl;
   Rcpp::Rcout << "nrownw = " << nrownw << std::endl;
   Rcpp::Rcout << "nc145 = " << nc145 << std::endl;
   Rcpp::Rcout << "nc2 = " << nc2 << std::endl;
   Rcpp::Rcout << "nc3 = " << nc3 << std::endl;
  
}
  
return;

}

#include <base/base.hpp>
#include <genmax/ccount.hpp>
#include <genmax/regprp.hpp>
#include <genmax/ast0xx.hpp>
#include <utility/vmaxr.hpp>
#include <utility/icheck.hpp>

//' Function to compute start values for the 
//' dead on arrival (doa) model
void ast2xx(int &kmod,
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

int nc145 = 0, nc2 = 0, nc3 = 0, ntotal = 0,kodeti;
double pgoa = 0;
double ycmax = 0;

// get modified codes and compute simple estimate of p(goa)
   ccount(codes,weight,nrownw,nc145,nc2,nc3,ntotal,ier);
   pgoa = ((double)nc145 + (double)nc2) / ((double)ntotal);
   vmaxr(y,nrownw,ycmax); //DONT DO ANYTHING WITH THIS

for(int i = 1; i <= nrownw; i++) {
  
    icheck(codes.at(i - 1),0,5,0,0,ier,-3498);
    
    if(codes.at(i - 1) == 0) {
      
       pcodes.at(i - 1) = 0;
       pweigh.at(i - 1) = 0;
       continue;
    }
    
    if(codes.at(i - 1) == 3) {
      
       pcodes.at(i - 1) = 0;
       pweigh.at(i - 1) = 0;
       continue;
    }
    
    if(codes.at(i - 1) == 5) {
      
       pcodes.at(i - 1) = 1;
       pweigh.at(i - 1) = weight.at(i - 1);
       continue;
    }
    
    pcodes.at(i - 1) =  codes.at(i - 1);
    pweigh.at(i - 1) = weight.at(i - 1);

}

 ast0xx(kmod,kdist,intd,nxd,ipxcd,irelad,npard,theta,
        thetas,kodet,ifix,nparm,y,ncoly,nrownw,x,ncolx,
        pcodes,pweigh,ty,ncolty,tc,kcentr,iplab,maxit,
        vcvs,vcv,r,ier,nstart,maxmsd,tol,lsd,pchmax);
 
   if(debug::kprint >= 3){
   
     Rcpp::Rcout << "\nAST2XX AFTER AST0XX\n" << std::endl;
     Rcpp::Rcout << "pgoa = " << pgoa << std::endl;
     Rcpp::Rcout << "nc145 = " << nc145 << std::endl;
     Rcpp::Rcout << "nc2 = " << nc2 << std::endl;
     Rcpp::Rcout << "nc3 = " << nc3 << std::endl;
     Rcpp::Rcout << "ntotal = " << ntotal << std::endl;
     Rcpp::Rcout << "npard = " << npard << std::endl;
     Rcpp::Rcout << "thetas = " << thetas << std::endl;
     Rcpp::Rcout << "intd = " << intd << std::endl;
     Rcpp::Rcout << "nxd = " << nxd << std::endl;
   
  }
 
// clean up thetas vector with dummys for proportion regression, if needed;
   kodeti = 3;
   regprp(pgoa,npard,thetas,intd,nxd,kodeti);
 
return;

}

#include <base/base.hpp>
#include <utility/vmaxr.hpp>
#include <utility/vminr.hpp>
#include <utility/icheck.hpp>
#include <genmax/ast0xx.hpp>
#include <utility/dexpc.hpp>
#include <genmax/regprp.hpp>
#include <utility/dlogc.hpp>

//' Start values for the sts model;

void ast3xx(int &kmod,
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

double ymin = 0,ymax = 0,ymid,ttt; 
int kount,ipwww,iweig,kodeti,kdists;
double xlocss,yaver;
Rcpp::NumericVector ycol0;

// Get modified codes and compute simple estimate of loc-steady
   ycol0 = y.column(0);
   vminr(ycol0,nrownw,ymin);
   vmaxr(ycol0,nrownw,ymax);
   ymid = (ymax - ymin) / 2.0;
   kount = 0;
   ttt = zero;
   
for(int i = 1; i <= nrownw; i++){
  
    icheck(codes.at(i - 1),0,5,0,0,ier,-3498);
  
    // Use midrange as a dividing point for steady state
       if(y.at(i - 1,ncoly - 1) > ymid) goto line50;
       
       if(codes.at(i - 1) == 0){
         
          pcodes.at(i - 1) = 0;
          pweigh.at(i - 1) = 0;
          continue;
         
       }
       
       if(codes.at(i - 1) == 1){
         
          pcodes.at(i - 1) = codes.at(i - 1);
          pweigh.at(i - 1) = weight.at(i - 1);
          continue;

       }
       
       if(codes.at(i - 1) == 2){
         
          pcodes.at(i - 1) = codes.at(i - 1);
          pweigh.at(i - 1) = weight.at(i - 1);
          continue;

       }

       if(codes.at(i - 1) == 3){
         
          pcodes.at(i - 1) = codes.at(i - 1);
          pweigh.at(i - 1) = weight.at(i - 1);
          continue;

       }

       if(codes.at(i - 1) == 4){
         
          pcodes.at(i - 1) = codes.at(i - 1);
          pweigh.at(i - 1) = weight.at(i - 1);
          continue;

       }

       if(codes.at(i - 1) == 5){
         
          pcodes.at(i - 1) = 1;
          pweigh.at(i - 1) = weight.at(i - 1);
          continue;
         
       }

// Call all lomg term stuff censored at ymid
   line50: pcodes.at(i - 1) = 0;
           if(codes.at(i - 1) > 0) pcodes.at(i - 1) = 2;
           pweigh.at(i - 1) = weight.at(i - 1);
           if(codes.at(i - 1) == 0) pweigh.at(i - 1) = 0;
           ipwww = pcodes.at(i - 1);
           
// Compute total time on test for long term survivors, 
// conditional on surviving up to ymid

// First, compute the average of y's for type 4
   yaver = (y.at(i - 1,0) + y.at(i - 1,ncoly - 1)) / 2.0;
   
// It looks like yaver is not really needed because of map above;
   ttt = ttt + (dexpc(yaver) - dexpc(ymid)) * double(pweigh.at(i - 1));
   
// Count the failures
   iweig = pweigh.at(i - 1);
   if(!((codes.at(i - 1) == 0) or (codes.at(i - 1) == 2))){
     
         kount = kount + iweig;
     
   }
   
   if(debug::kprint >= 3){
     
      Rcpp::Rcout << "\nAST3XX**3**\n" << std::endl;
      Rcpp::Rcout << "i = "          << i - 1 << std::endl;
      Rcpp::Rcout << "iweig = "      << iweig << std::endl;
      Rcpp::Rcout << "ipwww = "      << ipwww << std::endl;
      Rcpp::Rcout << "kount = "      << kount << std::endl;
      Rcpp::Rcout << "codes(i) = "   << codes.at(i - 1) << std::endl;
      Rcpp::Rcout << "pcodes(i) = "  << pcodes.at(i - 1) << std::endl;
      Rcpp::Rcout << "weight(i) = "  << weight.at(i - 1) << std::endl;
      Rcpp::Rcout << "pweigh(i) = "  << pweigh.at(i - 1) << std::endl;
      Rcpp::Rcout << "ymid = "       << ymid << std::endl;
      Rcpp::Rcout << "y(i,1) = "     << y.at(i - 1,0) << std::endl;
      Rcpp::Rcout << "y(i,ncoly) = " << y.at(i - 1,ncoly - 1) << std::endl;
      Rcpp::Rcout << "ttt = "        << ttt << std::endl;
     
   }

   if(ipwww == 2) {
   
      // Fix y for change to type 2
         for(int j = 1; j <= ncoly; j++) {
           
             y.at(i - 1,j - 1) = ymid;
           
         }
   }

}

// Do weibull analysis on the first part
   kdists = 1;
   ast0xx(kmod,kdists,intd,nxd,ipxcd,irelad,npard,
          theta,thetas,kodet,ifix,nparm,y,ncoly,nrownw,
          x,ncolx,pcodes,pweigh,ty,ncolty,tc,
          kcentr,iplab,maxit,vcvs,vcv,r,ier,
          nstart,maxmsd,tol,lsd,pchmax);

// Clean up theta vector with dummys for proportion regression, if needed
// Total time on test statistic for second half of the data
   kodeti = 1;
   xlocss = dlogc(ttt / double(kount));
   regprp(xlocss,npard,thetas,intd,nxd,kodeti);
   
return;

}

#include <base/base.hpp>
#include <genmax/ccount.hpp>
#include <genmax/regprp.hpp>
#include <genmax/ast0xx.hpp>
#include <utility/vmaxr.hpp>
#include <utility/icheck.hpp>

//' Function to compute start values for the
//' parametric proportional hazards model
void ast4xx(int &kmod,
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

int kdists,nxp;
double dummy;
  
// Fix pseudo codes and weights
   for(int i = 1; i <= nrownw; i++){
     
       icheck(codes.at(i - 1),0,5,0,0,ier,-3498);
     
       if(codes.at(i - 1) == 0){
          
          pcodes.at(i - 1) = 0;
          pweigh.at(i - 1) = 0;
          continue;
         
       }
       
       if(codes.at(i - 1) == 1){
         
          pcodes(i - 1) = codes(i - 1);
          pweigh(i - 1) = weight(i - 1);
          continue;
         
       }
       
       if(codes.at(i - 1) == 2){
         
          pcodes(i - 1) = codes(i - 1);
          pweigh(i - 1) = weight(i - 1);
          continue;
         
       }

       if(codes.at(i - 1) == 3){
         
          pcodes(i - 1) = codes(i - 1);
          pweigh(i - 1) = weight(i - 1);
          continue;
         
       }

       if(codes.at(i - 1) == 4){
         
          pcodes(i - 1) = codes(i - 1);
          pweigh(i - 1) = weight(i - 1);
          continue;
         
       }

       if(codes.at(i - 1) == 5){
         
          pcodes.at(i - 1) = 1;
          pweigh.at(i - 1) = weight.at(i - 1);
          continue;
         
       }
   
}

// Do weibull regression and then go to ph
   kdists = 1;
   ast0xx(kmod,kdists,intd,nxd,ipxcd,irelad,npard,theta,
          thetas,kodet,ifix,nparm,y,ncoly,nrownw,x,ncolx,
          pcodes,pweigh,ty,ncolty,tc,kcentr,iplab,maxit,
          vcvs,vcv,r,ier,nstart,maxmsd,tol,lsd,pchmax);

// Clean up thetas vector with dummys for 
// proportion regression, if needed
   theta = clone(thetas);

// 26 Aug 94 
// Dummy has not been defined here we need to look into this.
// Define it for now
dummy = 0.0;
thetas.at(0) = theta.at(0) + dummy;
thetas.at(1) = theta.at(nparm - 1);
//call wqm_copyd(theta.at(1),thetas.at(2),nparm-2);

// This needs to be generalized to handle parameters other than mu
// that have regression relationships;

// 26 Aug 94 f77 noticed that nxp was undefined--perhaps nxd?? change;
// do 33 i=3,nxp
   nxp = nxd.at(0);
   Rcpp::stop("Pre existing issue in ast4xx -- aborting");
   
for(int i = 3; i <= nxp; i++){
  
    thetas.at(i - 1) = thetas.at(i - 1) / thetas.at(1);
  
}

return;

}
