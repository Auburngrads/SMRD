#include <base/base.hpp>
#include <wqmmlesss/wqm_mlfi1.hpp>

//' Modified for svd by wqm
//' Compute maximum likelihood estimates of a linear 
//' model with location-scale errors and censored data

void wqm_mlfit(Rcpp::NumericMatrix &xnew,
               Rcpp::NumericMatrix &y,
               Rcpp::IntegerVector &cen,
               Rcpp::IntegerVector &wt,
               int &nty,
               Rcpp::NumericMatrix &ty,
               Rcpp::IntegerVector &tcodes,
               int &nrow,
               int &nter,
               int &ny,
               Rcpp::NumericVector &diag,
               Rcpp::NumericMatrix &tmat,
               Rcpp::NumericVector &thetb,
               int &kdist,
               Rcpp::NumericVector &gamthr,
               double &xlike,
               Rcpp::NumericVector &thetg,
               Rcpp::LogicalVector &lfix,
               int &nparm,
               double &upcen,
               Rcpp::NumericMatrix &res,
               Rcpp::NumericVector &fv,
               double &escale,
               Rcpp::NumericVector &e,
               int &maxit,
               Rcpp::NumericVector &dscrat,
               int &ier,
               Rcpp::NumericVector &itd,
               Rcpp::NumericVector &itf,
               Rcpp::NumericVector &ied,
               Rcpp::NumericVector &iw){

int iwdim = nparm * nparm + 3 * nparm;
        
 wqm_mlfi1(xnew,y,cen,wt,nty,ty,tcodes,nrow,nter,
           ny,diag,tmat,thetb,kdist,gamthr,
           xlike,thetg,lfix,nparm,upcen,res,fv,
           escale,e,maxit,itd,itf,ied,iw,iwdim,ier);

return;
  
}


#include <base/base.hpp>
#include <wqmmlesss/wqm_tran.hpp>
#include <wqmmlesss/wqm_toffst.hpp>
#include <wqmmlesss/wqm_powsss.hpp>
#include <wqmmlesss/wqm_dfxmu.hpp>
#include <wqmmlesss/wqm_upck.hpp>
#include <wqmmlesss/wqm_fillr.hpp>
using namespace lstd;

bool mod2(int x, int m) { return x % m == 0; }

void wqm_mlfi1(Rcpp::NumericMatrix &xnew,
               Rcpp::NumericMatrix &y,
               Rcpp::IntegerVector &cen,
               Rcpp::IntegerVector &wt,
               int &nty,
               Rcpp::NumericMatrix &ty,
               Rcpp::IntegerVector &tcodes,
               int &nrow,
               int &nter,
               int &ny,
               Rcpp::NumericVector &diag,
               Rcpp::NumericMatrix &tmat,
               Rcpp::NumericVector &thetb,
               int &kdist,
               Rcpp::NumericVector &gamthr,
               double &xlike,
               Rcpp::NumericVector &thetg,
               Rcpp::LogicalVector &lfix,
               int &nparm,
               double &upcen,
               Rcpp::NumericMatrix &res,
               Rcpp::NumericVector &fv,
               double &escale,
               Rcpp::NumericVector &e,
               int &maxit,
               Rcpp::NumericVector &thetad,
               Rcpp::NumericVector &thetaf,
               Rcpp::NumericVector &ed,
               Rcpp::NumericVector &w,
               int &iwdim,
               int &ier){

bool ltpp;
int icon, iretry, itype;
double temp,thhold,siglgn,sigma,xmu;
bool ltr3  = false;
bool ltyc3 = false;
ier = 0;

// Take logs for log-location-scale distributions
// (weibull, lognormal, log-logistic)
//
// ltr3 = .false. now - will be set to true if any 
// type 4 obs have 0 as lower lim
//  
//  If we subtract out an offset/threshold, everything else is
//  done with the new response

if(mod2(kdist,2)) wqm_tran(y,cen,wt,nrow,ny,gamthr,false,ltr3,ier); 

// Just do the offset thing

if(!mod2(kdist,2)) wqm_toffst(gamthr,y,nrow,ny); 

// Now take care of the truncation times in the same way

if((mod2(kdist,2)) and (nty > 0)) wqm_tran(ty,tcodes,wt,nrow,nty,gamthr,true,ltyc3,ier); 

// Just do the offset thing
if((!mod2(kdist,2)) and (nty > 0)) wqm_toffst(gamthr,ty,nrow,nty); 

if(debug::kprint >= 2) {
   
   bool check = mod2(kdist,2);
   Rcpp::Rcout << "\nMLFI1 **1**\n" << std::endl;
   Rcpp::Rcout << "y = \n" << y << std::endl;
   Rcpp::Rcout << "ltr3 = " << ltr3 << std::endl;
   Rcpp::Rcout << "check = " << check << std::endl;
  
}

if(ier != 0) return;

// Check for zero sigma start value
temp = thetg.at(nparm - 1);
if(temp >  0.0e00) thetg.at(nparm - 1) = std::log(thetg.at(nparm - 1));
if(temp <= 0.0e00) thetg.at(nparm - 1) = 0.0e00;
icon = 1;

// call the powell algorithm to maximize the likelihood
ltpp = true;

 wqm_powsss(thetg,e,nparm,upcen,xlike,
            escale,icon,ltpp,maxit,
            diag,tmat,thetb,thetad,thetaf,
            lfix,ed,xnew,y,cen,wt,nty,ty,tcodes,
            nrow,nter,ny,kdist,w,iwdim,ier);

e = Rcpp::NumericVector(nparm, 1.0e-7);
ltpp = false;
thhold = thetg.at(nparm - 1);

// if powell fails at this level, mult sigma est by 2 and retry

iretry = 0;

line30: wqm_powsss(thetg,e,nparm,upcen,xlike,
                   escale,icon,ltpp,maxit,
                   diag,tmat,thetb,thetad,thetaf,
                   lfix,ed,xnew,y,cen,wt,nty,ty,tcodes,
                   nrow,nter,ny,kdist,w,iwdim,ier);

if(ier == 0) goto line40;

// Try a restart at this level a max of four times
// if sigma gets big enough, we can move the likelihood

if(iretry > 3) goto line40;

iretry = iretry + 1;
siglgn = iretry + 1;

// If sigma is fixed, we are done - inputted this
// conditional to fix a bug found in bootstrap simulation
if(lfix.at(nparm - 1)) goto line40;
thetg.at(nparm - 1) = std::log((double)siglgn) + thhold;
goto line30;

line40: thetg.at(nparm - 1) = std::exp(thetg.at(nparm - 1));

// Compute the residuals and fitted values
sigma = thetg.at(nparm - 1);

for(int i = 1; i <= nrow; i++){

    for(int j = 1; j <= ny; j++){

        itype = cen.at(i - 1);
        xmu = wqm_dfxmu(i,xnew,nrow,nter,thetaf,nparm,upcen,sigma);
        fv.at(i - 1) = wqm_upck(xmu,kdist);

        if(!((itype == 3) and (ltr3) and (j == 1))) goto line50;

        y.at(i - 1,j - 1) = 0.0;
        cen.at(i - 1) = 4;
    
        line50: res.at(i - 1,j - 1) = wqm_upck((y.at(i - 1,j - 1) - xmu) / sigma, kdist);

    }

  }

return;

  }