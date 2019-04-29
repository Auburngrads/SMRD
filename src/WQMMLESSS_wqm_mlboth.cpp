#include <base/base.hpp>
#include <wqmmlesss/wqm_mlbot1.hpp>

//' Compute maximum likelihood estimates and 
//' variance-covariance matrix with a possible
//' linear regression model, location-scale errors, 
//' and censored; and/or truncated data;
//' @param x matrix dimension (nrow,nter) of explanatory variables.
//'          If an intercept term is desired in the model, then the 
//'          first column should be a column of ones. For single
//'          distribution, send down only a column of ones.
//' @param y matrix dimension (nrow,ny) of times to failure or 
//'          censoring times or lower and upper limits of type 4 
//'          (interval); observations. col 2 is needed only if 
//'          there are type 4 obs. if col 2 is present, col 1 
//'          and col 2 should contain the same values for all 
//'          but type 4 obs.
//'          
//' @param cen vector of censor codes (nrow)
//'        0 dummy observation
//'        1 failure time
//'        2 right censored observation
//'        3 left censored observation
//'        4 interval observation
//'        
//' @param wt vector of observation weights or multiplicities 
//' @param nrow number of observations
//' @param nter number of independent variables (including intercept)
//' @param ny number of columns in y (1 or 2)
//' @param nty number of cols in ty (0, 1 or 2)
//'        if nty=0, there is no truncation and ty 
//'        and tcodes are not accessed
//' @param ty Matrix dimension (nrow,nty) of truncation times
//'        first (second) col contains lower (upper)
//'        truncation limits for type 4 (interval truncated)
//'        observations. if nty=2, for all other types
//'         set ty(i,1)=ty(i,2);
// #;
// # tcodes(nrow) vector of truncation codes;
// # 1 no truncation;
// # 2 right truncated observation;
// # 3 left truncated observation;
// # 4 interval truncated obser.;
// #;
// # kdist distribution code;
// # 1 smallest extreme value distribution;
// # 2 weibull distribution;
// # 3 normal distribution;
// # 4 lognormal distribution;
// # 5 logistic distribution;
// # 6 log-logistic distribution;
// #;
// #;
// # gamthr vector of 'offsets' for responses--to be subtracted;
// # from each of the responses before fitting;
// #;
// # when fitting an 'even' distribution, this offset can be used;
// # as a known threshhold parameters for;
// #;
// # 3-parameter weibull;
// # 3-parameter lognormal;
// # 2-parameter exponential;
// #;
// # use zero as a default;
// #;
// # lfix(nparm) = true if parameter is to be fixed;
// # false otherwise;
// #;
// # lcheck = true if only the xmatrix is to be checked;
// # and no estimation is to be performed;
// # no starting values are needed for this;
// # option.;
// # false regular estimation option;
// #;
// # nparm number of parameters (nter+int);
// #;
// # int =1 if there is an intercept (constant term);
// # =0 otherwise;
// #;
// # escale powell algorithm scale factor (use 1000.);
// #;
// # e(nparm) powell algorithm final accuracy (e.g. .001);
// # note: maximum step size=e(i)*escale;
// #;
// # maxit powell algorithm maximum number of iterations;
// #;
// # kprint level of printing (0 to 5);
// #;
// # (note that theta--see below--is input and output);
// #;
// #;
// #********scratch arrays********;
// #;
// #;
// # dscrat double precision scratch array;
// # length (nparm*nrow+5nparm*nparm+12*nparm+1);
// #;
// # iscrat integer scratch array of length 2(nparm+1);
// #;
// #;
// #********outputs***************;
// #;
// #;
// # xlike maximum value of the loglikelihood function;
// #;
// # devian vector of deviances (likelihood contributions);
// # for each case;
// #;
// # theta(nparm) send down starting values, returns ml est.;
// # the scale parameter is the last in the vector;
// #;
// # fsder(nparm) first derivatives of the loglikelihood wrt theta;
// #;
// # vcv(nparm,nparm) variance-covariance matrix;
// #;
// # r(nparm,nparm) estimated correlation matrix of the ml est.;
// #;
// # res(nrow,ny) returns residuals for regression;
// #;
// # fv(nrow) returns fitted values for regression;
// #;
// # ierfit returning error code;
// # 0 no error;
// # 1 maximum change in parameter does not change;
// # the likelihood;
// # 2 maximum number of iterations used before conv.;
// # 3 too many poweld restarts--bad starting values;
// # or bad poweld input parameters;
// # 4 log of a nonpositive response (y) or truncation;
// # time requested;
// # 6 colinear columns in the x matrix;
// # 7 error in svd routine; check for faulty x matrix;
// #;
// # iervcv returning error code;
// #;
// # check: non full rank information matrix (iernfr=1);
// # information matrix that is not positive definite (iernpd=1);
// # first derivatives that are not close to zero (iernzd=1);
// #;
// # iernfd iernpd iernfr iervcv;
// # 0 0 0 0;
// # 1 0 0 1;
// # 0 1 0 2;
// # 1 1 0 3;
// # 0 0 1 4;
// # 1 0 1 5;
// # 0 1 1 6;
// # 1 1 1 7;
// # estimate of sigma was approaching zero 8;
// # likely cause is a large number of exact;
// # failures at exactly the same time (could;
// # be caused by coding a collection of;
// # censored observations as exact failures);
// #;
// # copyright c 1989;
// # william q. meeker, jr.;
// # route 1;
// # ames, iowa 50010;
// #;
// # june 25, 1989;
// #;
//*****************************************************************************;
// #18/23 nov 1992 modified for ford to;
// #;
// # o allow vector gamthr;
// # o return vector devian of deviances/likelihood contributions;
// # o allow completion even if first dervs not close to 0;
// # o add the -log(time-gamthr) term to the loglikelihood;
// # o fix eigenvalue error check to work with icbreak;

void wqm_mlboth(Rcpp::NumericMatrix &x,
                Rcpp::NumericMatrix &y,
                Rcpp::IntegerVector &cen,
                Rcpp::IntegerVector &wt,
                int &nrow,
                int &nter,
                int &ny,
                int &nty,
                Rcpp::NumericMatrix &ty,
                Rcpp::IntegerVector &tcodes,
                int &kdist,
                Rcpp::NumericVector &gamthr,
                Rcpp::LogicalVector &lfix,
                bool &lcheck,
                int &nparm,
                int &intcpt,
                double &escale,
                Rcpp::NumericVector &e,
                int &maxit,
                Rcpp::NumericVector &dscrat,
                Rcpp::IntegerVector &iscrat,
                double &xlike,
                Rcpp::NumericMatrix &devian,
                Rcpp::NumericVector &theta,
                Rcpp::NumericVector &fsder,
                Rcpp::NumericMatrix &vcv,
                Rcpp::NumericMatrix &r,
                Rcpp::NumericMatrix &res,
                Rcpp::NumericVector &fv,
                int &ierfit,
                int &iervcv,
                Rcpp::NumericMatrix &ipxnew,
                Rcpp::NumericVector &iprv1,
                Rcpp::NumericVector &ipdiag,
                Rcpp::NumericMatrix &iptmat,
                Rcpp::NumericVector &ipthb,
                Rcpp::NumericVector &ipthg,
                Rcpp::NumericVector &ipfsd,
                Rcpp::NumericMatrix &ipvcvb,
                Rcpp::NumericMatrix &ipvcvg,
                Rcpp::NumericVector &ipnext,
                Rcpp::NumericVector &itd,
                Rcpp::NumericVector &itf,
                Rcpp::NumericVector &ied,
                Rcpp::NumericVector &iw,
                Rcpp::NumericVector &ivd,
                Rcpp::NumericMatrix &ivcvd,
                Rcpp::NumericMatrix &ivcvdd,
                Rcpp::IntegerVector &iir,
                Rcpp::IntegerVector &ijc){
  
if(debug::kprint >= 3){
   
   Rcpp::Rcout << "\nWQM_MLBOTH***3***\n" << std::endl;
   Rcpp::Rcout << "ipxnew = " << ipxnew << std::endl;
   Rcpp::Rcout << "iprv1 = " << iprv1 << std::endl;
   Rcpp::Rcout << "ipdiag = " << ipdiag << std::endl;
   Rcpp::Rcout << "iptmat = " << iptmat << std::endl;
   Rcpp::Rcout << "ipthb = " << ipthb << std::endl;
   Rcpp::Rcout << "ipthg = " << ipthg << std::endl;
   Rcpp::Rcout << "ipfsd = " << ipfsd << std::endl;
   Rcpp::Rcout << "ipvcvb = " << ipvcvb << std::endl;
   Rcpp::Rcout << "ipvcvg = " << ipvcvg << std::endl;
   Rcpp::Rcout << "ipnext = " << ipnext << std::endl;
   
}

   wqm_mlbot1(x,y,cen,wt,nty,ty,tcodes,nrow,nter,ny,
              kdist,gamthr,intcpt,xlike,devian,
              theta,lfix,lcheck,nparm,res,fv,escale,e,
              maxit,ipnext,ierfit,fsder,ipfsd,vcv,
              ipvcvb,ipvcvg,r,iscrat,iervcv,ipxnew,
              iprv1,ipdiag,iptmat,ipthb,ipthg,itd,
              itf,ied,iw,ivd,ivcvd,ivcvdd,iir,ijc);

return;

}


#include <base/base.hpp>
#include <wqmmlesss/wqm_fpfail.hpp>
#include <utility/wqm_quant.hpp>
#include <wqmmlesss/wqm_svdfix.hpp>
#include <wqmmlesss/wqm_corr.hpp>
#include <wqmmlesss/wqm_varcov.hpp>
#include <wqmmlesss/wqm_mlfit.hpp>
#include <wqmmlesss/wqm_gtdev.hpp>
#include <wqmmlesss/wqm_vcvgam.hpp>
#include <wqmmlesss/wqm_vcvbet.hpp>
#include <wqmmlesss/wqm_comgam.hpp>
#include <wqmmlesss/wqm_combet.hpp>

void wqm_mlbot1(Rcpp::NumericMatrix &xold,
                Rcpp::NumericMatrix &y,
                Rcpp::IntegerVector &cen,
                Rcpp::IntegerVector &wt,
                int &nty,
                Rcpp::NumericMatrix &ty,
                Rcpp::IntegerVector &tcodes,
                int &nrow,
                int &nter,
                int &ny,
                int &kdist,
                Rcpp::NumericVector &gamthr,
                int &intcpt,
                double &xlike,
                Rcpp::NumericMatrix &devian,
                Rcpp::NumericVector &theta,
                Rcpp::LogicalVector &lfix,
                bool &lcheck,
                int &nparm,
                Rcpp::NumericMatrix &res,
                Rcpp::NumericVector &fv,
                double &escale,
                Rcpp::NumericVector &e,
                int &maxit,
                Rcpp::NumericVector &dscrat,
                int &ierfit,
                Rcpp::NumericVector &fsder,
                Rcpp::NumericVector &fsderd,
                Rcpp::NumericMatrix &vcv,
                Rcpp::NumericMatrix &vcvb,
                Rcpp::NumericMatrix &vcvg,
                Rcpp::NumericMatrix &r,
                Rcpp::IntegerVector &iscrat,
                int &iervcv,
                Rcpp::NumericMatrix &xnew,
                Rcpp::NumericVector &rv1,
                Rcpp::NumericVector &diag,
                Rcpp::NumericMatrix &tmat,
                Rcpp::NumericVector &thetb,
                Rcpp::NumericVector &thetg,
                Rcpp::NumericVector &itd,
                Rcpp::NumericVector &itf,
                Rcpp::NumericVector &ied,
                Rcpp::NumericVector &iw,
                Rcpp::NumericVector &ivd,
                Rcpp::NumericMatrix &ivcvd,
                Rcpp::NumericMatrix &ivcvdd,
                Rcpp::IntegerVector &iir,
                Rcpp::IntegerVector &ijc){
  
bool lsvd = true;
double svdtol = 1.0e-08, dgmin = 0;
int ierr = 0;
  
// svd xold=(xnew)(diag)(tmat)';
// get percentile correction for reparameterization;
// we could turn the svd off for checking with well-behaved data;

double upcen;
double fpfxxx;

fpfxxx = wqm_fpfail(cen,wt,nrow);
upcen  = wqm_quant(fpfxxx / two, kdist);
if((intcpt == 0) or (lfix.at(0))) upcen = zero;

if(debug::kprint >= 1) {
   
   Rcpp::Rcout << "\nWQM_MLBOT1**1**\n"                        << std::endl;
   Rcpp::Rcout << "threshhold parameter(0) = " << gamthr.at(0) << std::endl;
   Rcpp::Rcout << "proportion failing = "      << fpfxxx       << std::endl;
   Rcpp::Rcout << "upcen = "                   << upcen        << std::endl;
   Rcpp::Rcout << "intcpt = "                    << intcpt       << std::endl;
   Rcpp::Rcout << "nrow = "                    << nrow         << std::endl;
   Rcpp::Rcout << "nter = "                    << nter         << std::endl;
   Rcpp::Rcout << "ny = "                      << ny           << std::endl;
   Rcpp::Rcout << "nty = "                     << nty          << std::endl;
   Rcpp::Rcout << "nparm = "                   << nparm        << std::endl;
   Rcpp::Rcout << "kdist = "                   << kdist        << std::endl;
   Rcpp::Rcout << "maxit = "                   << maxit        << std::endl;
   Rcpp::Rcout << "lsvd = "                    << lsvd         << std::endl;
   Rcpp::Rcout << "lcheck = "                  << lcheck       << std::endl;
   
}

if(debug::kprint >= 3) {
   
   Rcpp::Rcout << "\nWQM_MLBOT1**2**\n" << std::endl;
   Rcpp::Rcout << "lfix = "     << lfix << std::endl;
   
}

// #find the svd of xold and also get the transform matrices;
// #use special fix for columns of x corresponding to fixed;
// #parameters;
// #vcvg and rv1 are scratch arrays here;

 wqm_svdfix(lfix,nrow,nter,nparm,xold,xnew,diag,
            tmat,dgmin,ierr,vcvg,rv1,lsvd);

if(ierr > 0)       { ierfit = 7; return; }
if(dgmin < svdtol) { ierfit = 6; return; }

// If only checking and everything is ok, we are done;
   if(lcheck) return;

// compute scaled start values;
   thetb = clone(theta);
   
   if(debug::kprint >= 2) {
      
      Rcpp::Rcout << "\nWQM_MLBOT1**3**\n"               << std::endl;
      Rcpp::Rcout << "Original start values = " << thetb << std::endl;
      
   }
   
   thetb.at(0) = thetb.at(0) + upcen * thetb.at(nparm - 1);
   wqm_comgam(thetb,diag,tmat,nparm,thetg);
   
   
   if(debug::kprint >= 2) {
      
      Rcpp::NumericVector ycol0 = y.column(0);
      Rcpp::Rcout << "\nWQM_MLBOT1**4**\n"             << std::endl;
      Rcpp::Rcout << "Scaled start values = " << thetg << std::endl;
      Rcpp::Rcout << "Column 0 of y = " << ycol0 << std::endl;
      
   }

   if(debug::kprint >= 3) {
      
      Rcpp::NumericVector ycolny = y.column(ny - 1);
      Rcpp::Rcout << "\nWQM_MLBOT1**5**\n"                   << std::endl;
      Rcpp::Rcout << "Column ny of y = " << ycolny << std::endl;
      
   }

// Find mle's;
   wqm_mlfit(xnew,y,cen,wt,nty,ty,tcodes,nrow,nter,ny,
             diag,tmat,thetb,kdist,gamthr,xlike,
             thetg,lfix,nparm,upcen,res,fv,escale,e,
             maxit,dscrat,ierfit,itd,itf,ied,iw);

// Try calling for a likelihood.
// The following will not work without modification
// because flike assumes that the last ele of thetag
// contains log(sigma). Could switch on fly if really needed.

//xx f=flike(y,xnew,cen,wt,nty,ty,tcodes,nrow,nter,ny,;
//xx &diag,tmat,thetb,kdist,thetg,nparm,upcen);
//xx if(kprint >= 2);
//xx &call wqm_prmess(' ending value of likelihood$');
//xx if(kprint >= 2)call wqm_prvecs(f,1);

// Call wqm_gtdev to get the deviance/likelihood contributions;
   double flikes = 0.0e00;

   wqm_gtdev(y,xnew,cen,wt,nty,ty,tcodes,
             nrow,nter,ny,diag,tmat,thetb,
             kdist,thetg,nparm,upcen,devian,
             flikes);

// Recover original parameter and reset upcen;
   wqm_combet(thetg,diag,tmat,nparm,thetb);
   thetb.at(0) = thetb.at(0) - upcen * thetb.at(nparm - 1);
   upcen = zero;

   wqm_comgam(thetb,diag,tmat,nparm,thetg);

// goto line compute the vcv matrix;
   wqm_varcov(xnew,y,cen,wt,nty,ty,tcodes,nrow,nter,ny,
              kdist,thetg,fsderd,lfix,nparm,upcen,vcvg,
              dscrat,iscrat,iervcv,ivd,ivcvd,ivcvdd,iir,ijc);

// print results corresponding to the orthogonal xnew;

if(iervcv >= 4) return;

// compute and print results corresponding to the original xold;
   wqm_combet(thetg,diag,tmat,nparm,thetb);
   wqm_vcvbet(vcvg,diag,tmat,nparm,vcvb);

// compute and print results corresponding to the orthogonal xnew;
   fsder = clone(fsderd);
   wqm_comgam(thetb,diag,tmat,nparm,thetg);
   wqm_vcvgam(vcvb,diag,tmat,nparm,vcvg);

// compute correlation matrix of gamma parameters
   vcv = clone(vcvg);
   wqm_corr(vcv,r,nparm);

// Move double precision results to single precision return arrays
   vcv = clone(vcvb);
   theta = clone(thetb);
   wqm_corr(vcv,r,nparm);

return; 

}
