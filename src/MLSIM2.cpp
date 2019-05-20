#include <base/base.hpp>
#include <mlsim2/simnp.hpp>
#include <mlsim2/snset.hpp>
#include <wqmmlesss/wqm_mlboth.hpp>

//'                     method 2
//'
//'                      nonparametric          parametric
//'
//'method 2 simulation---nonparametric sampling/parametric inference
//'
//'must send down a complete data set
//'
//'    parameter meanings same as in wqm_mlboth except the following:
//'
//'    can get rid of scalars like iervcv,ierfit,xlike,nobs
//'
//'theta(nparm)   true value of theta
//'
//'thetah(nparm)     space for thetahat
//'
//'iarray(marray)    scratch  space needed to generate random weights
//'
//'marray            length of scratch space. must be at least equal
//'                  in length to the number of rows in wt
//'                  a computationally more efficient method is used
//'                  if the number is larger than the sum of the weights
//'
//'wtnew             vector for the random weights for the simulation
//'
//'xnew              hold space for x
//'
//'ynew             hold space for y
//'
//'iret    amount of stuff to return
//'
//'retmat(numret,numsim)  matrix to return the results, one result per col
//'
//'numsim  number of simulations to be run
//'
//'nrowr   number of rows being returned in retmat
//'
//'tspass(33)  should be able to eliminate
//'
//'lrand     should be able to eliminate
//'
//'
//'iersim  data space too small
//'
//'   need to send down data space big enough to cover all data situations
//'
// [[Rcpp::export]]
Rcpp::List MLSIM2(Rcpp::NumericMatrix x,
                  Rcpp::NumericMatrix y,
                  Rcpp::IntegerVector cen,
                  Rcpp::IntegerVector wt,
                  int nrow,
                  int nter,
                  int ny,
                  int nty,
                  Rcpp::NumericMatrix ty,
                  Rcpp::IntegerVector tcodes, 
                  int kdist,
                  Rcpp::NumericVector gamthr,
                  Rcpp::LogicalVector lfix,
                  int nparm,
                  int intcpt,
                  double escale,
                  Rcpp::NumericVector e,
                  int maxit,
                  int kprint,
                  Rcpp::NumericVector dscrat,
                  Rcpp::IntegerVector iscrat,
                  Rcpp::NumericMatrix devian,
                  Rcpp::NumericVector thetah,
                  Rcpp::NumericVector fsder,
                  Rcpp::NumericMatrix vcv,
                  Rcpp::NumericMatrix r,
                  Rcpp::NumericMatrix res,
                  Rcpp::NumericVector fv, 
                  Rcpp::NumericVector theta,
                  Rcpp::IntegerVector iarray,
                  int marray,
                  Rcpp::IntegerVector wtnew,
                  Rcpp::NumericMatrix xnew,
                  Rcpp::NumericMatrix ynew,
                  int iret,
                  Rcpp::NumericMatrix retmat,
                  int numsim, 
                  int numret,
                  Rcpp::NumericVector tspass,
                  bool lrand,
                  int iersim){

bool lcheck;
int ii,nrowr, kount,method;
int ierfit = 0, iervcv = 0;
double xlike;
Rcpp::NumericMatrix ipxnew,iptmat,ipvcvb,ipvcvg,ivcvd,ivcvdd;
Rcpp::NumericVector iprv1,ipdiag,ipthb,ipthg,ipfsd,ipnext;
Rcpp::NumericVector itd,itf,ied,iw,ivd;
Rcpp::IntegerVector iir, ijc;

debug::kprint = kprint;
     
if(debug::kprint >= 2) {
  
   Rcpp::Rcout << "x = " << x << std::endl;
   Rcpp::Rcout << "y = " << y << std::endl;
   Rcpp::Rcout << "cen = " << cen << std::endl;
   Rcpp::Rcout << "weights = " << wt << std::endl;
   Rcpp::Rcout << "nrow = " << nrow << std::endl;
   Rcpp::Rcout << "nter = " << nter << std::endl;
   Rcpp::Rcout << "ny = " << ny << std::endl;
   Rcpp::Rcout << "nty = " << nty << std::endl;
   Rcpp::Rcout << "ty = " << ty << std::endl;
   Rcpp::Rcout << "tcodes = " << tcodes << std::endl;
   Rcpp::Rcout << "kdist = " << kdist << std::endl;
   Rcpp::Rcout << "gamthr = " << gamthr << std::endl;
   Rcpp::Rcout << "lfix = " << lfix << std::endl;
   Rcpp::Rcout << "nparm = " << nparm << std::endl;
   Rcpp::Rcout << "intcpt = " << intcpt << std::endl;
   Rcpp::Rcout << "escale = " << escale << std::endl;
   Rcpp::Rcout << "e = " << e << std::endl;
   Rcpp::Rcout << "maxit = " << maxit << std::endl;
   Rcpp::Rcout << "kprint = " << kprint << std::endl;
   Rcpp::Rcout << "dscrat = " << dscrat << std::endl;
   Rcpp::Rcout << "iscrat = " << iscrat << std::endl;
   Rcpp::Rcout << "devian = " << devian << std::endl;
   Rcpp::Rcout << "thetah = " << thetah << std::endl;
   Rcpp::Rcout << "fsder = " << fsder << std::endl;
   Rcpp::Rcout << "vcv = " << vcv << std::endl;
   Rcpp::Rcout << "r = " << r << std::endl;
   Rcpp::Rcout << "res = " << res << std::endl;
   Rcpp::Rcout << "fv = " << fv << std::endl;
   Rcpp::Rcout << "ierfit = " << ierfit << std::endl;
   Rcpp::Rcout << "iervcv = " << iervcv << std::endl;
   Rcpp::Rcout << "thetah = " << thetah << std::endl;
   Rcpp::Rcout << "iret = "   << iret << std::endl;
   Rcpp::Rcout << "numsim = " << numsim << std::endl;
   Rcpp::Rcout << "numret = " << numret << std::endl;
   Rcpp::Rcout << "iersim = " << iersim << std::endl;
        
}

// Program to simulate survival data
// First cut is single distribution only

// Need to save the start values

for(int i = 0; i < nparm; i++){
  
    theta.at(i) = thetah.at(i);
  
}

// Fix up things to keep this version simple
   lcheck = false;

// Setup generator
   snset(wt,nrow,kount,method,iarray,marray,iersim);
   if(debug::kprint >= 3){
      
      Rcpp::Rcout << "\nMLSIM2: AFTER SNSET\n" << std::endl;
      Rcpp::Rcout << "kount = " << kount << std::endl;
      Rcpp::Rcout << "method = " << method << std::endl;
      Rcpp::Rcout << "iarray = " << iarray << std::endl;
      Rcpp::Rcout << "weights = " << wt << std::endl;
      Rcpp::Rcout << "marray = " << marray << std::endl;
      Rcpp::Rcout << "iersim = " << iersim << std::endl;
      
   }
   
   if(iersim > 2000) goto exit;
   
for(int isim = 1; isim <= numsim; isim++){

    // Print progress information
    if(debug::kprint >= 3){
      
       if((isim % 50) == 1) {
         
           Rcpp::Rcout << "\nBeginning simulation number = " << isim << std::endl;
         
       }
       
       // Simulate the data  with new weights
          Rcpp::Rcout << "kount = " << kount << std::endl;
          Rcpp::Rcout << "method = " << method << std::endl;
          Rcpp::Rcout << "marray = " << marray << std::endl;
          Rcpp::Rcout << "iarray = " << iarray << std::endl;
          Rcpp::Rcout << "kdist = " << kdist << std::endl;

    }
    
    simnp(iarray,marray,method,kount,wtnew,nrow);
    
    if(debug::kprint >= 1){
      
       Rcpp::Rcout << "\nwtnew = " << wtnew << std::endl;
      
    }

    // Copy over x and y (because they may have been destroyed)
       xnew = clone(x);
       ynew = clone(y);
       
    // Get start values from true parameters
       for(int i = 0; i < nparm; i++){
         
           thetah.at(i) = theta.at(i);
           
       }
       
      if(debug::kprint >= 3){
        
         Rcpp::Rcout << "\nthetah = " << thetah << std::endl;
         Rcpp::Rcout << "xnew = " << x << std::endl;
         Rcpp::Rcout << "ynew = " << y << std::endl;
         Rcpp::Rcout << "cen = " << cen << std::endl;
         Rcpp::Rcout << "wtnew = " << wtnew << std::endl;
         Rcpp::Rcout << "nrow = " << nrow << std::endl;
         Rcpp::Rcout << "nter = " << nter << std::endl;
         Rcpp::Rcout << "ny = " << ny << std::endl;
         Rcpp::Rcout << "nty = " << nty << std::endl;
         Rcpp::Rcout << "ty = " << ty << std::endl;
         Rcpp::Rcout << "tcodes = " << tcodes << std::endl;
         Rcpp::Rcout << "kdist = " << kdist << std::endl;
         Rcpp::Rcout << "gamthr = " << gamthr << std::endl;
         Rcpp::Rcout << "lfix = " << lfix << std::endl;
         Rcpp::Rcout << "lcheck = " << lcheck << std::endl;
         Rcpp::Rcout << "nparm = " << nparm << std::endl;
         Rcpp::Rcout << "intcpt = " << intcpt << std::endl;
         Rcpp::Rcout << "escale = " << escale << std::endl;
         Rcpp::Rcout << "e = " << e << std::endl;
         Rcpp::Rcout << "maxit = " << maxit << std::endl;
         Rcpp::Rcout << "kprint = " << kprint << std::endl;
         Rcpp::Rcout << "dscrat = " << dscrat << std::endl;
         Rcpp::Rcout << "iscrat = " << iscrat << std::endl;
         Rcpp::Rcout << "xlike = " << xlike << std::endl;
         Rcpp::Rcout << "devian = " << devian << std::endl;
         Rcpp::Rcout << "theta = " << thetah << std::endl;
         Rcpp::Rcout << "fsder = " << fsder << std::endl;
         Rcpp::Rcout << "vcv = " << vcv << std::endl;
         Rcpp::Rcout << "r = " << r << std::endl;
         Rcpp::Rcout << "res = " << res << std::endl;
         Rcpp::Rcout << "fv = " << fv << std::endl;
         Rcpp::Rcout << "ierfit = " << ierfit << std::endl;
         Rcpp::Rcout << "iervcv = " << iervcv << std::endl;
      
      }

      // Should give this a junk number below too, probably
         xlike = 7777.0e00;
         iervcv = 0;
         ierfit = 0;
         ipxnew = Rcpp::NumericMatrix(nrow, nter);
         iprv1 = Rcpp::NumericVector(nparm);
         ipdiag = Rcpp::NumericVector(nparm);
         iptmat = Rcpp::NumericMatrix(nparm, nparm);
         ipthb = Rcpp::NumericVector(nparm);
         ipthg = Rcpp::NumericVector(nparm);
         ipfsd = Rcpp::NumericVector(nparm);
         ipvcvb = Rcpp::NumericMatrix(nparm, nparm);
         ipvcvg = Rcpp::NumericMatrix(nparm, nparm);
         ipnext = Rcpp::NumericVector(nparm);
         itd = Rcpp::NumericVector(nparm);
         itf = Rcpp::NumericVector(nparm);
         ied = Rcpp::NumericVector(nparm);
         iw = Rcpp::NumericVector(nparm * nparm + 3 * nparm);
         ivd = Rcpp::NumericVector(nparm);
         ivcvd = Rcpp::NumericMatrix(nparm, nparm);
         ivcvdd = Rcpp::NumericMatrix(nparm + 1, nparm + 1);
         iir = Rcpp::IntegerVector(nparm + 1);
         ijc = Rcpp::IntegerVector(nparm + 1);
      
          wqm_mlboth(xnew,ynew,cen,wtnew,nrow,nter,ny,nty,ty,
                     tcodes,kdist,gamthr,lfix,lcheck,nparm,
                     intcpt,escale,e,maxit,dscrat,
                     iscrat,xlike,devian,thetah,fsder,vcv,r,
                     res,fv,ierfit,iervcv,ipxnew,iprv1,ipdiag,iptmat,
                     ipthb,ipthg,ipfsd,ipvcvb,ipvcvg,ipnext,itd,itf,
                     ied,iw,ivd,ivcvd,ivcvdd,iir,ijc);
         
      // Save the results
      
      // Always save thetah and error
         for(int k = 0; k < nparm; k++){
           
             retmat.at(k + 1,isim - 1) = thetah.at(k);
           
         }
         
         retmat.at(0,isim - 1) = 10 * iervcv + ierfit;
         
      // if( iervcv+ierfit .gt. 0)write(6,4575)(wtnew(iii),iii=1,nrow)

      // if there is a problem, print the weights to help diagnose

      if((iervcv + ierfit) > 0) {
         
         Rcpp::warning("\nProblem with updated weights for bootstrap sample\nsimulation number = %i\n Updated weights = %i\nierfit = %i\niervcv = %i", isim,wtnew,ierfit,iervcv);
         
      }

      // Now check to see what else to save
      if(iret == 1) continue;
      if(iret == 2) goto line1002; // Theta and like
      if(iret == 3) goto line1003; // Theta, like, and vcv
      if(iret == 4) continue;      // removed predict stuff
      
line1002: retmat.at(nparm + 2 - 1,isim - 1) = xlike;
continue;

line1003: retmat.at(nparm + 2 - 1,isim - 1) = xlike;
ii = nparm + 2;

for(int iii = 1; iii <= nparm; iii++){
  
    for(int jjj = 1; jjj <= iii; jjj++){
      
        ii = ii + 1;
        retmat.at(ii - 1,isim - 1) = vcv.at(iii - 1,jjj - 1);
        
    }
           
}
         
}

// Record the number of rows coming back
if(iret == 1) nrowr = nparm + 1; // theta and error only
if(iret == 2) nrowr = nparm + 2; // theta, error, and like
if(iret == 3) nrowr = nparm + (nparm) * (nparm + 1) / 2 + 2; // theta, like, and vcv, and error
if(iret == 4) nrowr = nparm + 3; // theta and like

if(debug::kprint >= 2){
  
   Rcpp::Rcout << "\nFinal iersim = " << iersim << std::endl;
   
}

if(numret != nrowr) iersim = 4;

exit:

Rcpp::List ints = Rcpp::List::create(Named("kdist") = kdist,
                                     Named("nrow") = nrow,
                                     Named("nrowr") = nrowr,
                                     Named("nter") = nter,
                                     Named("ny") = ny,
                                     Named("nty") = nty,
                                     Named("nparm") = nparm,
                                     Named("intcpt") = intcpt,
                                     Named("maxit") = maxit,
                                     Named("kprint") = kprint,
                                     Named("marray") = marray,
                                     Named("iret") = iret,
                                     Named("numsim") = numsim,
                                     Named("numret") = numret,
                                     Named("iersim") = iersim,
                                     Named("ierfit") = ierfit,
                                     Named("iervcv") = iervcv,
                                     Named("method") = method);

Rcpp::List doubs = Rcpp::List::create(Named("escale") = escale,
                                      Named("xlike") = xlike);

Rcpp::List bools = Rcpp::List::create(Named("lrand") = lrand,
                                      Named("lcheck") = lcheck);

Rcpp::List intvec = Rcpp::List::create(Named("cen") = cen,
                                       Named("wt") = wt,
                                       Named("tcodes") = tcodes,
                                       Named("iscrat") = iscrat,
                                       Named("iarray") = iarray,
                                       Named("wtnew") = wtnew,
                                       Named("iir") = iir,
                                       Named("ijc") = ijc);

Rcpp::List numvec = Rcpp::List::create(Named("gamthr") = gamthr,
                                       Named("e") = e,
                                       Named("thetah") = thetah,
                                       Named("fsder") = fsder,
                                       Named("dscrat") = dscrat,
                                       Named("ipthb") = ipthb,
                                       Named("ipthg") = ipthg,
                                       Named("ipfsd") = ipfsd,
                                       Named("ipnext") = ipnext,
                                       Named("itd") = itd,
                                       Named("itf") = itf,
                                       Named("ied") = ied,
                                       Named("iw") = iw,
                                       Named("ivd") = ivd,
                                       Named("fv") = fv,
                                       Named("theta") = theta,
                                       Named("iprv1") = iprv1,
                                       Named("ipdiag") = ipdiag,
                                       Named("tspass") = tspass);

Rcpp::List nummat = Rcpp::List::create(Named("x") = x,
                                       Named("y") = y,
                                       Named("ty") = ty,
                                       Named("devian") = devian,
                                       Named("vcv") = vcv,
                                       Named("r") = r,
                                       Named("res") = res,
                                       Named("xnew") = xnew,
                                       Named("ynew") = ynew,
                                       Named("retmat") = retmat,
                                       Named("ipxnew") = ipxnew,
                                       Named("iptmat") = iptmat,
                                       Named("ipvcvb") = ipvcvb,
                                       Named("ipvcvg") = ipvcvg,
                                       Named("ivcvd") = ivcvd,
                                       Named("ivcvdd") = ivcvdd);

Rcpp::List logvec = Rcpp::List::create(Named("lfix") = lfix);

return Rcpp::List::create(Named("ints") = ints,
                          Named("doubs") = doubs,
                          Named("bools") = bools,
                          Named("intvec") = intvec,
                          Named("numvec") = numvec,
                          Named("logvec") = logvec,
                          Named("nummat") = nummat);
         
}