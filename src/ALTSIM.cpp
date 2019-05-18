#include <base/base.hpp>
#include <altsim/altsim.hpp>
#include <altsim/wqm_simalt.hpp>
#include <wqmmlesss/wqm_mlboth.hpp>
#include <utility/copyrr.hpp>

//' Parametric accelerated life test simulation
//' 
//' @name ALTSIM
//' 
//' @rdname altsim_cpp
//' 
//' @description Parametric accelerated life test simulation (spec censoring)
//' 
//' @details Parameter meanings same as in wqm_mlboth except those listed here
//'
//' @note Modified 19 December 1997 to account for multiple accelerating variables
//'
//' @param theta (nparm) true value of theta
//' @param thetah (nparm) space for thetahat
//' @param xnew hold space for x
//' @param ynew hold space for y
//' @param centim (nsubex) vector of censor times for alt
//' @param acvar (nsubex,nacvar) matrix of accelerating vars
//' @param Accelerating Variable - X1, X1, Comb1, Comb2, Comb3
//' @param nsubex number of subexperiments
//' @param nacvar number of accelerating variables
//' @param nsamsz (nsubex) vector giving number of units at each comb
//' @param kctype = 1 for type 1 at centim, = 2 for type 2 with centim failures
//' @param retmat (numret,numsim) matrix to return results, one result per col
//' @param numsim number of simulations to be run
//' @param nrowr number of rows being returned in retmat
//' @param iersim > 0 if data space too small
// [[Rcpp::export]]
Rcpp::List ALTSIM(Rcpp::NumericMatrix x,
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
                  Rcpp::NumericMatrix xnew,
                  Rcpp::NumericMatrix ynew,
                  Rcpp::NumericVector centim,
                  Rcpp::NumericMatrix acvar,
                  int nsubex,
                  int nacvar,
                  Rcpp::IntegerVector nsamsz,
                  Rcpp::IntegerVector krfail,
                  int kctype,
                  Rcpp::NumericMatrix retmat,
                  int numret,
                  int numsim,
                  int iersim){
  
debug::kprint = kprint;
  
bool lcheck;
Rcpp::List SIMALT;
int ii,nrowr,iervcv,ierfit;
int nrownw,kpred,inext;
Rcpp::List ints,doubs,bools,numvec,intvec,nummat,logvec;
Rcpp::NumericMatrix ipxnew,iptmat,ipvcvb,ipvcvg,ivcvd,ivcvdd;
Rcpp::NumericVector iprv1,ipdiag,ipthb,ipthg,ipfsd,ipnext;
Rcpp::NumericVector itd,itf,ied,iw,ivd;
Rcpp::IntegerVector iir, ijc;
double xlike;

if(debug::kprint >= 2) {
  
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
   Rcpp::Rcout << "thetareal = " << theta << std::endl;
   Rcpp::Rcout << "centim = " << centim << std::endl;
   Rcpp::Rcout << "kctype = " << kctype << std::endl;
   Rcpp::Rcout << "nacvar = " << nacvar << std::endl;
   Rcpp::Rcout << "nsubexr = " << nsubex << std::endl;
   Rcpp::Rcout << "acvar = " << acvar << std::endl;
   Rcpp::Rcout << "nsamsz = " << nsamsz << std::endl;
   Rcpp::Rcout << "numret = " << numret << std::endl;
   Rcpp::Rcout << "numsim = " << numsim << std::endl;

}

// Program to simulate survival data

// Fix up things to keep this version simple
   lcheck = false;
   // nvcv = (nparm) * (nparm + 1) / 2;

// Simulate the data and record failure times
for(int isim = 1; isim <= numsim; isim++){
  
    SIMALT = wqm_simalt(theta,nparm,intcpt,nsamsz,krfail,kctype,
                        centim,acvar,nsubex,nacvar,kdist,
                        x,y,cen,wt,nrow,nter,ny,nty,ty,tcodes,
                        nrownw,iersim,kprint);
  
    iersim = Rcpp::as<int>(Rcpp::as<List>(SIMALT)["iersim"]);
    nrownw = Rcpp::as<int>(Rcpp::as<List>(SIMALT)["nrownw"]);
    inext  = Rcpp::as<int>(Rcpp::as<List>(SIMALT)["inext"]);
    kpred  = Rcpp::as<int>(Rcpp::as<List>(SIMALT)["kpred"]);
    x      = Rcpp::as<NumericMatrix>(Rcpp::as<List>(SIMALT)["x"]);
    y      = Rcpp::as<NumericMatrix>(Rcpp::as<List>(SIMALT)["y"]);
    cen    = Rcpp::as<IntegerVector>(Rcpp::as<List>(SIMALT)["cen"]);
    wt     = Rcpp::as<IntegerVector>(Rcpp::as<List>(SIMALT)["wt"]);
    
// Copy over x and y (because they may have been destroyed)
// xx call realpt('x before copyr =$',-1,x, nrownw*nter)
// print out simulation number to see progress;
if(debug::kprint >= 3) {
  
   Rcpp::Rcout << "\nx(i,j) = \n" << x << std::endl;  
  
}

if((debug::kprint >=3) & ((isim % 50) == 1)){
  
    Rcpp::Rcout << "Beginning simulation number = " << isim << std::endl;
  
}

// copyrr adjusts for the different number of rows in x and xnew;
   copyrr(x,xnew,nter,nrow,nrownw);
   copyrr(y,ynew,ny,nrow,nrownw);

// Get start values from true parameters
   for(int i = 1; i <= nparm; i++){
     
       thetah.at(i - 1) = theta.at(i - 1);
     
   }
   
if(debug::kprint >= 2) {
  
   Rcpp::Rcout << "nrownw = " << nrownw << std::endl;
   Rcpp::Rcout << "ynew = " << ynew << std::endl;
   Rcpp::Rcout << "wt = " << wt << std::endl;
   Rcpp::Rcout << "cen = " << cen << std::endl;
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
   Rcpp::Rcout << "theta start values = " << theta << std::endl;

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

   wqm_mlboth(x,y,cen,wt,nrownw,nter,ny,nty,ty,tcodes,
              kdist,gamthr,lfix,lcheck,nparm,intcpt,escale,
              e,maxit,dscrat,iscrat,xlike,devian,
              thetah,fsder,vcv,r,res,fv,ierfit,iervcv,
              ipxnew,iprv1,ipdiag,iptmat,
              ipthb,ipthg,ipfsd,ipvcvb,ipvcvg,ipnext,itd,itf,
              ied,iw,ivd,ivcvd,ivcvdd,iir,ijc);
   
// Debug print
if(debug::kprint >= 3) {
  
   Rcpp::Rcout << "xlike = " << xlike << std::endl;
   Rcpp::Rcout << "devian = " << devian << std::endl;
   Rcpp::Rcout << "thetah = " << thetah << std::endl;
   Rcpp::Rcout << "fsder = " << fsder << std::endl;
   Rcpp::Rcout << "vcv = " << vcv << std::endl;
   Rcpp::Rcout << "r = " << r << std::endl;
   Rcpp::Rcout << "res = " << res << std::endl;
   Rcpp::Rcout << "fv = " << fv << std::endl;
   Rcpp::Rcout << "ierfit = " << ierfit << std::endl;
   Rcpp::Rcout << "iervcv = " << iervcv << std::endl;

}
// Save the results 

// Save thetah and error
   for(int i = 1; i <= nparm; i++){
     
       retmat.at(i, isim - 1) = thetah.at(i - 1);
     
   }
   
retmat.at(0,isim - 1) = 10 * iervcv + ierfit;

// Save likelihood and vcv
retmat.at(nparm + 2 - 1,isim - 1) = xlike;

ii = nparm + 2;

for(int iii = 1; iii <= nparm; iii++){
  
    for(int jjj = 1; jjj <= iii; jjj++){
      
        ii = ii + 1;
        retmat(ii - 1,isim - 1) = vcv(iii - 1,jjj - 1);
    
    }

}

}

// Record the number of rows coming back
   nrowr = nparm + (nparm) * (nparm + 1) / 2 + 2;
   if(numret != nrowr) iersim = 4;
   
      ints = Rcpp::List::create(Named("numret") = numret, 
                                Named("iersim") = iersim,
                                Named("nparm") = nparm,
                                Named("intcpt") = intcpt,
                                Named("maxit") = maxit,
                                Named("numsim") = numsim,
                                Named("kdist") = kdist,
                                Named("nrow") = nrow,
                                Named("nter") = nter,
                                Named("ny") = ny,
                                Named("nty") = nty,
                                Named("nrownw") = nrownw,
                                Named("kpred") = kpred,
                                Named("inext") = inext);

      doubs = Rcpp::List::create(Named("xlike") = xlike,
                                 Named("escale") = escale);
      
      bools = Rcpp::List::create(Named("lcheck") = lcheck);
      
      intvec = Rcpp::List::create(Named("tcodes") = tcodes,
                                  Named("krfail") = krfail,
                                  Named("cen") = cen,
                                  Named("wt") = wt,
                                  Named("nsamsz") = nsamsz);
      
      numvec = Rcpp::List::create(Named("thetah") = thetah,
                                  Named("fsder") = fsder,
                                  Named("fv") = fv,
                                  Named("theta") = theta,
                                  Named("centim") = centim,
                                  Named("e") = e,
                                  Named("gamthr") = gamthr);
      
      logvec = Rcpp::List::create(Named("lfix") = lfix);
      
      nummat = Rcpp::List::create(Named("retmat") = retmat,
                                  Named("x") = x,
                                  Named("y") = y,
                                  Named("devian") = devian,
                                  Named("vcv") = vcv,
                                  Named("r") = r,
                                  Named("ty") = ty,
                                  Named("res") = res);
   
return Rcpp::List::create(Named("ints") = ints,
                          Named("doubs") = doubs,
                          Named("bools") = bools,
                          Named("numvec") = numvec,
                          Named("intvec") = intvec,
                          Named("logvec") = logvec,
                          Named("nummat") = nummat);


}

#include <base/base.hpp>
#include <utility/wqm_quant.hpp>
#include <mlsim2/smldat.hpp>

//' Simulate the alt data
// [[Rcpp::export]]
Rcpp::List wqm_simalt(Rcpp::NumericVector &theta,
                      int &nparm,
                      int &intcpt,
                      Rcpp::IntegerVector &nsamsz,
                      Rcpp::IntegerVector &krfail,
                      int &kctype,
                      Rcpp::NumericVector &centim,
                      Rcpp::NumericMatrix &acvar,
                      int &nsubex,
                      int &nacvar,
                      int &kdist,
                      Rcpp::NumericMatrix &x,
                      Rcpp::NumericMatrix &y,
                      Rcpp::IntegerVector &cen,
                      Rcpp::IntegerVector &wt,
                      int &nrow,
                      int &nter,
                      int &ny,
                      int &nty,
                      Rcpp::NumericMatrix &ty,
                      Rcpp::IntegerVector &tcodes,
                      int &nrownw,
                      int &iersim,
                      int &kprint){
  
debug::kprint = kprint;

Rcpp::NumericVector thetax = Rcpp::NumericVector(2);
double pretim = 0.0e00;
double zero = 0.0e00;
int nparmx = 2, kpred;
iersim = 0;

// initilize the counter/pointer
   nrownw = 0;
   int nrownx = 0;
   int inext = 1;
   int nrlft, ixri, ixci;

// loop over the sub experiments;
for(int isub = 1; isub <= nsubex; isub++){

    // Store location and scale in
    // get mu for the group
        thetax.at(0) = zero;
        if(intcpt == 1) thetax.at(0) = theta.at(0);
    
        for(int jvar = 1; jvar <= nacvar; jvar++){
    
    
            thetax.at(0) = thetax.at(0) + theta.at(jvar - 1 + intcpt) * acvar.at(isub - 1,jvar - 1);
    
        }

    thetax.at(1) = theta.at(nparm - 1);

    // get number of rows that are left
       nrlft = nrow - inext + 1;

    // get the single dist sample
       smldat(thetax,nparmx,nsamsz,kctype,centim,
              pretim,kdist,x,y,cen,wt,nrlft,nter,
              ny,nty,ty,tcodes,nrownx,krfail,isub,
              inext,kpred,iersim);
    
    if(nrownx == 0) continue;

    // Set the explan vars
    for(int iobs = 1; iobs <= nrownx; iobs++){

        if(intcpt == 1) x.at(inext + iobs - 2, 0) = 1.0e00;

        for(int jvar = 1; jvar <= nacvar; jvar++){

            ixri = inext + iobs - 1;
            ixci = jvar + intcpt;
            x.at(ixri - 1, ixci - 1) = acvar.at(isub - 1, jvar - 1);

        }

    }

    // Set the pointer/counter
       inext = inext + nrownx;
       nrownw = nrownw + nrownx;

}

return Rcpp::List::create(Named("iersim") = iersim,
                          Named("nrownw") = nrownw,
                          Named("cen")    = cen,
                          Named("wt")     = wt,
                          Named("x")      = x,
                          Named("y")      = y,
                          Named("inext")  = inext,
                          Named("krfail") = krfail,
                          Named("kpred")  = kpred);

}