#include <base/base.hpp>
#include <mlsim2/msmdat1.hpp>
#include <wqmmrr2/mrr2.hpp>
#include <wqmmlesss/wqm_mlboth.hpp>

// staggered entry simulation to compare mle and mmr staggered_mle_mmr.f;
// #;
// #Multiple Time Censoring;
// #;
// #krfail number of failures observed in each simulation;
// #;
// #;
// #thetah(nparm) space for thetahat;
// #;
// # parameter meanings same as in mlboth except the following:;
// #;
// # can get rid of scalars like iervcv,ierfit,xlike;
// #;
// #theta(nparm) true value of theta;
// #;
// #retmat(numret,numsim) matrix to return the results, one result per row;
// #;
// #numsim number of simulations to be run;
// #;
// #prdelt time increment for prediction calibration;
// #;
// #ngroup number of groups;
// #;
// #centim(ngroup) censoring time for each group;
// #;
// #nsamsz(ngroup) sample size in each group;
// #;
// #nmrvec(ngroup) number that failed in each group (return);
// #;
// #nsimg returns the number of good simulations;
// #;
// #numret = 5 (number of columns of retmat);
// #;
// #nnomle returns the number of simulations with no mle's;
// #;
// #iersim data space too small or fewer than 2 failures;
// #;
// # need to send down data space big enough to cover all data situations;
// [[Rcpp::export]]
Rcpp::List MLSIM7(Rcpp::NumericMatrix x,
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
                  Rcpp::IntegerVector krfail,
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
                  Rcpp::NumericMatrix retmat,
                  int numsim,
                  double prdelt,
                  int ngroup,
                  Rcpp::NumericVector centim,
                  Rcpp::IntegerVector nsamsz,
                  Rcpp::IntegerVector nmrvec,
                  int nsimg,
                  int numret,
                  int nnomle,
                  int iersim){

debug::kprint = kprint;
   
bool lcheck;
double anslow,ansup,pquan,xlike;
int nwhich,nvcv,nrownw = 0,nfail;
int itry2, itry10;
int iervcv, ierfit, ierror;
int kpredt;
Rcpp::NumericVector thetamrr = Rcpp::NumericVector(2);
Rcpp::List ints,doubs,bools,numvec,intvec,nummat,logvec;
Rcpp::NumericMatrix ipxnew,iptmat,ipvcvb,ipvcvg,ivcvd,ivcvdd;
Rcpp::NumericVector iprv1,ipdiag,ipthb,ipthg,ipfsd,ipnext;
Rcpp::NumericVector itd,itf,ied,iw,ivd;
Rcpp::IntegerVector iir, ijc;
Rcpp::IntegerVector ikey,ikrevrank,iwtnew;
Rcpp::IntegerVector irorder,icenstar,icenstarn;
Rcpp::NumericVector iynew,ixstar,ix,irmdrank;

if(debug::kprint >= 2) {
  
   Rcpp::Rcout << "nrownw = " << nrownw << std::endl;
   Rcpp::Rcout << "nter = " << nter << std::endl;
   Rcpp::Rcout << "ny = " << ny << std::endl;
   Rcpp::Rcout << "nty = " << nty << std::endl;
   Rcpp::Rcout << "kdist = " << kdist << std::endl;
   Rcpp::Rcout << "lfix = " << lfix << std::endl;
   Rcpp::Rcout << "nparm = " << nparm << std::endl;
   Rcpp::Rcout << "intcpt = " << intcpt << std::endl;
   Rcpp::Rcout << "escale = " << escale << std::endl;
   Rcpp::Rcout << "e = " << e << std::endl;
   Rcpp::Rcout << "maxit = " << maxit << std::endl;
   Rcpp::Rcout << "kprint = " << kprint << std::endl;
   Rcpp::Rcout << "thetahat = " << thetah << std::endl;
   Rcpp::Rcout << "fsder = " << fsder << std::endl;
   Rcpp::Rcout << "vcv = " << vcv << std::endl;
   Rcpp::Rcout << "r = " << r << std::endl;
   Rcpp::Rcout << "thetareal = " << theta << std::endl;
   Rcpp::Rcout << "retmat = " << retmat << std::endl;
   Rcpp::Rcout << "numsim = " << numsim << std::endl;
   Rcpp::Rcout << "prdelt = " << prdelt << std::endl;
   Rcpp::Rcout << "ngroup = " << ngroup << std::endl;
   Rcpp::Rcout << "centim = " << centim << std::endl;
   Rcpp::Rcout << "nsamsz = " << nsamsz << std::endl;
   Rcpp::Rcout << "krfail = " << krfail << std::endl;
   Rcpp::Rcout << "numret = " << numret << std::endl;
   Rcpp::Rcout << "iersim = " << iersim << std::endl;
   Rcpp::Rcout << "numsim = " << numsim << std::endl;
   
   if(debug::kprint >= 4) {
  
      Rcpp::Rcout << "\nx = \n" << x << std::endl;
      Rcpp::Rcout << "y = \n" << y << std::endl;
      Rcpp::Rcout << "cen = " << cen << std::endl;
      Rcpp::Rcout << "weights = " << wt << std::endl;
      Rcpp::Rcout << "res = " << res << std::endl;
      Rcpp::Rcout << "fv = \n" << fv << std::endl;
      Rcpp::Rcout << "devian = \n" << devian << std::endl;
      Rcpp::Rcout << "ty = \n" << ty << std::endl;
      Rcpp::Rcout << "tcodes = " << tcodes << std::endl;
      Rcpp::Rcout << "gamthr = " << gamthr << std::endl;

   }

}

// Fix up things to keep this version simple
   for(int i = 1; i <= nrow; i++) {
     
       gamthr.at(i - 1) = 0.0e00;
       x.at(i - 1, 0) = 1.0e00;
     
   }
   
   nnomle = 0;
   ny = 1;

// nsimg returns the number of good simulations
// but brings down which alpha to return the naive pred bounds's
   nwhich = nsimg;
   nty = 1;
   nter = 1;
   nparm = 2;
   lfix.at(0) = false;
   lfix.at(1) = false;
   lcheck = false;
   intcpt = 1;
   nvcv = (nparm) * (nparm + 1) / 2;
   nsimg = 0;

// Do the numsim bootstrap simulations
   for(int isim = 1; isim <= numsim; isim++){

// Print progress information;
   if((debug::kprint > 0) & ((isim % 50) == 1)) {
     
       Rcpp::Rcout << "\nBeginning simulation number = " << isim << std::endl;
     
   }

// Generate a staggered entry data set
   msmdat1(theta,nparm,nsamsz,centim,kdist,x,y,cen,
           wt,nrow,nter,ny,nty,ty,tcodes,krfail,ngroup,
           nrownw,prdelt,kpredt,iersim);

// kprint = 4;

if(debug::kprint >= 2) {
  
   Rcpp::Rcout << "\nSIMDAT**i,y,c,w\n" << std::endl;
  
   for(int i = 1; i <= nrownw; i++){
     
       Rcpp::Rcout << "i" << i - 1 << std::endl;
       Rcpp::Rcout << "y(i,1)" << y(i - 1,0) << std::endl;
       Rcpp::Rcout << "cen(i)" << cen(i - 1) << std::endl;
       Rcpp::Rcout << "wt(i)" << wt(i - 1) << std::endl;
   
   }

}

// Check to see if there was only one or 0 failures
   nfail = 0;

   for(int i = 1; i <= nrownw; i++){
     
       if(cen.at(i - 1) < 1.5) nfail = nfail + 1;
   
   }
   
   ierror = 0;
   if(nfail < 2) ierror = 200;
   
if(ierror <= 0) {

   // Check to see if there was enough space, if not return
      if(iersim > 0) goto exit;
   
   // Get start values from true parameters
      for(int i = 1; i <= nparm; i++){
        
          thetah.at(i - 1) = theta.at(i - 1);
        
      }
   
   if(debug::kprint >= 3){
    
      Rcpp::Rcout << "\nthetah start\n" <<  std::endl;
      Rcpp::Rcout << "\nthetah = " << thetah << std::endl;  
      
   }
   
   // Fit ML
      if(debug::kprint > 0) {
      
         Rcpp::NumericVector ycol = y.column(0);
         Rcpp::Rcout << "\ny col 0 = \n" << ycol << std::endl;
         Rcpp::Rcout << "cen = "         << cen  << std::endl;          
         Rcpp::Rcout << "wt = "          << wt   << std::endl;          
      
      }

   itry2  = 2;
   itry10 = 10;
   // if(itry2 < itry10) goto line1587;
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
   
      ierror = 10 * iervcv + ierfit;

}

if(debug::kprint >= 3) {
  
   Rcpp::Rcout << "\nierror check\n"    << std::endl;
   Rcpp::Rcout << "isim = "   << isim   << std::endl;
   Rcpp::Rcout << "nsimg = "  << nsimg  << std::endl;
   Rcpp::Rcout << "nfail = "  << nfail  << std::endl;
   Rcpp::Rcout << "ierror = " << ierror << std::endl;
   Rcpp::Rcout << "itry2 = "  << itry2  << std::endl;
   Rcpp::Rcout << "itry10 = " << itry10 << std::endl; 
            
}

retmat.at(0,isim - 1) = ierror;

if(debug::kprint >= 3){
  
   Rcpp::Rcout << "\ntheta ml = " << thetah << std::endl;
  
}

// Initialize values for mrr2
   ikey      = Rcpp::IntegerVector(nrownw);
   ikrevrank = Rcpp::IntegerVector(nrownw);
   iynew     = Rcpp::NumericVector(nrownw);
   irmdrank  = Rcpp::NumericVector(nrownw);
   icenstar  = Rcpp::IntegerVector(nrownw);
   ixstar    = Rcpp::NumericVector(nrownw);
   ix        = Rcpp::NumericVector(nrownw);
   irorder   = Rcpp::IntegerVector(nrownw);
   icenstarn = Rcpp::IntegerVector(nrownw);
   iwtnew    = Rcpp::IntegerVector(nrownw);

//line1587 continue;

if(ierror > 0) {

   // Count the bad ones and get out if there is an error
      nnomle = nnomle + 1;
      goto line5556;

   // Save the results
   // Save error and thetah
   
}

// Add to the good count
   nsimg = nsimg + 1;
   retmat.at(1,isim - 1) = thetah.at(0);
   retmat.at(2,isim - 1) = thetah.at(1);

// Do mrr
   for(int i = 1; i <= nrownw; i++){
     
       y.at(i - 1,0) = std::exp(y.at(i - 1,0));
     
   }

// if(itry2 < itry10) goto line1588;
   mrr2(y,cen,wt,iscrat, dscrat, nrownw, thetamrr,
       ikey,ikrevrank,iynew,irmdrank,icenstar,ixstar,
       ix,irorder,icenstarn,iwtnew);
   
   if(debug::kprint >= 3){
     
      Rcpp::Rcout << "theta mmr = " << thetamrr <<std::endl;

   }
   
// line1588 continue;

retmat.at(3,isim - 1) = std::log(thetamrr.at(0));
retmat.at(4,isim - 1) = 1 / thetamrr.at(1);

line5556: if(debug::kprint >= 1){
  
             Rcpp::Rcout << "\nmlsim7**\n" << std::endl;
             Rcpp::Rcout << "isim = " << isim - 1 << std::endl;
             Rcpp::Rcout << "nsimg = " <<  nsimg - 1<< std::endl;  
             Rcpp::Rcout << "ngroup = " << ngroup  << std::endl;  
             Rcpp::Rcout << "kpredt = " << kpredt  << std::endl;  
             Rcpp::Rcout << "nmrvec(0) = " <<  nmrvec.at(0) << std::endl;  
             Rcpp::Rcout << "retmat(1,nsimg) = " <<  retmat.at(1, nsimg - 1) << std::endl;  
             Rcpp::Rcout << "retmat(2,nsimg) = " <<  retmat.at(2, nsimg - 1) << std::endl;  
  
          }

   }

if(debug::kprint >= 2) {
  
   Rcpp::Rcout << "nrownw = " << nrownw << std::endl;
   Rcpp::Rcout << "nter = " << nter << std::endl;
   Rcpp::Rcout << "ny = " << ny << std::endl;
   Rcpp::Rcout << "nty = " << nty << std::endl;
   Rcpp::Rcout << "kdist = " << kdist << std::endl;
   Rcpp::Rcout << "lfix = " << lfix << std::endl;
   Rcpp::Rcout << "nparm = " << nparm << std::endl;
   Rcpp::Rcout << "intcpt = " << intcpt << std::endl;
   Rcpp::Rcout << "escale = " << escale << std::endl;
   Rcpp::Rcout << "e = " << e << std::endl;
   Rcpp::Rcout << "maxit = " << maxit << std::endl;
   Rcpp::Rcout << "kprint = " << kprint << std::endl;
   Rcpp::Rcout << "thetahat = " << thetah << std::endl;
   Rcpp::Rcout << "fsder = " << fsder << std::endl;
   Rcpp::Rcout << "vcv = " << vcv << std::endl;
   Rcpp::Rcout << "r = " << r << std::endl;
   Rcpp::Rcout << "thetareal = " << theta << std::endl;
   Rcpp::Rcout << "retmat = " << retmat << std::endl;
   Rcpp::Rcout << "numsim = " << numsim << std::endl;
   Rcpp::Rcout << "prdelt = " << prdelt << std::endl;
   Rcpp::Rcout << "ngroup = " << ngroup << std::endl;
   Rcpp::Rcout << "centim = " << centim << std::endl;
   Rcpp::Rcout << "nsamsz = " << nsamsz << std::endl;
   Rcpp::Rcout << "krfail = " << krfail << std::endl;
   Rcpp::Rcout << "numret = " << numret << std::endl;
   Rcpp::Rcout << "iersim = " << iersim << std::endl;
   Rcpp::Rcout << "numsim = " << numsim << std::endl;
   
   if(debug::kprint >= 4) {
  
      Rcpp::Rcout << "\nx = \n" << x << std::endl;
      Rcpp::Rcout << "y = \n" << y << std::endl;
      Rcpp::Rcout << "cen = " << cen << std::endl;
      Rcpp::Rcout << "weights = " << wt << std::endl;
      Rcpp::Rcout << "res = " << res << std::endl;
      Rcpp::Rcout << "fv = \n" << fv << std::endl;
      Rcpp::Rcout << "devian = \n" << devian << std::endl;
      Rcpp::Rcout << "ty = \n" << ty << std::endl;
      Rcpp::Rcout << "tcodes = " << tcodes << std::endl;
      Rcpp::Rcout << "gamthr = " << gamthr << std::endl;

   }

}

exit: ints = Rcpp::List::create(Named("nsimg") = nsimg, 
                                Named("numret") = numret, 
                                Named("nnomle") = nnomle, 
                                Named("iersim") = iersim,
                                Named("nparm") = nparm,
                                Named("intcpt") = intcpt,
                                Named("maxit") = maxit,
                                Named("numsim") = numsim,
                                Named("ngroup") = ngroup,
                                Named("kdist") = kdist,
                                Named("nrow") = nrow,
                                Named("nter") = nter,
                                Named("ny") = ny,
                                Named("nty") = nty,
                                Named("nwhich") = nwhich,
                                Named("nvcv") = nvcv,
                                Named("nrownw") = nrownw,
                                Named("nfail") = nfail);

      doubs = Rcpp::List::create(Named("prdelt") = prdelt,
                                 Named("anslow") = anslow,
                                 Named("ansup") = ansup,
                                 Named("pquan") = pquan,
                                 Named("xlike") = xlike,
                                 Named("escale") = escale);
      
      bools = Rcpp::List::create(Named("lcheck") = lcheck);
      
      intvec = Rcpp::List::create(Named("tcodes") = tcodes,
                                  Named("krfail") = krfail,
                                  Named("cen") = cen,
                                  Named("wt") = wt,
                                  Named("nsamsz") = nsamsz, 
                                  Named("nmrvec") = nmrvec);
      
      numvec = Rcpp::List::create(Named("thetah") = thetah,
                                  Named("fsder") = fsder,
                                  Named("fv") = fv,
                                  Named("theta") = theta,
                                  Named("centim") = centim,
                                  Named("e") = e,
                                  Named("gamthr") = gamthr,
                                  Named("thetamrr") = thetamrr);
      
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