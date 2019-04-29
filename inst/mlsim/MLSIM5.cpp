#include <base/base.h>

//' staggered entry prediction bootstrap for a new sample
//' 
//' 
//' thetah(nparm)     space for thetahat
//' 
//'     parameter meanings same as in wqm_mlboth except the following:
//' 
//'     can get rid of scalars like iervcv,ierfit,xlike
//' 
//' theta(nparm)   true value of theta
//' 
//' krfail  number of failures observed in each simulation
//' numsim  number of simulations to be run
//' 
//' nsamsz          sample size vector
//' 
//' centim  censoring time vector
//' 
//' retmat(numret,numsim)  matrix to return the results, one result per row
//' 
//' nrowr   number of columns being returned in retmat
//' 
//' nnomle     returns the number of boots with no mle's
//' 
//' clevc(nclev)     vector of clev_cal values
//' 
//' nclev
//' 
//' cpl(nclev)        lower coverage probs
//' 
//' cpu(nclev)        upper coverage probs
//' 
//' iersim  data space too small
//' 
//'    need to send down data space big enough to cover all data situations

// [[Rcpp::export]]
Rcpp::List mlsim4(Rcpp::NumericMatrix x,
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
                  int ngroup,
                  Rcpp::NumericVector centim,
                  Rcpp::IntegerVector nsamsz,
                  Rcpp::NumericVector clevc,
                  Rcpp::NumericVector cpl,
                  Rcpp::NumericVector cpu,
                  Rcpp::NumericVector cpl2,
                  Rcpp::NumericVector cpu2,
                  int nclev,
                  int nsimg,
                  int numret,
                  int nnomle,
                  int iersim){

debug::kprint = kprint;
bool lcheck;
double anslow,ansup,pquan,xlike,prdelt;
int nvcv,nfail;
Rcpp::NumericMatrix ipxnew,iptmat,ipvcvb,ipvcvg,ivcvd,ivcvdd;
Rcpp::NumericVector iprv1,ipdiag,ipthb,ipthg,ipfsd,ipnext;
Rcpp::NumericVector itd,itf,ied,iw,ivd;
Rcpp::IntegerVector iir, ijc;

if(debug::kprint >= 2){
   
   if(debug::kprint >= 4){
      
      Rcpp::Rcout << "x = " << x << std::endl
      Rcpp::Rcout << "y = " << y << std::endl
      Rcpp::Rcout << "cen = " << cen << std::endl
      Rcpp::Rcout << "weights = " << wt << std::endl
      Rcpp::Rcout << "ty = " << ty << std::endl
      Rcpp::Rcout << "tcodes = " << tcodes << std::endl
      Rcpp::Rcout << "gamthr = " << gamthr << std::endl
      Rcpp::Rcout << "devian = " << devian << std::endl
      Rcpp::Rcout << "res = " << res << std::endl
      Rcpp::Rcout << "fv = " << fv << std::endl
         
   }

   Rcpp::Rcout << "nrow = "  << nrownw << std::endl;
   Rcpp::Rcout << "nter = "  << nter << std::endl;
   Rcpp::Rcout << "ny = "  << ny << std::endl;
   Rcpp::Rcout << "nty = "  << nty << std::endl;
   Rcpp::Rcout << "kdist = "  << kdist << std::endl;
   Rcpp::Rcout << "lfix = "  << lfix << std::endl;
   Rcpp::Rcout << "nparm = "  << nparm << std::endl;
   Rcpp::Rcout << "intcpt = "  << intcpt << std::endl;
   Rcpp::Rcout << "escale = "  << escale << std::endl;
   Rcpp::Rcout << "e = "  << e << std::endl;
   Rcpp::Rcout << "maxit = "  << maxit << std::endl;
   Rcpp::Rcout << "kprint = "  << kprint << std::endl;
   Rcpp::Rcout << "thetahat = "  << thetah << std::endl;
   Rcpp::Rcout << "fsder = "  << fsder << std::endl;
   Rcpp::Rcout << "vcv = "  << vcv << std::endl;
   Rcpp::Rcout << "r = "  << r << std::endl;
   Rcpp::Rcout << "theta = "  << theta << std::endl;
   Rcpp::Rcout << "retmat = "  << retmat << std::endl;
   Rcpp::Rcout << "numsim = "  << numsim << std::endl;
   Rcpp::Rcout << "ngroup = "  << ngroup << std::endl;
   Rcpp::Rcout << "centim = "  << centim << std::endl;
   Rcpp::Rcout << "nsamsz = "  << nsamsz << std::endl;
   Rcpp::Rcout << "krfail = "  << krfail << std::endl;
   Rcpp::Rcout << "numret = "  << numret << std::endl;
   Rcpp::Rcout << "iersim = "  << iersim << std::endl;
         
}

// Fix  up things to keep this version simple
   wqm_filld(0.0e00,gamthr,1,nrow);
   Rcpp::NumericVector xcol0 = Rcpp::NumericVector(nrow,1);
   x.column(0) = xcol0;

// prdelt is a dummy here
   prdelt = 0.0e00; 
   nnomle = 0;
   ny = 1;

// nsimg returns the number of good simulations
// but brings down which alpha to return the naive cb's
   nwhich = nsimg;
   nty = 0;
   nter = 1;
   nparm = 2;
   lfix.at(0) = false;
   lfix.at(1) = false;
   lcheck = false;
   intcpt = 1;
   nvcv = (nparm) * (nparm + 1) / 2;

// Initilize
   for(int iclev = 1; iclev <= nclev; iclev++){
      
       cpl.at(iclev - 1) = zero;
       cpu.at(iclev - 1) = zero;
       cpl2.at(iclev - 1) = zero;
       cpu2.at(iclev - 1) = zero;
      
   }
   
   nsimg = 0;
   
// Do the numsim bootstrap simulations
   for(int isim = 1; isim <= numsim; isim++){
      

       // Print progress information
                if(((isim % 50) == 1) and (debug::kprint >= 1)){
                   
                     Rcpp::Rcout << "\nBeginning simulation number = " << isim << std::endl;
                   
                }

       // Generate a staggered entry data set
          msmdat1(theta,nparm,nsamsz,centim,kdist,x,y,
                  cen,wt,nrow,nter,ny,nty,ty,tcodes,krfail,
                  ngroup,nrownw,prdelt,kpredt,iersim);
                
          if(debug::kprint >= 2) {
             
             Rcpp::NumericVector ycol0 = y.column(0);
             Rcpp::Rcout << "\n simdat**i,y,c,w\n" << std::endl;
             Rcpp::Rcout << "y[,0] = " << ycol0 << std::endl;
             Rcpp::Rcout << "cen = " << cen << std::endl;
             Rcpp::Rcout << "wt = " << wt << std::endl;
             
          }
          
          // Check to see if there were any failures
             nfail = 0;
             for(int i = 1; i <= nrownw; i++){
               
                 icen = cen.at(i - 1);
                 if(icen != 1) continue;
                 iwt = wt.at(i - 1);
                 nfail = nfail + iwt;
                 
             }
             
             if(nfail <= 0){
               
                // Count the bad ones
                    nnomle = nnomle + 1;
                    goto line5556;
               
             }


          // Check to see if there was enough space, if not return
             if(iersim > 0) goto exit;
             
          // Get start values from true parameters
             for(int i = 1; i <= nparm; i++){
                
                 thetah.at(i - 1) = theta.at(i - 1);
                
             }

          // Fit model
          // Should give this a junk number below too, probably
             xlike = 7777.0e00;
             int iervcv = 0;
             int ierfit = 0;
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
          // Save error and thetah
             nsimg = nsimg + 1;
             retmat.at(0,nsimg - 1) = 10 * iervcv + ierfit;
             retmat.at(1,nsimg - 1) = thetah.at(0);
             retmat.at(2,nsimg - 1) = thetah.at(1);
             retmat.at(3,nsimg - 1) = xlike;

             for(int iclev = 1; iclev <= nclev; iclev++){
                
                 // Lower prediction bound
                    pquan = one - clevc.at(iclev - 1);
                
                    if(debug::kprint >= 1){
                       
                       Rcpp::Rcout << "\nMLSIM4 - QCHECK\n" << std::endl;
                       Rcpp::Rcout << "kmeth = " << kmeth << std::endl;
                       Rcpp::Rcout << "ngroup = " << ngroup << std::endl;
                       Rcpp::Rcout << "nmrvec(1) = " << nmrvec.at(0) << std::endl;
                       Rcpp::Rcout << "pquan = " << pquan << std::endl;
                       Rcpp::Rcout << "rhostr(1) = " << rhostr.at(0) << std::endl;
                       
                    }
                    
                    qsbinx(kmeth,pquan,nmrvec,rhostr,ngroup,klower);
                    klower = std::max(0,klower - 1);
                    psbinx(kmeth,klower - 1,nmrvec,rhohat,ngroup,anslow);
                    anslow = one - anslow;
                    cpl.at(iclev - 1) = cpl.at(iclev - 1) + anslow;
                    cpl2.at(iclev - 1) = cpl2.at(iclev - 1) + anslow * anslow;
         
                //  Upper prediction bound
                     pquan = clevc.at(iclev - 1);
                     qsbinx(kmeth,pquan,nmrvec,rhostr,ngroup,kupper);
                     psbinx(kmeth,kupper,nmrvec,rhohat,ngroup,ansup);
                     cpu.at(iclev - 1) = cpu.at(iclev - 1) + ansup;
                     cpu2.at(iclev - 1) = cpu2.at(iclev - 1) + ansup * ansup;
                     
                     if(debug::kprint >= 2){
                        
                        Rcpp::Rcout << "\nMLSIM4: isim,nsim,ialp\n" << std::endl;
                        Rcpp::Rcout << "isim = " << isim - 1 << std::endl;
                        Rcpp::Rcout << "nsimg = " << nsimg << std::endl;
                        Rcpp::Rcout << "iclev = " << iclev - 1 << std::endl;
                        Rcpp::Rcout << "nwhich = " << nwhich << std::endl;
                        Rcpp::Rcout << "klower = " << klower << std::endl;
                        Rcpp::Rcout << "kupper = " << kupper << std::endl;
                        Rcpp::Rcout << "clevc(iclev) = " << clevc.at(iclev - 1) << std::endl;
                        Rcpp::Rcout << "anslow = " << anslow << std::endl;
                        Rcpp::Rcout << "ansup = " << ansup << std::endl;
                        Rcpp::Rcout << "cpl(iclev) = " << cpl.at(iclev - 1) << std::endl;
                        Rcpp::Rcout << "cpu(iclev) = " << cpu.at(iclev - 1) << std::endl;
                        
                     }
                     
                  // return one of the cbs
                     retmat.AT(4,nsimg - 1) = klower;
                     retmat.AT(5,nsimg - 1) = kupper;
                     
                     if(debug::kprint >= 4){
                        
                        Rcpp::Rcout << "\nMLSIM: ATEST\n" << std::endl;
                        Rcpp::Rcout << "isim = " << isim - 1 << std::endl;
                        Rcpp::Rcout << "iclev = " << iclev - 1 << std::endl;
                        Rcpp::Rcout << "kmeth = " << kmeth << std::endl;
                        Rcpp::Rcout << "kpredt = " << kpredt << std::endl;
                        Rcpp::Rcout << "crisk = " << crisk << std::endl;
                        Rcpp::Rcout << "klower = " << klower << std::endl;
                        Rcpp::Rcout << "kupper = " << kupper << std::endl;
                        Rcpp::Rcout << "clevc(iclev) = " << clevc(iclev - 1) << std::endl;
                        Rcpp::Rcout << "anslow = " << anslow << std::endl;
                        Rcpp::Rcout << "ansup = " << ansup << std::endl;
                        
                     }
                        
             }
             
             if(debug::kprint >= 1){
                
                Rcpp::Rcout << "\nMLSIM4: is,km,ng,kp,nmr,r,l,u\n" << std::endl;
                Rcpp::Rcout << "isim = " << isim - 1 << std::endl;
                Rcpp::Rcout << "nsimg = " << nsimg << std::endl;
                Rcpp::Rcout << "kmeth = " << kmeth << std::endl;
                Rcpp::Rcout << "ngroup = " << ngroup << std::endl;
                Rcpp::Rcout << "kpredt = " << kpredt << std::endl;
                Rcpp::Rcout << "nmrvec(1) = " << nmrvec.at(0) << std::endl;
                Rcpp::Rcout << "rhostr(1) = " << rhostr.at(0) << std::endl;
                Rcpp::Rcout << "retmat(2,nsimg) = " << retmat.at(1,nsimg - 1) << std::endl;
                Rcpp::Rcout << "retmat(3,nsimg) = " << retmat.at(2,nsimg - 1) << std::endl;
                
             }
    
}
   
for(int iclev = 1; iclev <= nclev; iclev++){
   
    cpl2.at(iclev - 1) = std::sqrt(((cpl2.at(iclev - 1) - std::pow(cpl.at(iclev - 1),2) / (double)nsimg) / (double)nsimg) / (double)nsimg);
    cpu2.at(iclev - 1) = std::sqrt(((cpu2.at(iclev - 1) - std::pow(cpu.at(iclev - 1),2) / (double)nsimg) / (double)nsimg) / (double)nsimg);
    cpl.at(iclev - 1) = cpl.at(iclev - 1) / (double)nsimg;
    cpu.at(iclev - 1) = cpu.at(iclev - 1) / (double)nsimg;
    
    if(debug::kprint >= 1){
       
       Rcpp::Rcout << "\nMLSIM: ICLEV\n" << std::endl;
       Rcpp::Rcout << "iclev = " << iclev - 1 << std::endl;
       Rcpp::Rcout << "nsimg = " << nsimg << std::endl;
       Rcpp::Rcout << "cpl(iclev) = "  << cpl.at(iclev - 1) << std::endl;
       Rcpp::Rcout << "cpl2(iclev) = " << cpl2.at(iclev - 1) << std::endl;
       Rcpp::Rcout << "cpu(iclev) = "  << cpu.at(iclev - 1) << std::endl;
       Rcpp::Rcout << "cpu2(iclev) = " << cpu2.at(iclev - 1) << std::endl;
       
    }
               
}

if(debug::kprint >= 2){
   
   if(debug::kprint >= 4){
      
      Rcpp::Rcout << "x = " << x << std::endl
      Rcpp::Rcout << "y = " << y << std::endl
      Rcpp::Rcout << "cen = " << cen << std::endl
      Rcpp::Rcout << "weights = " << wt << std::endl
      Rcpp::Rcout << "ty = " << ty << std::endl
      Rcpp::Rcout << "tcodes = " << tcodes << std::endl
      Rcpp::Rcout << "gamthr = " << gamthr << std::endl
      Rcpp::Rcout << "devian = " << devian << std::endl
      Rcpp::Rcout << "res = " << res << std::endl
      Rcpp::Rcout << "fv = " << fv << std::endl
         
   }

   Rcpp::Rcout << "nrow = "  << nrownw << std::endl;
   Rcpp::Rcout << "nter = "  << nter << std::endl;
   Rcpp::Rcout << "ny = "  << ny << std::endl;
   Rcpp::Rcout << "nty = "  << nty << std::endl;
   Rcpp::Rcout << "kdist = "  << kdist << std::endl;
   Rcpp::Rcout << "lfix = "  << lfix << std::endl;
   Rcpp::Rcout << "nparm = "  << nparm << std::endl;
   Rcpp::Rcout << "intcpt = "  << intcpt << std::endl;
   Rcpp::Rcout << "escale = "  << escale << std::endl;
   Rcpp::Rcout << "e = "  << e << std::endl;
   Rcpp::Rcout << "maxit = "  << maxit << std::endl;
   Rcpp::Rcout << "kprint = "  << kprint << std::endl;
   Rcpp::Rcout << "thetahat = "  << thetah << std::endl;
   Rcpp::Rcout << "fsder = "  << fsder << std::endl;
   Rcpp::Rcout << "vcv = "  << vcv << std::endl;
   Rcpp::Rcout << "r = "  << r << std::endl;
   Rcpp::Rcout << "theta = "  << theta << std::endl;
   Rcpp::Rcout << "retmat = "  << retmat << std::endl;
   Rcpp::Rcout << "numsim = "  << numsim << std::endl;
   Rcpp::Rcout << "prdelt = "  << prdelt << std::endl;
   Rcpp::Rcout << "ngroup = "  << ngroup << std::endl;
   Rcpp::Rcout << "centim = "  << centim << std::endl;
   Rcpp::Rcout << "nsamsz = "  << nsamsz << std::endl;
   Rcpp::Rcout << "krfail = "  << krfail << std::endl;
   Rcpp::Rcout << "numret = "  << numret << std::endl;
   Rcpp::Rcout << "iersim = "  << iersim << std::endl;
         
}

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
                                     Named("numsim") = numsim,
                                     Named("numret") = numret,
                                     Named("iersim") = iersim,
                                     Named("ierfit") = ierfit,
                                     Named("iervcv") = iervcv,
                                     Named("method") = method);

Rcpp::List doubs = Rcpp::List::create(Named("escale") = escale,
                                      Named("xlike") = xlike);

Rcpp::List bools = Rcpp::List::create(Named("lcheck") = lcheck);

Rcpp::List intvec = Rcpp::List::create(Named("cen") = cen,
                                       Named("wt") = wt,
                                       Named("tcodes") = tcodes,
                                       Named("iscrat") = iscrat,
                                       Named("wtnew") = wtnew,
                                       Named("iir") = iir,
                                       Named("ijc") = ijc,
                                       Named("nsamsz") = nsamsz,
                                       Named("nmrvec") = nmrvec,
                                       Named("krfail") = krfail);

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
                                       Named("ipdiag") = ipdiag);

Rcpp::List numvec2 = Rcpp::List::create(Named("centim") = centim,
                                        Named("rhostr") = rhostr,
                                        Named("rhohat") = rhohat,
                                        Named("clevc") = clevc,
                                        Named("cpl") = cpl,
                                        Named("cpu") = cpu,
                                        Named("cpl2") = cpl2,
                                        Named("cpu2") = cpu2);

Rcpp::List nummat = Rcpp::List::create(Named("x") = x,
                                       Named("y") = y,
                                       Named("ty") = ty,
                                       Named("devian") = devian,
                                       Named("vcv") = vcv,
                                       Named("r") = r,
                                       Named("res") = res,
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
