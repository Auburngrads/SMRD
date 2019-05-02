#include <base/base.hpp>
#include <genmax/setup.hpp>
#include <gensiz/explan.hpp>
#include <gensiz/fdnprd.hpp>
#include <genfun/outfun.hpp>

//'  #***see comments at top of genmax for description of the first
//'  #***arguments of this subroutine
//'     #it looks like ti is necessary to sned down the
//'     scaled information as well as the scaled and transformed
//'     x and y. 
//'  #
//'  #
//'  #
//'  #
//'  #conlev      (I)  confidence level as a probability (e.g.0.95)
//'  #
//'  #
//'  #
//'  #kodef      (I but should be O!)   range kode for the function to be computed
//'  #                1    -infinity to +infinity
//'  #                2     greater than zero
//'  #                3     between 0 and 1
//'  #
//'  #
//'  #
//'  #fargv(nargv)  (I) double precision vector of arguments for the
//'  #                  function (e.g.times for failure probabilities
//'  #                  or probs for quantiles)
//'  #
//'  #
//'  #nargv      (I)   length of fargv (should be 0 if no argument)
//'  #
//'  #
//'  #
//'  #kfuncp    (I)    function number
//'  #              x1  failure probability
//'  #              x2  distribution quantile
//'  #              x3  hazard rate
//'  #            >100  user specified
//'  #
//'  #        for more complicated models (e.g., model 1, 2, or 3)
//'  #        x is the subpopulation number (0 for entire population)
//'  #
//'  #
//'  #
//'  #  kpopu    (I)  for mixtures, specifies which population for quantiles
//'  #                 failure probs, etc.
//'  #
//'  #
//'  #kpoint    (I)    row number for getting explanatory variable conditions
//'  #                 send down kpoint=1 if no regression
//'  #
//'  #
//'  #vcvs(nparm,nparm) (I)  double precision covariance matrix of thetas
//'  #
//'  #
//'  #epsxp     (I)    double precision epsilon for finite differences
//'  #                 if zero is sent down, 1.0d-08 is used (now hardwired)
//'  #
//'  #
//'  #fest(nargv)   (O)  estimates of function
//'  #
//'  #stderr(nargv) (O)  estimated standard error of fest
//'  #
//'  #xlow(nargv)   (O)  lower confidence bounds
//'  #
//'  #xup(nargv)    (O)  upper confidence bound
// [[Rcpp::export]]
Rcpp::List GENFUN(int kmod,
                  int kdist,
                  Rcpp::IntegerVector ilabp,
                  Rcpp::IntegerVector ilabd,
                  Rcpp::NumericVector theta,
                  Rcpp::NumericVector thetas,
                  Rcpp::IntegerVector kodet,
                  Rcpp::IntegerVector ifix,
                  int nparm,
                  int npard,
                  Rcpp::NumericMatrix y,
                  int ncoly,
                  int nrownw,
                  Rcpp::NumericMatrix x,
                  int ncolx,
                  Rcpp::IntegerVector codes,
                  Rcpp::IntegerVector weight,
                  Rcpp::NumericMatrix ty,
                  int ncolty,
                  Rcpp::IntegerVector tcodes,
                  int kprint,
                  Rcpp::IntegerVector kparv,
                  Rcpp::IntegerVector nrvar,
                  Rcpp::IntegerMatrix mrelat,
                  int nrelat,
                  int mnrvar,
                  double conlev,
                  int kodef,
                  Rcpp::NumericVector fargv,
                  int nargv,
                  int kfuncp,
                  int kpopu,
                  int kpoint,
                  Rcpp::NumericMatrix vcvs,
                  Rcpp::NumericVector fest,
                  Rcpp::NumericVector std_err,
                  Rcpp::NumericVector xlow,
                  Rcpp::NumericVector xup,
                  Rcpp::IntegerVector nxd,
                  Rcpp::IntegerVector intd,
                  Rcpp::List ipxcd,
                  Rcpp::IntegerVector irelad,
                  int ier,
                  int llog,
                  int kmodp,
                  int maxpd,
                  int nregr,
                  int kmccde,
                  double pfail,
                  int npardm,
                  int nnum, 
                  int kparm, 
                  int iup, 
                  int nterd){

debug::kprint = kprint; 
   
double epsx = 1.0e-08;
int kcentr,kgtall;
Rcpp::List ints,doubs,intvec,numvec,nummat;
   
// The following print routines are similar to the splus ones, but
// are defined to run stand-alone. If running undersplus, we
// use versions of the routines that call the splus versions
   if(debug::kprint > 3){
      
      Rcpp::Rcout << "\nGENFUN**1**\n" << std::endl;
      Rcpp::Rcout << "kmod = " << kmod << std::endl;
      Rcpp::Rcout << "kdist = " << kdist << std::endl;
      Rcpp::Rcout << "theta = " << theta << std::endl;
      Rcpp::Rcout << "thetas = " << thetas << std::endl;
      Rcpp::Rcout << "kodet = " << kodet << std::endl;
      Rcpp::Rcout << "ifix = " << ifix << std::endl;
      Rcpp::Rcout << "nparm = " << nparm << std::endl;
      Rcpp::Rcout << "npard = " << npard << std::endl;
      Rcpp::Rcout << "y = " << y << std::endl;
      Rcpp::Rcout << "ncoly = " << ncoly << std::endl;
      Rcpp::Rcout << "nrownw = " << nrownw << std::endl;
      Rcpp::Rcout << "x = " << x << std::endl;
      Rcpp::Rcout << "ncolx = " << ncolx << std::endl;
      Rcpp::Rcout << "codes = " << codes << std::endl;
      Rcpp::Rcout << "weight = " << weight << std::endl;
      Rcpp::Rcout << "ty = " << ty << std::endl;
      Rcpp::Rcout << "ncolty = " << ncolty << std::endl;
      Rcpp::Rcout << "tcodes = " << tcodes << std::endl;
      Rcpp::Rcout << "kparv = " << kparv << std::endl;
      Rcpp::Rcout << "nrvar = " << nrvar << std::endl;
      Rcpp::Rcout << "mrelat = " << mrelat << std::endl;
      Rcpp::Rcout << "nrelat = " << nrelat << std::endl;
      Rcpp::Rcout << "mnrvar = " << mnrvar << std::endl;
      Rcpp::Rcout << "conlev = " << conlev << std::endl;
      Rcpp::Rcout << "kodef = " << kodef << std::endl;
      Rcpp::Rcout << "fargv = " << fargv << std::endl;
      Rcpp::Rcout << "nargv = " << nargv << std::endl;
      Rcpp::Rcout << "kfuncp = " << kfuncp << std::endl;
      Rcpp::Rcout << "kpoint = " << kpoint << std::endl;
      Rcpp::Rcout << "vcvs = " << vcvs << std::endl;
      Rcpp::Rcout << "epsx = " << epsx << std::endl;
          
   }
   
// Grab space for distribution parameter lables
   Rcpp::IntegerVector iplabp = Rcpp::IntegerVector(20 * (nparm + 1));
   Rcpp::NumericMatrix ipy = Rcpp::NumericMatrix(nrownw,ncoly);
   Rcpp::IntegerVector ipcode = Rcpp::IntegerVector(nrownw);
   Rcpp::IntegerVector ipweig = Rcpp::IntegerVector(nrownw);
   Rcpp::IntegerVector iptc = Rcpp::IntegerVector(nrownw);
   Rcpp::NumericMatrix ipty = Rcpp::NumericMatrix(nrownw,ncolty);
   Rcpp::NumericMatrix ipx  = Rcpp::NumericMatrix(nrownw,(ncolx + 1));
      
   ipy = clone(y);
   ipx = clone(x);
   ipweig = clone(weight);
   ipcode = clone(codes);
   if(ncolty > 0) ipty = clone(ty);
   if(ncolty > 0) iptc = clone(tcodes);

// Call explan to set up explanatory variables
   kcentr = 1;
   explan(kparv,nrvar,mrelat,nrelat,mnrvar,
          nxd,intd,ipxcd,irelad,ncolx,ier,npardm,
          nnum,kparm,iup,nterd);
   
   kgtall = 1;
   fdnprd(kmod,kdist,intd,nxd,nregr,npard,
          kgtall,nparm,ilabp,ilabd,llog,kmodp,
          ier,maxpd);
   
   iplabp = clone(ilabp);
   
// Call setup to center and standardize data
// set likilihood communication variables below
// transform theta values for standardized and centered variables
   setup(kmod,kdist,intd,nxd,ipxcd,irelad,npard,
         theta,thetas,kodet,ifix,nparm,ipy,ncoly,
         nrownw,ipx,ncolx,ipcode,ipweig,ipty,ncolty,
         iptc,kcentr,iplabp,kmodp,pfail,kmccde,llog,
         nregr,ier);
   
   if(debug::kprint >= 4) {
      
      Rcpp::Rcout << "\nGENFUN AFTER SETUP\n" << std::endl;
      Rcpp::Rcout << "ier = " << ier << std::endl;
      Rcpp::Rcout << "theta = " << theta << std::endl;
      Rcpp::Rcout << "thetas = " << thetas << std::endl;
      Rcpp::Rcout << "nargv = " << nargv << std::endl;
      
   }
   
   if(ier > 0) goto exit;
   
// cxxx      epsx=eps
// Compute function of parameters
   outfun(kmod,kdist,intd,nxd,ipxcd,irelad,npard,theta,
          thetas,kodet,ifix,nparm,ipy,ncoly,nrownw,ipx,
          ncolx,ipcode,ipweig,ipty,ncolty,iptc,kcentr,
          iplabp,conlev,kodef,fargv,nargv,kfuncp,kpopu,
          kpoint,epsx,vcvs,fest,std_err,xlow,xup,kmodp,
          pfail,kmccde,llog,nregr,ier);
   
   if(debug::kprint > 3) {
      
      Rcpp::Rcout << "\nGENFUN**2**\n" << std::endl;
      Rcpp::Rcout << "fest = " << fest << std::endl;
      Rcpp::Rcout << "stderr = " << std_err << std::endl;
      Rcpp::Rcout << "xlow = " << xlow << std::endl;
      Rcpp::Rcout << "xup = " << xup << std::endl;
      
   } 
   
// Return the value of the functions standard error
   exit: ints = Rcpp::List::create(Named("ier") = ier,
                                   Named("kodef") = kodef);
   
         doubs = Rcpp::List::create(Named("epsx") = epsx);
         
         intvec = Rcpp::List::create(Named("codes") = codes);
         
         numvec = Rcpp::List::create(Named("fest") = fest,
                                     Named("stderr") = std_err,
                                     Named("xlow") = xlow,
                                     Named("xup")  = xup,
                                     Named("theta.hat") = theta,
                                     Named("thetas.hat") = thetas);
         
         nummat = Rcpp::List::create(Named("vcvs") = vcvs,
                                     Named("y") = y,
                                     Named("x") = x,
                                     Named("ty") = ty,
                                     Named("ipy") = ipy,
                                     Named("ipx") = ipx,
                                     Named("ipty") = ipty,
                                     Named("mrelat") = mrelat);
   
      return Rcpp::List::create(Named("ints") = ints,
                                Named("doubs") = doubs,
                                Named("intvec") = intvec,
                                Named("numvec") = numvec,
                                Named("nummat") = nummat);
}