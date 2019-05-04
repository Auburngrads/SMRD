#include <base/base.hpp>
#include <gensiz/explan.hpp>
#include <gensiz/fdnprd.hpp>
#include <genmax/corrg.hpp>
#include <genmax/astww.hpp>
#include <genmax/fitww.hpp>

//' New version that sends down arrays instead of pointers!!
//'
//'  #  kmod           (I)  model code (usual list 1-5)
//'  #  -------------------------------------------------------------
//'  #    0  distribution kdist with one or more parameters related to
//'  #       explanatory variables
//'  #    1  same as 0 with an lfp parameter
//'  #    2  same as 0 with a doa parameter
//'  #    3  steady-state model with distribution kdist plus exponential
//'  #       competing risk
//'  #    4  proportional hazards model for distribution kdist plus
//'  #       explanatory variables for the power (or other) parameters
//'  #
//'  #  kdist         (I)   distribution number (usual list 1-12)
//'  #                   >100 is a user specified distribution
//'  #
//'  #  theta(nparm)   (O)   double precision vector returning ml estimates
//'  #                      of model parameters and values of any
//'  #                      fixed parameters
//'  #
//'  #  thetas(nparm) (O)   double precision vector returning ml
//'  #                      scaled estimates
//'  #                   of model parameters and (scaled) values of
//'  #                   any fixed parameters
//'  #                   data scaled according to centered and scaled x
//'  #
//'  #  kodet(nparm)  (O)   integer vector of parameter codes indicating range
//'  #                        0 fixed
//'  #                        1 unrestricted
//'  #                        2 positive
//'  #                        3 0-1
//'  #
//'  #
//'  #  ifix(nparm)   (I)   an integer vector of 0's and 1's to indicate which
//'  #                   parameters are to be fixed (1 for fixed)
//'  #
//'  #  nparm       (O)     number of model parameters
//'  #                   nparm is the sum of the parameters in all of the
//'  #                   regression relationships plus any model parameters
//'  #                   that do not depend on explanatory variables
//'  #                   If this number is needed above before calling
//'  #                   genmax (to allocate parameter space) it can be
//'  #                   gotten from a call to gensiz
//'  #                   the model stuff (kmod,kdist,kparv,nrvar,mrelat,nrelat)
//'  #                   has been set.
//'  #
//'  #  npard       (O)     number of model parameters
//'  #                   npard is the  number of parameters in the distributional
//'  #                   model (i.e., for a particular combination of the
//'  #                   explanatory variables
//'  #                   If this number is needed above before calling
//'  #                   genmax (to allocate parameter space) it can be
//'  #                   gotten from a call to gensiz
//'  #                   the model stuff (kmod,kdist,kparv,nrvar,mrelat,nrelat)
//'  #                   has been set.
//'  #
//'  #  y            (I)  real matrix of response times .
//'  #                      transformed times may be returned
//'  #
//'  #  ncoly        (I)    number of columns in the observation matrix (1 or 2)
//'  #
//'  #  nrownw        (I)     number of rows in the data matrix
//'  #
//'  #  x         (I)     real matrix x of explanatory variables
//'  #                      which  must have a
//'  #                      column of ones in the first column (even if it
//'  #                      is not used).
//'  #
//'  #               *****  note that upon return, x may not be in its original
//'  #                      form due to scaling or other standardization.
//'  #
//'  #  ncolx         (I)   number of columns in x, not including the required col
//'  #                      of ones (because we think of the ones as column zero)
//'  #
//'  #  codes         (I)  real vector of censor codes
//'  #
//'  #  weight        (I)   real vector of observation weights
//'  #
//'  #  ty          (I)   real matrix of truncation times
//'  #
//'  #  ncolty       (I)    number of columns in the matrix of truncation times
//'  #                      (0, 1, or 2)
//'  #
//'  #  tc           (I)  real vector of truncation codes
//'  #
//'  #  kprint       (I)    print code 0-none  1-minimal 2-usual 3-light debug
//'  #                   4 and higher provides more debug output
//'  #
//'  #
//'  #  kparv        (I) integer vector giving parameter
//'  #                   numbers having regression
//'  #                   relationships
//'  #
//'  #  nrvar      (I)   integer vector. nrvar(i) gives the number of columns
//'  #                   of x that are in the relationship for the
//'  #                   parameter specified in kparv(i)
//'  #
//'  #  mrelat(mnrvar,nrelat)  (I) integer matrix
//'  #                   in which entry i,j gives the col of x
//'  #                   for the ith term of the jth relationship.  i=1,nrvar(j),
//'  #                   j=1,nrelat
//'  #
//'  #  nrelat     (I) number of regression relationships
//'  #
//'  #  mnrvar     (I)   integer vector. nrvar(i) gives the number of columns
//'  #
//'  #  xlogl        (O) double precision scaler returning value of the
//'  #                   log likelihood
//'  #
//'  #  yhat       (O) real vector (matrix) of fitted values (same size as y)
//'  #
//'  #  resid       (O) real vector  (matrix) of residuals (same size as y)
//'  #
//'  #  vcv(nparm,nparm) (O) double precision matrix returning estimated matrix of
//'  #                   variances and covariances of the ml estimates
//'  #
//'  #  vcvs(nparm,nparm)(O)  double precision matrix returning estimated
//'  #                        matrix of variances and covariances of the
//'  #                        ml scaled estimates
//'  #
//'  #  r(nparm,nparm)   (O)  double precision matrix returning estimated
//'  #                        correlation  matrix from the vcv matrix
//'  #
//'  # start           (I--opt)  optional vector of starting parameter values
//'  #                       (ignored if lstar=0)
//'  #
//'  # lstar            (I)    =1 is start values are in start
//'  #                         =0 if automatic start values are
//'  #                             to be computed velow
//'  #
//'  # ilabp           (O)  integer vector of parameter labels (length 8*nparm)
//'  #                      in this new version we copy over to stack and pass
//'  #                      pointers below
//'  #
//'  # ilabd           (O)  integer vector of distribution parmameters
//'  #                      (length 8*nparm)
//'  #                      in this new version we copy over to stack and pass
//'  #                      pointers below
//'  #
//'  #  ier            code       meaning
//'  #     -------------------------------------------------------------------
//'  #     third digit:  0 no optimization errors detected
//'  #                   1 likelihood shape caused problems with the powell alg
//'  #                   2 convergence criterion not met after maximum number
//'  #                     or iterations
//'  #
//'  #   second digit:   0 first derivatives of likelihood small
//'  #                   1 first derivatives of the loglikelihood too large
//'  #
//'  #    first digit:   0 estimated fisher info matrix inverted successfully
//'  #                   1 estimated fisher info matrix appears to be singular
//'  #
//'  #note: all pointers sent down in the argument list are pointing to the
//'  #     rs(.) stack in labeled common.
//'  #
//'  #      fortran typing conventions are generally followed for typing
//'  #      except that double precision is used except when data is to
//'  #      be taken from the worksheet.
// [[Rcpp::export]]
Rcpp::List GENMAX(int &kmod,
                  int &kdist,
                  Rcpp::NumericVector &theta,
                  Rcpp::NumericVector &thetas,
                  Rcpp::IntegerVector &kodet,
                  Rcpp::IntegerVector &ifix,
                  int &nparm,
                  int &npard,
                  Rcpp::NumericMatrix &y,
                  int &ncoly,
                  int &nrownw,
                  Rcpp::NumericMatrix &x,
                  int &ncolx,
                  Rcpp::IntegerVector &codes,
                  Rcpp::IntegerVector &weight,
                  Rcpp::NumericMatrix &ty,
                  int &ncolty,
                  Rcpp::IntegerVector &tcodes,
                  int &kprint,
                  Rcpp::IntegerVector &kparv,
                  Rcpp::IntegerVector &nrvar,
                  Rcpp::IntegerMatrix &mrelat,
                  int &nrelat,
                  int &mnrvar,
                  double &xlogl,
                  Rcpp::NumericMatrix &yhat,
                  Rcpp::NumericMatrix &resid,
                  Rcpp::NumericMatrix &vcvs,
                  Rcpp::NumericMatrix &vcv,
                  Rcpp::NumericMatrix &r,
                  Rcpp::NumericVector &start,
                  int &lstar,
                  double &conlev,
                  Rcpp::IntegerVector &ilabp,
                  Rcpp::IntegerVector &ilabd,
                  int &ier,
                  Rcpp::IntegerVector &nxd,
                  Rcpp::IntegerVector &intd,
                  Rcpp::List &ipxcd,
                  Rcpp::IntegerVector &irelad,
                  Rcpp::NumericVector &fstder,
                  int &nregr, 
                  int &kcentr, 
                  int &kpoint,
                  int &ifit,
                  int &kgtall,
                  int &llog, 
                  int &kmodp, 
                  int &maxit,
                  double &pest, 
                  double &epsx,
                  int &npardm,
                  int &nnum, 
                  int &kparm, 
                  int &iup, 
                  int &nterd,
                  int &maxpd,
                  double &pfail,
                  int &kmccde,
                  int &nstart,  // ast0x1
                  int &maxmsd,  // ast0x1
                  double &tol,
                  int &lsd,
                  double &pchmax){ // ast0x1

   debug::kprint = kprint;
   Rcpp::NumericMatrix iptynw;
   Rcpp::IntegerVector iptc;

// Grab space for distribution parameter lables
   Rcpp::IntegerVector iplabp = Rcpp::IntegerVector(20 * (nparm + 1));

// explan set up explanatory variables
   explan(kparv,nrvar,mrelat,nrelat,
          mnrvar,nxd,intd,ipxcd,irelad,
          ncolx,ier,npardm,nnum,kparm,iup,nterd);

// Set up parameter vector
   fdnprd(kmod,kdist,intd,nxd,nregr,
          npard,kgtall,nparm,ilabp,
          ilabd,llog,kmodp,ier,maxpd);
  
   int maxpp = nparm + 1;
// call copyi(ilabp,is(iplabp),nparm*8);

// Initialize global variables
   Rcpp::NumericMatrix ipynew = clone(y);
   Rcpp::NumericMatrix ipxnew = clone(x);
   Rcpp::IntegerVector ipcode = clone(codes);
   Rcpp::IntegerVector ipweig = clone(weight);

if(ncolty > 0) {
  
   Rcpp::NumericMatrix iptynw = clone(ty);
   Rcpp::IntegerVector iptc   = clone(tcodes);
  
}

// Get global objects needed to return stuff
   Rcpp::NumericMatrix iresid = Rcpp::NumericMatrix(nrownw, ncoly);
   Rcpp::NumericMatrix iyhat  = Rcpp::NumericMatrix(nrownw, ncoly);

if(debug::kprint > 3) {
 
   Rcpp::Rcout << "\nGENMAX BEFORE ASTWW\n" << std::endl;
   
   for(int ik = 0; ik < 5; ik++){
     
       if(ipxcd[ik] != R_NilValue) {
      
          SEXP l = ipxcd[ik]; Rcpp::IntegerVector y(l);
          Rcpp::Rcout << "i = " << ik << std::endl;
          Rcpp::Rcout << "ipxcd(i) = " << y << std::endl;
     
       }
   }

   Rcpp::Rcout << "intd = "     << intd      << std::endl;
   Rcpp::Rcout << "nxd = "      << nxd       << std::endl;
   Rcpp::Rcout << "irelad = "  << irelad    << std::endl;
   Rcpp::Rcout << "ynew = "    << ipynew    << std::endl;
   Rcpp::Rcout << "xnew = "    << ipxnew    << std::endl;
   Rcpp::Rcout << "codes = "   << ipcode    << std::endl;
   Rcpp::Rcout << "weights = " << ipweig    << std::endl;
   Rcpp::Rcout << "tc = "      << iptc      << std::endl;
   Rcpp::Rcout << "tynew = "   << iptynw    << std::endl;
   Rcpp::Rcout << "maxpp = "   << maxpp     << std::endl;
        
}

// astart automatic starting values
   astww(kmod,kdist,intd,nxd,ipxcd,
         irelad,npard,theta,thetas,
         kodet,ifix,nparm,ipynew,ncoly,
         nrownw,ipxnew,ncolx,ipcode,ipweig,
         iptynw,ncolty,iptc,kcentr,iplabp,pest,maxit,
         vcvs,vcv,r,maxpp,ilabp,ilabd,ier,kgtall,
         nregr,llog,kmodp,maxpd,pfail,kmccde,nstart,
         maxmsd,tol,lsd,pchmax);

// After using these values in astww we need
// to restore them so that we can use them again
   ipynew = clone(y);
   ipxnew = clone(x);
   iptynw = clone(ty);

// Fit via maximum likelihood
   fitww(kmod,kdist,intd,nxd,ipxcd,irelad,
         npard,theta,thetas,kodet,ifix,nparm,
         ipynew,ncoly,nrownw,ipxnew,ncolx,
         ipcode,ipweig,iptynw,ncolty,iptc,
         kcentr,iplabp,ifit,pest,maxit,
         epsx,fstder,xlogl,iyhat,iresid,vcvs,vcv,
         r,start,lstar,maxpp,conlev,ilabp,ilabd,ier);

// Return the fitted values and residuals
   resid = clone(iresid);
   yhat  = clone(iyhat);

corrg(vcv,r,nparm);

List ints1 = Rcpp::List::create(Named("kmod") = kmod,
                                Named("kdist") = kdist,
                                Named("nrelat") = nrelat,
                                Named("mnrvar") = mnrvar,
                                Named("ncolx") = ncolx,
                                Named("ncoly") = ncoly,
                                Named("ncolty") = ncolty,
                                Named("nrownw") = nrownw,
                                Named("nparm") = nparm,
                                Named("npard") = npard,
                                Named("ier") = ier,
                                Named("nstart") = nstart,
                                Named("maxmsd") = maxmsd,
                                Named("lsd") = lsd,
                                Named("debug__kprint") = debug::kprint);

List ints2 = Rcpp::List::create(Named("nregr") = nregr,
                                Named("kgtall") = kgtall,
                                Named("llog") = llog,
                                Named("kmodp") = kmodp,
                                Named("npardm") = npardm,
                                Named("nnum") = nnum,
                                Named("kparm") = kparm,
                                Named("iup") = iup,
                                Named("nterd") = nterd,
                                Named("maxpd") = maxpd,
                                Named("lstar") = lstar,
                                Named("kcentr") = kcentr, 
                                Named("kpoint") = kpoint,
                                Named("ifit") = ifit,
                                Named("maxit") = maxit,
                                Named("kmccde") = kmccde);

List doubs = Rcpp::List::create(Named("pest") = pest, 
                                Named("epsx") = epsx,
                                Named("xlogl") = xlogl,
                                Named("conlev") = conlev,
                                Named("pfail") = pfail,
                                Named("tol") = tol,
                                Named("pchmax") = pchmax);
  
List intvec = Rcpp::List::create(Named("kodet") = kodet,
                                 Named("ifix") = ifix,
                                 Named("codes") = codes,
                                 Named("weight") = weight,
                                 Named("tcodes") = tcodes,
                                 Named("mnrvar") = mnrvar,
                                 Named("genmax_g__iplabp") = iplabp,
                                 Named("genmax_g__ipcode") = ipcode,
                                 Named("genmax_g__ipweig") = ipweig,
                                 Named("genmax_g__iptc")   = iptc,
                                 Named("nxd")              =  nxd,
                                 Named("intd")             =  intd,
                                 Named("irelad")           =  irelad);

List numvec = Rcpp::List::create(Named("theta") = theta,
                                 Named("thetas") = thetas,
                                 Named("start") = start,
                                 Named("fstder") = fstder);

List nummat = Rcpp::List::create(Named("y") = y,
                                 Named("x") = x,
                                 Named("ty") = ty,
                                 Named("yhat") = yhat,
                                 Named("resid") = resid,
                                 Named("vcvs") = vcvs,
                                 Named("vcv") = vcv,
                                 Named("r") = r,
                                 Named("genmax_g__ipynew") = ipynew,
                                 Named("genmax_g__ipxnew") = ipxnew,
                                 Named("genmax_g__iptynw") = iptynw,
                                 Named("genmax_g__iresid") = iresid,
                                 Named("genmax_g__iyhat")  = iyhat);

List EXPLAN_G = Rcpp::List::create(Named("mu_rel") = explan_g::mu_cols,
                                   Named("si_rel") = explan_g::si_cols,
                                   Named("p1_rel") = explan_g::p1_cols,
                                   Named("p2_rel") = explan_g::p2_cols,
                                   Named("p3_rel") = explan_g::p3_cols);

List ASTWW_G = Rcpp::List::create(Named("iipcod")  =  astww_g::iipcod,
                                  Named("iipwei")  =  astww_g::iipwei);

List AST0XX_G = Rcpp::List::create(Named("iresid") = ast0xx_g::iresid,
                                   Named("iyhat") = ast0xx_g::iyhat,
                                   Named("itimes") = ast0xx_g::itimes,
                                   Named("iq") = ast0xx_g::iq,
                                   Named("ip") = ast0xx_g::ip,
                                   Named("iprob") = ast0xx_g::iprob,
                                   Named("ipsd") = ast0xx_g::ipsd,
                                   Named("ithtmp") = ast0xx_g::ithtmp,
                                   Named("iypoin") = ast0xx_g::iypoin,
                                   Named("ippoin") = ast0xx_g::ippoin,
                                   Named("ippsd") = ast0xx_g::ippsd);

List GENX20 = Rcpp::List::create(Named("nxg")   = genx20::g_nxg,
                                 Named("nterg") = genx20::g_nterg,
                                 Named("intg")  = genx20::g_intg,
                                 Named("ipxcg") = genx20::g_ipxcg);

List GENX03 = Rcpp::List::create(Named("ipkode") = genx03::g_ipkode,
                                 Named("ipplab") = genx03::g_ipplab,
                                 Named("ngame") = genx03::g_ngame);

List GENX05 = Rcpp::List::create(Named("ncolx") = genx05::g_ncolx,
                                 Named("ipxbar") = genx05::g_ipxbar,
                                 Named("ipxbru") = genx05::g_ipxbru,
                                 Named("ipsd") = genx05::g_ipsd,
                                 Named("ipiscd") = genx05::g_ipiscd,
                                 Named("ipx") = genx05::g_ipx);

List GENX07 = Rcpp::List::create(Named("kdist") = genx07::g_kdist,
                                 Named("kmod") = genx07::g_kmod,
                                 Named("kmccde") = genx07::g_kmccde,
                                 Named("kpopu") = genx07::g_kpopu,
                                 Named("llog") = genx07::g_llog,
                                 Named("nparm") = genx07::g_nparm);

List GENX21 = Rcpp::List::create(Named("gamms") = genx21::g_gamms,
                                 Named("ipthet") = genx21::g_ipthet,
                                 Named("irelag") = genx21::g_irelag,
                                 Named("igtyg") = genx21::g_igtyg);

List globals = Rcpp::List::create(Named("explan") = EXPLAN_G,
                                  Named("astww")  = ASTWW_G,
                                  Named("ast0xx") = AST0XX_G,
                                  Named("genx20") = GENX20,
                                  Named("genx21") = GENX21,
                                  Named("genx03") = GENX03,
                                  Named("genx05") = GENX05,
                                  Named("genx07") = GENX07);

 return Rcpp::List::create(Named("ints1")   = ints1,
                           Named("ints2")   = ints2,
                           Named("doubs" )  = doubs,
                           Named("intvec")  = intvec,
                           Named("numvec")  = numvec,
                           Named("nummat" ) = nummat,
                           Named("globals") = globals,
                           Named("ipxcd")   = ipxcd);


}