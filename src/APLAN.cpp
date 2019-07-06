#include <base/base.hpp>
#include <vvar1/zfcdf.hpp>
#include <utility/wqm_quant.hpp>
#include <aplan/optf.hpp>
#include <aplan/poweld.hpp>
#include <aplan/vara.hpp>

using namespace pass1;
using namespace pass2;
using namespace pass3;

//' Generate accelerated life test plans with specified characteristics.
//'
//' @name aplan
//'
//' @param ap Standardized intercept
//' @param bp Standardized slope = (muh-mud)/sigmad;
//' @param thet1p theta1 = sigmah/sigmad;
//' @param pvalp Percentile of interest
//' @param knownp \code{integer} specifying type of model (see details)
//' @param idistp equals 1 for SEV/Weibull, 2 for Normal/Lognormal
//' @param ioptsp option for lower stress (see details)
//' @param ioptap option for allocation (see details)
//' @param ioptmp option for the middle stress level (see details)
//' @param pifixp middle allocation when ioptap=3
//' @param zholdp fixed value of z when ioptsp=3
//' @param pmlimp lower bound on pm when ioptmp=2
//' 
//' @return
//'   \code{zp}     Length-3 vector of standardized stress levels
//'   \code{pip}    Length-3 vector of proportionate allocations;
//'   \code{fpp}    Length-3 vector of failure probabilities;
//'   \code{pqp}    Length-3  vector equal to pip * fp
//'   \code{var}    Variance of yhat(pvalp)
//'   \code{iprinp} Level of printing (0 for none, 4 for debug)
//' 
//' @details 
//' \code{knownp} = 1 for unknown theta1, 
//'                 2 for known theta1,
//'                 3 for constant sigma quadratic model.
//' \code{ioptsp} = 1 set low stress such that pmlim is failure prob at zl
//'                 2 optimize low stress level,
//'                 3 set low stress such that zl=zholdp
//'                 4 optimize low and middle stress levels
//' \code{ioptap} = 1 equal allocation
//'                 2 equal expected number failing
//'                 3 optimize with fixed pim
//'                 4 optimize with e(rl)=r(rm);
//'                 5 optimize with e(rm)=r(rh);
//'                 6 4 2 1 relative allocation;
//'                 7 1 2 2 relative expected failures;
//'                 8 1 2 3 relative expected failures;
//'                 9 optimize low and middle;
//'                 10 optimize with pim=pih (as suggested by Wayne N.)
//' \code{ioptmp} = 0 xm=(xl+xh)/2
//'                 1 fm=(fl+fh)/2
//'                 2 xm=(xl+xh)/2 if fm>pmlim and fm=pmlim otherwise
//'                 3 float                 
// [[Rcpp::export]]
Rcpp::List APLAN(double ap,
                 double b1p,
                 double b2p,
                 double thet1p,
                 double pvalp,
                 int knownp,
                 int idistp,
                 int ioptsp,
                 int ioptap,
                 int ioptmp,
                 double pifixp,
                 double zholdp,
                 double pmlimp,
                 Rcpp::NumericVector zp,
                 Rcpp::NumericVector pip,
                 Rcpp::NumericVector fpp,
                 Rcpp::NumericVector pqp,
                 double var,
                 int iprinp,
                 int ier){
  
debug::kprint = iprinp;
  
Rcpp::List l_pass1, l_pass2;
Rcpp::List ints,doubs,numvec;
Rcpp::NumericVector e(4), delta(4);
double escale,ph,xistar,xilst,ximst;
double xpnum,pi2c,pi1st,pi2st;
int nopt,iprp,k;

double eprod = 1.0e-01, ecomm = 1.0e-05;
int icon = 1, maxit = 15;

// Move subroutine input to common for function evaluation
   pass2::g_a = ap;
   pass2::g_b1 = b1p;
   pass2::g_b2 = b2p;
   pass2::g_thet1 = thet1p;
   pass2::g_pval = pvalp;
   pass2::g_pifix = pifixp;
   pass2::g_pmlim = pmlimp;
   pass2::g_zlhold = zholdp;
   
   pass3::g_knownt = knownp;
   pass3::g_idist = idistp;
   pass3::g_iopts = ioptsp;
   pass3::g_iopta = ioptap;
   pass3::g_ioptm = ioptmp;

// Set up the e vector for powell such that e*escale=eprod=.1(def)
   for(int i = 1; i <= 3; i++){
   
       escale      = (one / ecomm) * eprod;
       e.at(i - 1) = ecomm;
   
   }

// Set up for optimization of stress level using iopts
//====================================================

// iopts = 1: set low stress such that pmlim is failure prob at zl
   if(pass3::g_iopts == 1){
     
      nopt = 0;
     
   }

// iopts = 2: optimize low stress level
   if(pass3::g_iopts == 2){
     
      nopt = 1;
      pass2::g_dlimu.at(nopt - 1) = one;
      pass2::g_dliml.at(nopt - 1) = zero;
      // dliml(nopt)=-two;
      ph = zfcdf(((pass2::g_a - pass2::g_b1 - pass2::g_b2) / pass2::g_thet1),pass3::g_idist);
      xistar = (pass2::g_a - wqm_quant(ph / 3.0e00, pass3::g_idist)) / pass2::g_b1;
      if(xistar > 0.8e00) xistar = 0.8e00;
      if(xistar < 0.2e00) xistar = 0.2e00;
      
      delta.at(nopt - 1) = optf(xistar,pass2::g_dliml.at(nopt - 1),pass2::g_dlimu.at(nopt - 1));
      
      if(debug::kprint >= 1){
        
         Rcpp::Rcout << "\nAPLAN XISTAR\n"                     << std::endl;
         Rcpp::Rcout << "nopt = "        << nopt - 1           << std::endl;
         Rcpp::Rcout << "a = "           << pass2::g_a         << std::endl;
         Rcpp::Rcout << "b1 = "          << pass2::g_b1        << std::endl;
         Rcpp::Rcout << "ph = "          << ph                 << std::endl;
         Rcpp::Rcout << "xistar = "      << xistar             << std::endl;
         Rcpp::Rcout << "delta(nopt) = " << delta.at(nopt - 1) << std::endl;
        
      } 
   
   }

// iopts = 3: set low stress such that zl = zholdp
   if(pass3::g_iopts == 3){
     
      nopt = 0;
     
   }

// iopts = 4: optimize xl and xm
   if(pass3::g_iopts == 4){
     
      nopt = 1;
      pass2::g_dlimu.at(nopt - 1) = one;
      pass2::g_dliml.at(nopt - 1) = zero;
      // dliml(nopt)=-two;
      xilst = 0.395;
      delta.at(nopt - 1) = optf(xilst,pass2::g_dliml.at(nopt - 1),pass2::g_dlimu.at(nopt - 1));
      nopt = nopt + 1;
      pass2::g_dlimu.at(nopt - 1) = one;
      pass2::g_dliml.at(nopt - 1) = zero;
      // dliml(nopt)=-two;
      ximst = 0.902;
      delta.at(nopt - 1) = optf(ximst,pass2::g_dliml.at(nopt - 1),pass2::g_dlimu.at(nopt - 1));
     
   }

// ioptm (doesn't seem to be used now)
//====================================================

// ioptm = 0: xm = (xl + xh) / 2
// ioptm = 1: fm = (fl + fh) / 2
// ioptm = 2: xm = (xl + xh) / 2 if fm > pmlim; fm = pmlim otherwise
// ioptm = 3: float

// Set up allocation optimization according to iopta
//==================================================

// iopta = 1: equal allocation
   if(pass3::g_iopta == 1) { };
  
// iopta = 2: equal expected number failing
   if(pass3::g_iopta == 2) { };
   
// iopta = 3: optimize with fixed pim   
   if(pass3::g_iopta == 3) {
     
      nopt = nopt + 1;
      pass2::g_dlimu.at(nopt - 1) = one;
      pass2::g_dliml.at(nopt - 1) = zero;
      delta.at(nopt - 1) = optf(zp2,pass2::g_dliml.at(nopt - 1),pass2::g_dlimu.at(nopt - 1));
     
   }
   
// iopta = 4: optimize with e(rl) = r(rm)
   if(pass3::g_iopta == 4) {
     
      nopt = nopt + 1;
      pass2::g_dlimu.at(nopt - 1) = one;
      pass2::g_dliml.at(nopt - 1) = zero;
      delta.at(nopt - 1) = optf(zp2,pass2::g_dliml.at(nopt - 1),pass2::g_dlimu.at(nopt - 1));
     
   }

// iopta = 5: optimize with e(rm) = r(rh)
   if(pass3::g_iopta == 5) {
     
      nopt = nopt + 1;
      pass2::g_dlimu.at(nopt - 1) = one;
      pass2::g_dliml.at(nopt - 1) = zero;
      delta.at(nopt - 1) = optf(zp2,pass2::g_dliml.at(nopt - 1),pass2::g_dlimu.at(nopt - 1));
     
   }
   
// iopta = 6: 4 2 1 relative allocation
   if(pass3::g_iopta == 6) { };
   
// iopta = 7: 1 2 2 relative expected failures   
   if(pass3::g_iopta == 7) { };
   
// iopta = 8: 1 2 3 relative expected failures
   if(pass3::g_iopta == 8) { };
   
// iopta = 9: optimize low and middle
   if(pass3::g_iopta == 9) {
     
      nopt = nopt + 1;
      pass2::g_dlimu.at(nopt - 1) = one;
      pass2::g_dliml.at(nopt - 1) = zero;
      pi1st = 0.771e00;
      delta.at(nopt - 1) = optf(pi1st,pass2::g_dliml.at(nopt - 1),pass2::g_dlimu.at(nopt - 1));
      nopt = nopt + 1;
      pass2::g_dlimu.at(nopt - 1) = one;
      pass2::g_dliml.at(nopt - 1) = zero;
      pi2st = 0.143;
      pi2c = pi2st / (one - pi1st);
      delta.at(nopt - 1) = optf(pi2c,pass2::g_dliml.at(nopt - 1),pass2::g_dlimu.at(nopt - 1));  
      
   }
   
// iopta = 10: optimize with pim = pih (as suggested by wayne n.)
   if(pass3::g_iopta == 10) { };

if(nopt != 0) {
  
   iprp = 0;
   if(debug::kprint >= 5) iprp = 5;
   poweld(delta,e,nopt,var,escale,iprp,icon,maxit,vara,ier);

} 

var = vara(delta,nopt);

zp  = clone(pass1::g_z);
pip = clone(pass1::g_pi);
fpp = clone(pass1::g_fp);
pqp = clone(pass1::g_pq);

xpnum = 100 * (pass3::g_iopts - 1) + 10 * (pass3::g_ioptm - 1) + pass3::g_iopta;
k = 3;

if(debug::kprint >= 3) {
  
   Rcpp::Rcout << "\nEND OF APLAN\n"               << std::endl;
   Rcpp::Rcout << "a = "        << pass2::g_a      << std::endl;
   Rcpp::Rcout << "b1 = "       << pass2::g_b1     << std::endl;
   Rcpp::Rcout << "b2 = "       << pass2::g_b2     << std::endl;
   Rcpp::Rcout << "thet1 = "    << pass2::g_thet1  << std::endl;
   Rcpp::Rcout << "knownt = "   << pass3::g_knownt << std::endl;
   Rcpp::Rcout << "pi = "       << pass1::g_pi     << std::endl;
   Rcpp::Rcout << "z = "        << pass1::g_z      << std::endl;
   Rcpp::Rcout << "fp = "       << pass1::g_fp     << std::endl;
   Rcpp::Rcout << "pq = "       << pass1::g_pq     << std::endl;
   Rcpp::Rcout << "k = "        << k               << std::endl;
   Rcpp::Rcout << "idist = "    << pass3::g_idist  << std::endl;
   Rcpp::Rcout << "pval = "     << pass2::g_pval   << std::endl;
   Rcpp::Rcout << "xpnum = "    << xpnum           << std::endl;
   Rcpp::Rcout << "var = "      << var             << std::endl;
  
}

ints   = Rcpp::List::create(Named("iopta")  = pass3::g_iopta,
                            Named("knownp") = knownp,
                            Named("idistp") = idistp,
                            Named("ioptsp") = ioptsp,
                            Named("ioptap") = ioptap,
                            Named("ioptmp") = ioptmp,
                            Named("ier") = ier);

doubs  = Rcpp::List::create(Named("ap") = ap,
                            Named("b1p") = b1p,
                            Named("b2p") = b2p,
                            Named("thet1p") = thet1p,
                            Named("pvalp") = pvalp,
                            Named("pifixp") = pifixp,
                            Named("zholdp") = zholdp,
                            Named("pmlimp") = pmlimp,
                            Named("var") = var);

numvec = Rcpp::List::create(Named("zp") = zp,
                            Named("pip") = pip,
                            Named("fpp") = fpp,
                            Named("pqp") = pqp);

l_pass1 = Rcpp::List::create(Named("pi") = pass1::g_pi,
                             Named("z") = pass1::g_z,
                             Named("fp") = pass1::g_fp,
                             Named("pq") = pass1::g_pq);

l_pass2 = Rcpp::List::create(Named("a") = pass2::g_a,
                             Named("b1") = pass2::g_b1,
                             Named("b2") = pass2::g_b2,
                             Named("thet1") = pass2::g_thet1,
                             Named("pval") = pass2::g_pval,
                             Named("pifix") = pass2::g_pifix,
                             Named("pmlim") = pass2::g_pmlim,
                             Named("zlhold") = pass2::g_zlhold);

return Rcpp::List::create(Named("ints")   = ints,
                          Named("doubs")  = doubs, 
                          Named("numvec") = numvec,
                          Named("pass1")  = l_pass1, 
                          Named("pass2")  = l_pass2);

}