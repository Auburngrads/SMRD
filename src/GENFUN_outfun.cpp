#include <base/base.hpp>
#include <genmax/setup.hpp>
#include <genfun/funint.hpp>
#include <genfun/functg.hpp>

using namespace genx14;
using namespace genx08;

//' compute estimates and confidence limits for a function of parameters
//' 
//' conlev        confidence level as a probability (e.g.0.95)
//' 
//' kodef         range kode for the function to be computed
//' 
//' fargv(nargv)  double precision vector of arguments for the
//'               function (e.g.times for failure probabilities
//'               or probs for quantiles)
//' 
//' nargv         length of fargv (should be 0 if no argument)
//' 
//' kfuncp        function number
//'               x1  failure probability
//'               x2  distribution quantile
//'               x3  hazard rate
//'             >100  user specified
//' 
//'         for more complicated models (e.g., model 1, 2, or 3)
//'         x is the subpopulation number (0 for population)
//' 
//' kpoint        row number for getting explanatory variable conditions
//' 
//' vcvs(nparm,nparm) double precision covariance matrix of thetas
//' 
//' epsxp         double precision epsilon for finite differences
//'               if zero is sent down, 1.0d-08 is used
//' 
//' fest(nargv)   estimates of function
//' 
//' stderr(nargv) estimated standard error of fest
//' 
//' xlow(nargv)   lower confidence bounds
//' 
//' xup(nargv)    upper confidence bound

void outfun(int &kmod,
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
            Rcpp::NumericMatrix &ipy,
            int &ncoly,
            int &nrownw,
            Rcpp::NumericMatrix &ipx,
            int &ncolx,
            Rcpp::IntegerVector &ipcode,
            Rcpp::IntegerVector &ipweig,
            Rcpp::NumericMatrix &ipty,
            int &ncolty,
            Rcpp::IntegerVector &iptc,
            int &kcentr,
            Rcpp::IntegerVector &ipplab,
            double &conlev,
            int &kodef,
            Rcpp::NumericVector &fargv,
            int &nargv,
            int &kfuncp,
            int &kpopu,
            int &kpoint,
            double &epsxp,
            Rcpp::NumericMatrix &vcvs,
            Rcpp::NumericVector &fest,
            Rcpp::NumericVector &std_err,
            Rcpp::NumericVector &xlow,
            Rcpp::NumericVector &xup,
            int &kmodp,
            double &pfail,
            int &kmccde,
            int &llog,
            int &nregr,
            int &ier){

int kfuncf,i;
double epsx,funarg;
    
// Call setup to center and standardize data
// set likilihood communication variables below
// transform theta values for standardized and centered variables
   ier = 0;
   kmccde = 0;
   pfail = 0.0e00;
   setup(kmod,kdist,intd,nxd,ipxcd,irelad,npard,
         theta,thetas,kodet,ifix,nparm,ipy,ncoly,
         nrownw,ipx,ncolx,ipcode,ipweig,ipty,ncolty,
         iptc,kcentr,ipplab,kmodp,pfail,kmccde,llog,
         nregr,ier);
   
   if(debug::kprint >= 4) {
      
      Rcpp::Rcout << "\nOUTFUN AFTER SETUP\n" << std::endl;
      Rcpp::Rcout << "ier = " << ier << std::endl;
      Rcpp::Rcout << "kpoint = " << kpoint << std::endl;
      Rcpp::Rcout << "kpopu = " << kpopu << std::endl;
      Rcpp::Rcout << "kfuncp = " << kfuncp << std::endl;
      Rcpp::Rcout << "nargv = " << 4 << std::endl;
      
   }
  
   if(ier > 0) return;
     
// get flat funtion indicator
// 
//   kpopu        population
// ---------------------------
//     0            basic population
//     1            sub population 1
//     2            sub population 2
   kfuncf = 3 * kpopu + kfuncp;
   epsx = 1.0e-08;
   if(epsxp <= zero) epsx = 1.0e-08;
   
// Set function evaluation constants in common in case there are no arguments
   funarg = 0.0;
   genx14::g_funarg = funarg;
   genx14::g_kfuncf = kfuncf;
   genx08::g_kpoint = kpoint;
   
   if(nargv != 0) {
     
      //if(debug::kprint >= 1)call printh(kmod,kfuncf,kpoint,conlev,kodef);
      
      // Loop over the function argument values
         for(i = 1; i <= nargv; i++){
           
                     Rcpp::Rcout << "\nHere\n" << std::endl;
             // Reset kprint to the original value for subsequent arguments
                //if((i > 1) and (kprh >= 1)) debug::kprint = 2;
                
                funarg = fargv.at(i - 1);
             // Set function evaluation constants in common
                genx14::g_funarg = funarg;
                genx14::g_kfuncf = kfuncf;
                genx08::g_kpoint = kpoint;

             // Compute the function estimate, se, and conf bounds
                Rcpp::Rcout << "\nHere\n" << std::endl;
                funint(functg,kodef,conlev,thetas,vcvs,kodet,
                       nparm,epsx,fest.at(i - 1),std_err.at(i - 1),
                       xlow.at(i - 1),xup.at(i - 1));
     
         }
         
   // print the function estimate, se, and conf bounds
      // if(debug::kprint >= 1) printt(fargv,nargv,fest,stderr,xlow,xup,2,conlev);
      return;
      
   }
   
// This part is for functions with no arguments
// like the ratio of two parameters or weibull parameters from sev
   funint(functg,kodef,conlev,thetas,vcvs,kodet,
          nparm,epsx,fest.at(i - 1),std_err.at(i - 1),
          xlow.at(i - 1),xup.at(i - 1));
   
// stprin(kprh);
// kprint = kprh;

return;

}