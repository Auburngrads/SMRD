#include <base/base.hpp>
#include <genmax/pxmat.hpp>
#include <gensiz/distxx.hpp>
#include <wqmmlesss/wqm_fpfail.hpp>
#include <genmax/proexv.hpp>
#include <genmax/stset.hpp>
#include <genmax/fdin.hpp>
#include <genmax/fdiscd.hpp>
#include <genmax/trdat.hpp>
#include <utility/wqm_copyi.hpp>
#include <genmax/scpx.hpp>

using namespace genx00;
using namespace genx01;
using namespace genx03;
using namespace genx05;
using namespace genx07;

//' @details Call \code{setup} to center and standardize data 
//'          set likilihood communication variables below 
//'          transform theta values for standardized and centered 
//'          variables return \code{pfail} and \code{kmccde} for further 
//'          application specif.
//' 
//' @note This routine must be preceeded by a call to ignout and a call
//'       to isnout must follow within any loop that contains this 
//'       routine since we will be allocating space from the stack below
//'       and the space needs to be released before releasing anything
//'       beyond point of call.

void setup(int &kmod,
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
           int &kmodp,
           double &pfail,
           int &kmccde,
           int &llog,
           int &nregr,
           int &ier){
  
// Added 29 aug to see the x matrix
   if(debug::kprint >= 4) pxmat(ipx);

// Get pointers for vectors needed in likelihood communication
   Rcpp::IntegerVector ipkode = Rcpp::IntegerVector(nparm);
   Rcpp::IntegerVector ipiscd = Rcpp::IntegerVector(ncolx + 1);
   Rcpp::IntegerVector ipinow = Rcpp::IntegerVector(nrownw);
   Rcpp::IntegerVector ipgty  = Rcpp::IntegerVector(npard);
   Rcpp::IntegerVector ipmark = Rcpp::IntegerVector(npard);
   Rcpp::NumericVector ipxbar = Rcpp::NumericVector(ncolx + 1);
   Rcpp::NumericVector ipxbru = Rcpp::NumericVector(ncolx + 1);
   Rcpp::NumericVector ipsd   = Rcpp::NumericVector(ncolx + 1);

// Find the proportion failing in the
// sample for reparameterization
   pfail = wqm_fpfail(ipcode,ipweig,nrownw);

// Set distribution and model
// Figure the number of model parameters and get igtyd
   int npardc = 0;
   distxx(kmod,kdist,llog,npard,ipgty,
          ipmark,npardc,kmodp,ier);

// Make sure that the number of distribution parameters
// found in kmod/kdist combination agrees with the value
// of npard that was provided coming into this routine
   if(npard != npardc) {

      Rcpp::Rcout << "ERROR: npard != npardc in setup"  << std::endl;
      Rcpp::Rcout << "npard = "  << npard               << std::endl;
      Rcpp::Rcout << "npardc = " << npardc              << std::endl;
      Rcpp::stop("Execution stopped at setup.cpp");

   }

// Set up communication with the likelihood routines
   int ngamd  = 0;
   int ngame  = 0;
   int nparmc = 0;
   proexv(nxd,intd,ipxcd,irelad,ipgty,ipmark,npard,
          nregr,ngamd,ngame,nparmc,ipkode,ifix);

   if(nparm != nparmc) {

      Rcpp::Rcout << "ERROR: nparm != nparmc in setup"  << std::endl;
      Rcpp::Rcout << "nparm = "  << nparm               << std::endl;
      Rcpp::Rcout << "nparmc = " << nparmc              << std::endl;
      Rcpp::stop("Execution stopped at setup.cpp");

   }

// COMPUTE THE MODEL COMPLEXITY INDEX
//
// This part implies something special about the first
// dist parameter kmccde
// 0 - No regression and simple distribution
// 1 - Only location regr with int and simple dist (i.e. <12)
//     can use upest short cut
// 2 - Location regr with int any dist or other relationships
//     can still translate but cannot use upest translate
// 3 - Location regr with no int (do we use this??) perhaps
//     as an indicator of whether to center data or use
//     percentile stuff

kmccde = 0;

// First, check for kmccde = 3
   if((kmod == 3) or (kdist > 12) or (intd.at(0) == 0)) {
      
       kmccde = 3;
       goto line100;
      
   }

// Next, check for kmccde = 2
   if(((nxd.at(0) == 0) and (nregr > 1)) or (nregr > 2)) {
      
        kmccde = 2;
        goto line100;
      
   }

// Finally, check for kmccde = 1
   if(nregr > 0) {
      
      kmccde = 1;
      goto line100;
      
   }

// Set constants and pointers in common
   line100: stset(kdist,llog,kmod,ipkode,nparm,ngame,
                  ipxbar,ipxbru,ipsd,ipy,ncoly,nrownw,ipx,ncolx,
                  ipcode,ipweig,ipty,ncolty,ipinow,ipiscd,ipplab,iptc);

   if(debug::kprint >= 5){
   
      Rcpp::Rcout << "\nSETUP AFTER STSET\n" << std::endl;
      Rcpp::Rcout << "kmccde = " << kmccde << std::endl;
      
   }

   
// Figure out which observations are to be used
   fdin(genx01::g_ipcode,genx01::g_ipweig,
        genx01::g_ipinow,genx00::g_nrownw);

// if(debug::kprint >= 6)call getdum(1);

// Figure out which explanatory variables are to be centered
   fdiscd(genx05::g_ipiscd,
          genx05::g_ncolx,
          intd,ipxcd,irelad,nxd,
          npard,kcentr,kmod,ier);

// // Transform y and scale the x matrix, if requested
//    if(debug::kprint >= 4) {
//
//       dgends(kmod,kdist,intd,nxd,nregr,ipxcd,irelad,npard,
//              theta,thetas,kodet,ifix,nparm,ipy,ncoly,nrownw,
//              ipx,ncolx,ipcode,ipweig,ipty,ncolty,iptc,kcentr,
//              ipplab);
//
//    }
//
   trdat(genx01::g_ipy,
         genx00::g_ncoly,
         genx00::g_nrownw,
         genx05::g_ipx,
         genx05::g_ncolx,
         genx01::g_ipweig,
         genx01::g_ipty,
         genx00::g_ncolty,
         genx07::g_llog,
         genx05::g_ipiscd,
         genx05::g_ipxbar,
         genx05::g_ipxbru,
         genx05::g_ipsd,
         ier);

//if(debug::kprint >= 4)call getdum(1);

// Get the scaled versions of the theta values
   scpx(theta,thetas);

// Copy kodet back for return;
   wqm_copyi(genx03::g_ipkode,kodet,genx07::g_nparm);

// // Need to figure out a way to update this
// if(debug::kprint >= 4) {
//    
// write(6,431);
// line431 format(/,/,3x,'current model parameter values'/,/;
// &8x,'parameter'/;
// &' number',2x,'name',5x,'code',5x,'start value'/;
// &1x,38('-'));
// do 22 i=1,nparm;
// call indlab(i,iil,iiu);
// write(6,432)i,(is(ipplab+ii-1),ii=iil,iiu),kodet(i),theta(i);
// line432 format(1x,i3,4x,8a1,3x,i2,5x,f10.3);
// line22 continue;
// call myskip(1,3);
// line405 if(kprint >= 4)call printp(is(ipplab),theta,nparm,1);
// if(kprint >= 4)call printp(is(ipplab),thetas,nparm,1);
//    
// }

return;

}
