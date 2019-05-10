#include <base/base.hpp>
#include <wqmmlesss/wqm_derivt.hpp>
#include <wqmmlesss/wqm_combet.hpp>
#include <wqmmlesss/wqm_dfxmu.hpp>
#include <wqmsphiall/wqm_ztran.hpp>
#include <wqmmlesss/wqm_fliket.hpp>

//' Compute the deviance

void wqm_gtdev(Rcpp::NumericMatrix &y,
               Rcpp::NumericMatrix &xnew,
               Rcpp::IntegerVector &cen,
               Rcpp::IntegerVector &wt,
               int &nty,
               Rcpp::NumericMatrix &ty,
               Rcpp::IntegerVector &tcodes,
               int &nrow,
               int &nter,
               int &ny,
               Rcpp::NumericVector &diag,
               Rcpp::NumericMatrix &tmat,
               Rcpp::NumericVector &thetb,
               int &kdist,
               Rcpp::NumericVector &thetg,
               int &nparm,
               double &upcen,
               Rcpp::NumericMatrix &devian,
               double &flikes){

bool leven = false;
if((kdist % 2) == 0) leven = true;

if(debug::kprint >= 1) {
  
   Rcpp::Rcout << "\nGTDEV**1**\n"    << std::endl;
   Rcpp::Rcout << "leven = " << leven << std::endl;
  
}

double epslike = 1.0e-04, addon;
Rcpp::NumericVector qder(5);

double sigma, sigmal, epsxxx, z = 0.0e00, xmu;
double trl, tru, z2 = 0.0e00, flikem;
int itype, ittype = 1;
flikes  = -1.0e15;

// This is different from wqm_flike where we have to exponentiate
   sigma  = thetg.at(nparm - 1);
   sigmal = std::log(sigma);
   flikes = 0.0e00;

for(int i = 1; i <= nrow; i++){
  
    itype = cen.at(i - 1);
    if(itype == 0) continue;
    epsxxx = zero;

    // June 5, 1994: Found and corrected epsxxx = esplike.
    // Don't know the effect of the error, but this is 
    // probably the reason that code 5 was not working
    // and we had to do this stuff from inside Splus.

    if(itype == 5) epsxxx = epslike;

    // addon is for the t in the denominator of the log 
    // distributions with addon is actually -log(t-gamthr)

    addon = zero;
    if((leven) and (itype == 1)) addon = -1 * y.at(i - 1,0);
    xmu = wqm_dfxmu(i,xnew,nrow,nter,thetg,
                    nparm,upcen,sigma);
    z = (y.at(i - 1,0) - xmu - epsxxx) / sigma;
    z = wqm_ztran(z,kdist);
    
    if(itype >= 4){
      
       z2 = (y.at(i - 1,ny - 1) - xmu + epsxxx) / sigma;
       z2 = wqm_ztran(z2, kdist);
       
    }
    
    if(debug::kprint >= 6){
      
       Rcpp::Rcout << "\nGTDEV**6**\n" << std::endl;
       Rcpp::Rcout << "cen = " << itype << std::endl;
       Rcpp::Rcout << "z = "  << z  << std::endl;
       Rcpp::Rcout << "z2 = " << z2 << std::endl;
      
    }
    
    if(nty > 0){
      
       trl = (ty.at(i - 1,0) - xmu) / sigma;
       trl = wqm_ztran(trl,kdist);
       ittype = tcodes.at(i - 1);
       
       if(ittype == 4){
         
          tru = (ty.at(i - 1,1) - xmu) / sigma;
          tru = wqm_ztran(tru, kdist);
         
       }
    }
    
    flikem = addon + wqm_fliket(itype,z,z2,sigmal,nty,
                                ittype,trl,tru,kdist);
    
	  wqm_derivt(qder,itype,z,z2,nty,
               ittype,trl,tru,kdist);
	  
		devian.at(i - 1,1) = qder.at(0) * wt.at(i - 1);
		devian.at(i - 1,2) = qder.at(2) * wt.at(i - 1);
    flikes = flikes + flikem * wt.at(i - 1);
    devian.at(i - 1,0) =  flikem * wt.at(i - 1);
    
    if(debug::kprint >= 4){
      
       Rcpp::NumericVector devrowi = devian.row(i - 1);
       Rcpp::Rcout << "\nGTDEV**4**\n"           << std::endl;
       Rcpp::Rcout << "           i = " << i - 1 << std::endl;
       Rcpp::Rcout << "          ny = " << ny << std::endl;
       Rcpp::Rcout << "       itype = " << itype << std::endl;
       Rcpp::Rcout << "      ittype = " << ittype << std::endl;
       Rcpp::Rcout << "  devian(i,) = " << devrowi << std::endl;
       Rcpp::Rcout << "        qder = " << qder << std::endl;
       Rcpp::Rcout << "       wt(i) = " << wt.at(i - 1) << std::endl;
       Rcpp::Rcout << "      flikes = " << flikes << std::endl;
       Rcpp::Rcout << "   xnew(i,0) = " << xnew.at(i - 1,0) << std::endl;
       Rcpp::Rcout << "xnew(i,nter) = " << xnew.at(i - 1,nter - 1) << std::endl;
       Rcpp::Rcout << "      y(i,0) = " << y.at(i - 1,0) << std::endl;
       Rcpp::Rcout << "     y(i,ny) = " << y.at(i - 1,ny - 1) << std::endl;
       Rcpp::Rcout << "         xmu = " << xmu << std::endl;
       Rcpp::Rcout << "       sigma = " << sigma << std::endl;
       Rcpp::Rcout << "           z = " << z << std::endl;
      
    }
           
}

if(kprint >= 4) {

   // Find the current parameter estimates in terms 
   // of the original x matrix
   wqm_combet(thetg,diag,tmat,nparm,thetb);
   thetb.at(0) = thetb.at(0) - upcen * sigma;
   
   Rcpp::Rcout << "\nGTDEV**end**\n" << std::endl;
   Rcpp::Rcout << "flikes = " << flikes << std::endl;
   Rcpp::Rcout << "thetb = " << thetb << std::endl;
   Rcpp::Rcout << "sigma = " << sigma << std::endl;

}

return;

}