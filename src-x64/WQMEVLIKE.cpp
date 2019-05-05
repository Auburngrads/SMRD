#include <base/base.hpp>
#include <wqmmlesss/wqm_tran.hpp>
#include <wqmmlesss/wqm_toffst.hpp>
#include <wqmmlesss/wqm_comgam.hpp>
#include <wqmmlesss/wqm_flike.hpp>
#include <wqmmlesss/wqm_fpfail.hpp>
#include <utility/wqm_quant.hpp>
#include <wqmmlesss/wqm_svdfix.hpp>
using namespace lstd;

//' Wrap for wqm_flike to vectorize the 
//' computation of the likelihood
// [[Rcpp::export]]
Rcpp::List WQMEVLIKE(Rcpp::NumericMatrix xold,
                     Rcpp::NumericMatrix y,
                     Rcpp::IntegerVector cen,
                     Rcpp::IntegerVector wt,
                     Rcpp::NumericMatrix ty,
                     Rcpp::IntegerVector tcodes,
                     Rcpp::NumericVector gamthr,
                     int nrow,
                     int ny,
                     int nty,
                     int nparm,
                     int intcpt,
                     int nter,
                     Rcpp::NumericMatrix thetav,
                     Rcpp::LogicalVector lfix,
                     int ntheta,
                     double fpfxxx,
                     double upcen,
                     int kdist,
                     Rcpp::NumericVector thetb,
                     Rcpp::NumericVector thetg,
                     Rcpp::NumericMatrix xnew,
                     Rcpp::NumericVector diag,
                     Rcpp::NumericMatrix tmat,
                     Rcpp::NumericVector rv1,
                     Rcpp::NumericMatrix vcvg,
                     int kprinp,
                     Rcpp::NumericVector xlike,
                     int ier){

bool ltr3,ltyc3,lsvd = true;
double temp, dgmin = 0;
lstd::g_ltp = false;
debug::kprint = kprinp;
Rcpp::List FLIKE;

if(debug::kprint >= 2) {
  
      Rcpp::Rcout << "\nWQMEVLIKE**1**\n"   << std::endl;
      Rcpp::Rcout << "nrow = "    << nrow   << std::endl;
      Rcpp::Rcout << "nter = "    << nter   << std::endl;
      Rcpp::Rcout << "ny = "      << ny     << std::endl;
      Rcpp::Rcout << "nty = "     << nty    << std::endl;
      Rcpp::Rcout << "nparm = "   << nparm  << std::endl;
      Rcpp::Rcout << "ntheta = "  << ntheta << std::endl;
      Rcpp::Rcout << "kdist = "   << kdist  << std::endl;
      Rcpp::Rcout << "xold = "    << xold   << std::endl;
      Rcpp::Rcout << "thetav = "  << thetav << std::endl;
      Rcpp::Rcout << "y = "       << y      << std::endl;
      Rcpp::Rcout << "codes = "   << cen    << std::endl;
      Rcpp::Rcout << "weights = " << wt     << std::endl;
      Rcpp::Rcout << "gamthr = "  << gamthr << std::endl;
}

// Data setup stuff from mlbot1

fpfxxx = wqm_fpfail(cen,wt,nrow);
upcen  = wqm_quant(fpfxxx / 2.0e00,kdist);

if(intcpt == 0) upcen = 0.0e00;

 wqm_svdfix(lfix,nrow,nter,nparm,xold,xnew,diag,
            tmat,dgmin,ier,vcvg,rv1,lsvd);

ltr3  = false;
ltyc3 = false;

if((kdist % 2) == 0) {

   wqm_tran(y,cen,wt,nrow,ny,gamthr,false,ltr3,ier);

   wqm_toffst(gamthr,y,nrow,ny);

   if(nty > 0) {

      wqm_tran(ty,tcodes,wt,nrow,nty,gamthr,true,ltyc3,ier);

      wqm_toffst(gamthr,ty,nrow,nty);

   }

}

if(debug::kprint >= 2) {
  
      Rcpp::Rcout << "WQMEVLIKE**2**" << std::endl;
      Rcpp::Rcout << "xnew =" << xnew << std::endl;
      Rcpp::Rcout << "diag =" << diag << std::endl;
      Rcpp::Rcout << "tmat =" << tmat << std::endl;
      Rcpp::Rcout << "log (if needed) y = " << y << std::endl;

}

// Loop over the different theta values to compute the likelihoods

for(int itheta = 0; itheta < ntheta; itheta++){

    thetb = thetav.column(itheta);
    thetb.at(0) = thetb.at(0) + upcen * thetb.at(nparm - 1);

    if(debug::kprint >= 2) {
      
      Rcpp::Rcout << "\nthetg before comgam =" << thetg << std::endl;
      
    }
    
    // Check for zero sigma start value
    wqm_comgam(thetb,diag,tmat,nparm,thetg);
    temp  = thetg.at(nparm - 1);
    if(temp >  0.0e00) thetg.at(nparm - 1) = std::log(thetg.at(nparm - 1));
    if(temp <= 0.0e00) thetg.at(nparm - 1) = 0.0e00;

    if(debug::kprint >= 2) {
      
         Rcpp::Rcout << "\nthetg & thetb before flike =" << std::endl;
         Rcpp::Rcout << "thetb = " << thetb << std::endl;
         Rcpp::Rcout << "thetg = " << thetg << std::endl;
      
    }

    
    // thetag is used first below
       FLIKE = wqm_flike(y,xnew,cen,wt,nty,ty,
                         tcodes,nrow,nter,ny,diag,
                         tmat,thetb,kdist,thetg,
                         nparm,upcen);
    
       xlike.at(itheta) = -1 * Rcpp::as<double>(Rcpp::as<List>(FLIKE)["flike"]);
       //thetb = Rcpp::as<Rcpp::NumericVector>(Rcpp::as<List>(FLIKE)["thetb"]);


}

      return Rcpp::List::create(Named("y")  = y,
                                Named("ty") = ty,
                                Named("cen") = cen,
                                Named("xlike") = xlike,
                                Named("xnew")  = xnew,
                                Named("diag")  = diag,
                                Named("upcen") = upcen,
                                Named("thetg") = thetg,
                                Named("thetb") = thetb,
                                Named("vcvg")  = vcvg,
                                Named("ierr")  = ier,
                                Named("rv1")   = rv1,
                                Named("thetav") = thetav,
                                Named("xold")   = xold,
                                Named("fpfail") = fpfxxx,
                                Named("tmat")   = tmat);
  
}