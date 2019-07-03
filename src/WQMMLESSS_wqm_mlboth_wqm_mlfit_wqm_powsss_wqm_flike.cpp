#include <base/base.hpp>
#include <wqmmlesss/wqm_dfxmu.hpp>
#include <wqmsphiall/wqm_ztran.hpp>
#include <wqmmlesss/wqm_fliket.hpp>
#include <wqmmlesss/wqm_combet.hpp>

bool MOD_2(int x, int m) { return x % m == 0; }

//' Compute the log-likelihood

Rcpp::List wqm_flike(Rcpp::NumericMatrix y,
                     Rcpp::NumericMatrix xnew,
                     Rcpp::IntegerVector cen,
                     Rcpp::IntegerVector wt,
                     int nty,
                     Rcpp::NumericMatrix ty,
                     Rcpp::IntegerVector tcodes,
                     int nrow,
                     int nter,
                     int ny,
                     Rcpp::NumericVector diag,
                     Rcpp::NumericMatrix tmat,
                     Rcpp::NumericVector thetb,
                     int kdist,
                     Rcpp::NumericVector thetg,
                     int nparm,
                     double upcen){
  
bool leven = false;
double dmaxx = 39.0e00, epslike = 1.0e-04;
double sigma = 0.0e00, epsxxx, addon, xmu;
double z = 0.0e00, z2 = 0.0e00;
double trl = 0.0e00, tru = 0.0e00, flike, flikem;
int itype, ittype = 0;

if(MOD_2(kdist,2)) leven = true; 

double flikes = -1.0e15;
double sigmal = thetg.at(nparm - 1);

if(!((sigmal < (-1 * dmaxx)) or (sigmal > dmaxx))) {

    sigma = std::exp(sigmal);
    flikes = 0.0e00;

    for(int i = 1; i <= nrow; i++){

         itype = cen.at(i - 1);
         if(itype == 0) continue;
         epsxxx = zero;
         if(itype == 5) epsxxx = epslike;

// addon is for the t in the denominator of the
// log distributions with addon is actually -log(t-gamthr)

        addon = zero;
        flikem = zero;
        if((leven) and (itype == 1)) addon = -1 * y.at(i - 1,0);
         xmu = wqm_dfxmu(i,xnew,nrow,nter,thetg,nparm,upcen,sigma);
         z = (y.at(i - 1,0) - xmu - epsxxx) / sigma;
         z = wqm_ztran(z,kdist);

         if(itype >= 4){

            z2 = (y.at(i - 1,ny - 1) - xmu + epsxxx) / sigma;
            z2 = wqm_ztran(z2,kdist);
         }

         if(nty > 0) {

            trl = (ty.at(i - 1,0) - xmu) / sigma;
            trl = wqm_ztran(trl,kdist);

            ittype = tcodes.at(i - 1);

            if(ittype == 4){

               tru = (ty.at(i - 1,1) - xmu) / sigma;
               tru = wqm_ztran(tru,kdist);

            }

         }

         flikem = addon + wqm_fliket(itype,z,z2,
                                     sigmal,nty,ittype,
                                     trl,tru,kdist);

         flikes = flikes + flikem * wt.at(i - 1);

    }

}

flike = -flikes;

if(debug::kprint >= 4) {
  
   // Find the current parameter estimates 
   // in terms of the original x matrix
      wqm_combet(thetg,diag,tmat,nparm,thetb);
      thetb.at(0) = thetb.at(0) - upcen * sigma;
  
      Rcpp::Rcout << "\nEND OF WQM_FLIKE\n"  << std::endl;
      Rcpp::Rcout << "flikes = "   << flikes << std::endl;
      Rcpp::Rcout << " thetb = "   << thetb  << std::endl;
      Rcpp::Rcout << "  nter = "   << nter   << std::endl;
      Rcpp::Rcout << " sigma = "   << sigma  << std::endl;
  
}

      return Rcpp::List::create(Named("flike") = flike,
                                Named("thetb") = thetb);
        
}

