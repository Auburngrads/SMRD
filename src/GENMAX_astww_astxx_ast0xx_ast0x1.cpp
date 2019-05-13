#include <base/base.hpp>
#include <genmax/cdfggg.hpp>
#include <genmax/stardt.hpp>
#include <genmax/ilsxx.hpp>
#include <wqmpoints/wqm_points.hpp>

//' Root finding routine for ast0xx
void ast0x1(int &kmod,
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
            Rcpp::NumericMatrix &y,
            int &ncoly,
            int &nrownw,
            Rcpp::NumericMatrix &x,
            int &ncolx,
            Rcpp::IntegerVector &pcodes,
            Rcpp::IntegerVector &weight,
            Rcpp::NumericMatrix &ty,
            int &ncolty,
            Rcpp::IntegerVector &tc,
            int &kcentr,
            Rcpp::IntegerVector &iplab,
            int &maxit,
            Rcpp::NumericMatrix &vcvs,
            Rcpp::NumericMatrix &vcv,
            Rcpp::NumericMatrix &r,
            int &ier,
            Rcpp::NumericMatrix &resid,
            Rcpp::NumericMatrix &yhat,
            Rcpp::NumericMatrix &times,
            Rcpp::NumericVector &p,
            Rcpp::NumericVector &q,
            Rcpp::NumericVector &prob,
            Rcpp::NumericVector &sd,
            Rcpp::NumericVector &thtmp,
            Rcpp::NumericVector &yplot,
            Rcpp::NumericVector &pplot,
            Rcpp::NumericVector &sdplot,
            int &nstart,
            int &maxmsd,
            double &tol,
            int &lsd,
            double &pchmax){
  
int m = q.size(), mplot = 0;

// Now take care of location regression (if needed)
   int ncolyx = ncoly;
   times = clone(y);

// use normal dist em for starting values for the regression parameters;
   if(nxd.at(0) != 0) {
   
      // get number of terms in location regression;
         int nter = nxd.at(0) + intd.at(0);
         
         if(debug::kprint >= 12) {
            
            Rcpp::Rcout << "\nBEFORE ILSXX\n" << std::endl;
            Rcpp::Rcout << "times = \n" << times << std::endl;
            Rcpp::Rcout << "resid = \n" << resid << std::endl;
            Rcpp::Rcout << "vcv = \n"   << vcv   << std::endl;
            
         }
         
         ilsxx(thetas,thtmp,kodet,ifix,nter,nparm,y,ncoly,
               times,pcodes,weight,nrownw,x,ncolx,iplab,
               vcv,r,yhat,resid,ier);
          
         ncolyx = 1;
         times.column(0) = resid.column(0);
         
         if(debug::kprint >= 12) {
            
            Rcpp::Rcout << "\nAFTER ILSXX\n"     << std::endl;
            Rcpp::Rcout << "times = \n" << times << std::endl;
            Rcpp::Rcout << "resid = \n" << resid << std::endl;
            Rcpp::Rcout << "vcv = \n"   << vcv   << std::endl;
            
         }

      
      // Change the codes so that intervals look like 
      // exact failures when we go to cdfggg / cdfest
         for(int i = 1; i <= nrownw; i++){
           
             if(pcodes.at(i - 1) == 4) pcodes.at(i - 1) = 1;
         
         }
   
   }

// Compute nonparametric estimate of cdf
    cdfggg(times,ncolyx,pcodes,weight,ty,ncolty,tc,
           nrownw,nstart,maxit,tol,maxmsd,p,
           q,prob,sd,m,pchmax,lsd,ier);
 
// ier = 21 is not serious here
   if(ier == 21) ier = 0;
   if(ier > 1) Rcpp::stop("\nier error at ast0x1 -- ier = %i",ier);

   wqm_points(q,p,prob,sd,lsd,m,yplot,pplot,sdplot,mplot);
 
// compute location (ignored in regression) and scale estimate using
// nonparametric estimate and simple location-scale distribution
// and do other distribution-specific stuff
   stardt(yplot,pplot,mplot,kdist,thetas,nparm,intd,nxd);
 
 return;
 
}
