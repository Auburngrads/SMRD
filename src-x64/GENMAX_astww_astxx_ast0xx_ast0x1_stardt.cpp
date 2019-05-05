#include <base/base.hpp>
#include <genmax/lspar.hpp>
#include <genmax/regprp.hpp>

//' Fill up thetas with simple start values from non par cdf
//' what we do here depends on the distribution

void stardt(Rcpp::NumericVector &yplot,
            Rcpp::NumericVector &pplot,
            int &mplot,
            int &kdist,
            Rcpp::NumericVector &thetas,
            int &nparm,
            Rcpp::IntegerVector &intd,
            Rcpp::IntegerVector &nxd){
  
// Get estimates of selected percentiles
   double xloc,scale,shape;
   int kdists,lscale,kodeti,iparm;

// Location-scale with cdf inverse--keep original distribution
   if((kdist > 0) & (kdist < 7)){
     
       kdists = kdist;
       lscale = 1;
       lspar(pplot,yplot,mplot,kdists,lscale,xloc,scale);
       thetas.at(0) = thetas.at(0) + xloc;
       
       // Check for scale regression
          kodeti = 2;
          iparm = 2;
          regprp(scale,iparm,thetas,intd,nxd,kodeti);
     
   }

// Exponential distribution--only location parameter
   if((kdist == 7) or (kdist == 8)){
     
       kdists = 1;
       lscale = 0;
       lspar(pplot,yplot,mplot,kdists,lscale,xloc,scale);
       thetas.at(0) = thetas.at(0) + xloc;
     
   }
   
// Go from gen gamma to normal/lognormal
   if((kdist == 9) or (kdist == 10)){
     
       kdists = 3;
       lscale = 1;
       lspar(pplot,yplot,mplot,kdists,lscale,xloc,scale);
       thetas.at(0) = thetas.at(0) + xloc;

       // Check for scale regression
          kodeti = 2;
          iparm = 2;
          regprp(scale,iparm,thetas,intd,nxd,kodeti);
  
       // Check for shape regression
          kodeti = 1;
          shape = one;
          iparm = 3;
          regprp(shape,iparm,thetas,intd,nxd,kodeti);
     
   }

// Go from gamma back to exponential
   if((kdist == 11) or (kdist == 12)){
      
       kdists = 1;
       lscale = 0;
       lspar(pplot,yplot,mplot,kdists,lscale,xloc,scale);
       thetas.at(0) = thetas.at(0) + xloc;
       
       // Check for shape regression
          kodeti = 1;
          shape = one;
          iparm = 2;
          regprp(shape,iparm,thetas,intd,nxd,kodeti);
     
   }
   
if(debug::kprint >= 3){
  
   Rcpp::Rcout << "\nSTARDT**3**\n" << std::endl;
   Rcpp::Rcout << "kdist = " << kdist << std::endl;
// Rcpp::Rcout << "y1 = " << y1 << std::endl;
// Rcpp::Rcout << "y2 = " << y2 << std::endl;
// Rcpp::Rcout << "p1 = " << p1 << std::endl;
// Rcpp::Rcout << "p2 = " << p2 << std::endl;
   Rcpp::Rcout << "xloc = " << xloc << std::endl;
   Rcpp::Rcout << "scale = " << scale << std::endl;
   Rcpp::Rcout << "thetas = " << thetas << std::endl;
  
}

return;

}