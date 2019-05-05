#include <base/base.hpp>
#include <utility/dlogc.hpp>
#include <utility/dexpc.hpp>
#include <utility/dcheck.hpp>
#include <spgeng/gcdf.hpp>
#include <genfun/gcdfm.hpp>
#include <sgpdfl/gpdf.hpp>
#include <sgquan/gquant.hpp>
#include <genfun/gmquan.hpp>
#include <genfun/gphhaz.hpp>
#include <genfun/gmphqu.hpp>
#include <genfun/gphcdf.hpp>
#include <genfun/gphqu.hpp>

using namespace genx09;

//' Compute interesting functions of gamma parameter vector
//' 
//'        kfuncf        function
//'          1            failure probability
//'          2            quantile
//'          3            hazard rate

double func4(Rcpp::NumericVector gamme,
             int ngame,
             int kdist,
             int kfuncf,
             int llog,
             int funarg){
  
double xbill = 1.0000e00,power,fquant,frate;
double func_4 = zero,scale,tfarg;
int kfunca;

// Get power location
   power = gamme.at(genx09::g_kpwloc - 1);
   kfunca = std::abs(kfuncf);
   
// Population failure probability as a function of time
   if(kfunca == 1){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      func_4 = gphcdf(tfarg,gamme,power,kdist);
        
   }
      
// Population quantiles
   if(kfunca == 2){
     
      if((funarg <= zero) or (funarg >= one)) {
        
          Rcpp::stop("\nFUNARG ERROR IN FUNC3\n");
        
      }
      
      if(kfuncf > 0) goto line1025;
      
      // Return intercept from quantile estimate in gamme(1)
         fquant = gmphqu(funarg,gamme,power,kdist);
         goto line1026;
         
      // Regular quantile returned
         line1025: fquant = gphqu(funarg,gamme,power,kdist);
         line1026: if(llog == 1) fquant = dexpc(fquant);
         
      func_4 = fquant;
  
   }

// Population failure rate
   if(kfunca == 3){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      frate = gphhaz(tfarg,gamme,power,kdist);
      
      // Tthe logscale correction is made in gphhaz before power trans
      // #looks like xbill factor is below also
         //cxxx if(llog != 0) frate = frate / (funarg);
         func_4 = frate * xbill;
  
   }

if(debug::kprint >= 6){
  
   Rcpp::Rcout << "\nFUNC4**6**\n" << std::endl;
   Rcpp::Rcout << "kfuncf = " << kfuncf << std::endl;
   Rcpp::Rcout << "kdist = " << kdist << std::endl;
   Rcpp::Rcout << "llog = " << llog << std::endl;
   Rcpp::Rcout << "func4 = " << func_4 << std::endl;
   Rcpp::Rcout << "funarg = " << funarg << std::endl;
   Rcpp::Rcout << "scale = " << scale << std::endl;
   Rcpp::Rcout << "gamme = " << gamme << std::endl;
  
}

return func_4;

}