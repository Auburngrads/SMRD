#include <base/base.hpp>
#include <utility/dlogc.hpp>
#include <utility/dexpc.hpp>
#include <utility/dcheck.hpp>
#include <spgeng/gcdf.hpp>
#include <genfun/gcdfm.hpp>
#include <sgpdfl/gpdf.hpp>
#include <sgquan/gquant.hpp>
#include <genfun/gmquan.hpp>

using namespace genx09;

//' single distribution functions
//' compute interesting functions of gamma parameter vector
//' 
//'        kfuncf        function
//'          1            failure probability
//'          2            quantile
//'          3            hazard rate

double func0(Rcpp::NumericVector gamme,
             int ngame,
             int kdist,
             int kfuncf,
             int llog,
             double funarg){
               
double xbill = 1.0000e00,scale,tfarg,fquant;
double func_0 = zero,phis,phibm,frate;
int ier = 0,kfunca;

// Get absolute value (negative indicates need for constraint shift)
   kfunca = std::abs(kfuncf);
   scale = one;
   if(genx09::kscloc > 0) scale = gamme.at(genx09::kscloc - 1);
 
// Population failure probability as a function of time
// later we will allow a negative kfuncf here for probability constraint
   if(kfunca == 1){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      func_0 = gcdf(tfarg,gamme,kdist);
     
   }  
   
// Population quantiles
   if(kfunca == 2){
     
      if((funarg < zero) or (funarg >= one)) {
        
          Rcpp::stop("\nFUNARG ERROR IN FUNC0\n");
        
      }
      
      if(kfuncf > 0) goto line1025;
      
      // Return intercept from quantile estimate in gamme(1)
         fquant = gmquan(funarg,gamme,kdist);
         goto line1026;
         
      // Regular quantile returned
         line1025:  fquant = gquant(funarg,gamme,kdist);
         line1026:  if(llog == 1) fquant = dexpc(fquant);
                    func_0 = fquant;
                    
   }
   
// Population failure rate
   if(kfunca == 3){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      phis = gpdf(tfarg,gamme,kdist);
      phibm = gcdfm(tfarg,gamme,kdist);
      dcheck(phibm,1.0e-30,1.0e00,1.0e-30,1.0e00,ier,-98200);
      frate = (phis) / (phibm) * xbill;
      if(llog != 0) {
        
         frate = frate / (funarg);
        
      }
      
      func_0 = frate;
     
   }

if(debug::kprint >= 6){
  
   Rcpp::Rcout << "\nFUNC0**6**\n" << std::endl;
   Rcpp::Rcout << "kfuncf = " << kfuncf << std::endl;
   Rcpp::Rcout << "kdist = " << kdist << std::endl;
   Rcpp::Rcout << "llog = " << llog << std::endl;
   Rcpp::Rcout << "func0 = " << func_0 << std::endl;
   Rcpp::Rcout << "funarg = " << funarg << std::endl;
   Rcpp::Rcout << "scale = " << scale << std::endl;
   Rcpp::Rcout << "gamme = " << gamme << std::endl;
  
}

return func_0;

}