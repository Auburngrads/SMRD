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

//' Compute interesting functions of gamma parameter vector for the sts model
//' 
//'        kfuncf        function
//'          1            population failure probability
//'          2            second subpopulation quantile
//'         -2            gamma(1) from population quantile
//'          3            population hazard rate

double func3(Rcpp::NumericVector gamme,
             int ngame,
             int kdist,
             int kfuncf,
             int llog,
             int funarg){
  
double xbill = 1.0000e00,fquant,frate;
double func_3 = zero,scale,tfarg;
double phis,phis1,phis2,phis3,phis4,phis5,phis6,phibm;
int kfunca, ier = 0;

      scale = one;
      if(genx09::g_kscloc > 0) scale = gamme.at(genx09::g_kscloc - 1);
      kfunca = std::abs(kfuncf);
      
// Population failure probability as a function of time
   if(kfunca == 1){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      func_3 = one - gcdfm(tfarg,gamme(genx09::g_kpwloc),7) * gcdfm(tfarg,gamme,kdist);
      
   }
      
// Population quantiles
   if(kfunca == 2){
     
      if((funarg <= zero) or (funarg >= one)) {
        
          Rcpp::stop("\nFUNARG ERROR IN FUNC3 -- funarg = %f",funarg);
        
      }
      
      if(kfuncf > 0) goto line1025;
      
      // Return intercept from quantile estimate in gamme(1)
         fquant = gmquan(funarg,gamme,kdist);
         goto line1026;
         
      // Regular quantile returned
      // cxx We need a general quantile finder here.
      // cxx perhaps we can write one that will work with inputted pdf and cdf
         line1025: fquant = gquant(funarg,gamme,kdist);
         line1026: if(llog == 1) fquant = dexpc(fquant);
         
      func_3 = fquant;
  
   }

// Population failure rate
   if(kfunca == 3){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      phis1 = gpdf(tfarg,gamme,kdist);
      phis2 = gcdfm(tfarg,gamme,kdist);
      phis3 = gpdf(tfarg,gamme(genx09::g_kpwloc),7);
      phis4 = gcdfm(tfarg,gamme(genx09::g_kpwloc),7);
      phis5 = gcdfm(tfarg,gamme,kdist);
      phis6 = gcdfm(tfarg,gamme(genx09::g_kpwloc),7);
      phis  = (phis1 / (phis2) + phis3 / (phis4)) * phis5 * phis6;
      phibm = gcdfm(tfarg,gamme(genx09::g_kpwloc),7) * gcdfm(tfarg,gamme,kdist);
      
      dcheck(phibm,1.0e-30,1.0e00,1.0e-30,1.0e00,ier,-98230);
      
      frate = (phis) / (phibm) * xbill;
      
      if(llog != 0) {
        
         // #This needs checking
            frate = frate / (funarg);
        
      }
      
      func_3 = frate;
        
   }
   
if(debug::kprint >= 6){
  
   Rcpp::Rcout << "\nFUNC3**6**\n" << std::endl;
   Rcpp::Rcout << "kfuncf = " << kfuncf << std::endl;
   Rcpp::Rcout << "kdist = " << kdist << std::endl;
   Rcpp::Rcout << "llog = " << llog << std::endl;
   Rcpp::Rcout << "func3 = " << func_3 << std::endl;
   Rcpp::Rcout << "funarg = " << funarg << std::endl;
   Rcpp::Rcout << "scale = " << scale << std::endl;
   Rcpp::Rcout << "gamme = " << gamme << std::endl;
  
}

return func_3;

}
