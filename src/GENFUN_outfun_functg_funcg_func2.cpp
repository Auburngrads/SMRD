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

//' Compute interesting functions of gamma parameter vector for the doa model
//' 
//'        kfuncf        function
//'          1            population failure probability
//'          2            population quantile
//'         -2            usual location parameter gamma(1)
//'                       computed from population quantile in funarg
//'          3            population hazard rate
//'          4            subpopulation failure probability
//'          5            subpopulation quantile
//'         -5            gamma(1) from subpopulation quantile
//'          6            subpopulation hazard rate

double func2(Rcpp::NumericVector gamme,
             int ngame,
             int kdist,
             int kfuncf,
             int llog,
             int funarg){
  
double xbill = 1.0000e00,big = 1.0e15;
double func_2 = zero,scale,p,pt,pminus,tfarg;
double phis,phibm,fquant,frate;
int kfunca,ier = 0;

// Pick up scale and probability
   scale = one;
   if(genx09::kscloc > 0) scale = gamme.at(genx09::kscloc - 1);
   p = one - gamme.at(genx09::kprloc - 1);
        
// We need pminus because if funarg is real close to p, the
// routines that compute derivatives will blow up
   pminus = std::max(zero,p - 0.0001e00);
   kfunca = std::abs(kfuncf);
   
// Population failure probabilities
   if(kfunca == 1){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      func_2 = p + (one - p) * gcdf(tfarg,gamme,kdist);
        
   }
      
// Population distribution quantiles
   if(kfunca == 2){
     
      if((funarg <= zero) or (funarg >= one)) {
        
          Rcpp::stop("\nFUNARG ERROR IN FUNC2\n");
        
      }
      
      // if (0 < q < phat )wqm_quant=zero   or -infinity
         func_2 = -big;
         if(llog == 1) func_2 = zero;
         if(!((funarg <= zero) or (funarg <= pminus))){
           
               pt = (funarg - p) / (1.0e00 - p);
               if(kfuncf > 0) goto line2025;
               
            // Return estimate of location parameter or
            // intercept for given quantile estimate
               fquant = gmquan(pt,gamme,kdist);
               goto line2026;
               
            // Regular distribution quantile
               line2025: fquant = gquant(pt,gamme,kdist);
               line2026: if(llog == 1) fquant = dexpc(fquant);
               
               func_2 = fquant;
        
         }
  
   }

// Population and subpopulation failure rate times 1.0e09
   if(kfunca == 3){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      phis = gpdf(tfarg,gamme,kdist);
      phibm = gcdfm(tfarg,gamme,kdist);
      
      dcheck(phibm,1.0e-30,1.0e00,1.0e-30,1.0e00,ier,-98220);
      frate = (phis / phibm) * xbill;
      if(llog != 0) frate = frate / (funarg);
      
      func_2 = frate;
        
   }
      
// Subpopulation failure probabilities
   if(kfunca == 4){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      func_2 = gcdf(tfarg,gamme,kdist);
        
   }
      
// Subpopulation distribution quantiles
   if(kfunca == 5){
     
      if((funarg <= zero) or (funarg >= one)) {
        
          Rcpp::stop("\nFUNARG ERROR IN FUNC2\n");
        
      }
      
      if(kfuncf > 0) goto line2055;
      
      // Return estimate of location parameter or intercept
      // for a given subpopulation quantile estimate
         fquant = gmquan(funarg,gamme,kdist);
         goto line2056;
         
      // Regular subpopulation quantile
         line2055: fquant = gquant(funarg,gamme,kdist);
         line2056: if(llog == 1) fquant = dexpc(fquant);
         
         func_2 = fquant;
  
   }
   
if(debug::kprint >= 6){
  
   Rcpp::Rcout << "\nFUNC2**6**\n" << std::endl;
   Rcpp::Rcout << "kfuncf = " << kfuncf << std::endl;
   Rcpp::Rcout << "kdist = " << kdist << std::endl;
   Rcpp::Rcout << "llog = " << llog << std::endl;
   Rcpp::Rcout << "func2 = " << func_2 << std::endl;
   Rcpp::Rcout << "funarg = " << funarg << std::endl;
   Rcpp::Rcout << "scale = " << scale << std::endl;
   Rcpp::Rcout << "gamme = " << gamme << std::endl;
  
}

return func_2;

}
