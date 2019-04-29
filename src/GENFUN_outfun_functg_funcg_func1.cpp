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

//' Compute interesting functions of gamma parameter vector for the lfp model
//' 
//'        kfuncf        function
//'          1            population failure probability
//'          2            population quantile
//'         -2            gamma(1) from population quantile
//'          3            population hazard rate
//'          4            subpopulation failure probability
//'          5            subpopulation quantile
//'         -5            gamma(1) from subpopulation quantile
//'          6            subpopulation hazard rate

double func1(Rcpp::NumericVector gamme,
             int ngame,
             int kdist,
             int kfuncf,
             int llog,
             int funarg){
  
double xbill = 1.0000e00,big = 1.0e15;
double func_1 = zero,scale,p,pminus,tfarg;
double phis,phib,phibm,fquant,frate;
int kfunca,ier = 0;

// Pick up scale and probability
   scale = one;
   if(genx09::kscloc > 0) scale = gamme.at(genx09::kscloc - 1);
   p = gamme.at(genx09::kprloc - 1);
   
// We need pminus because if funarg is real close to p, the
// routines that compute derivatives will blow up
   pminus = std::max(zero,p - 0.0001e00);
   kfunca = std::abs(kfuncf);

// Population failure probability as a function of time
   if(kfunca == 1){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      func_1 = p * gcdf(tfarg,gamme,kdist);
     
   }

// Population distribution quantiles
// first check for valid quantile value
   if(kfunca == 2){
  
      if((funarg <= zero) or (funarg >= one)) {
        
          Rcpp::stop("\nFUNARG ERROR IN FUNC1\n");
        
      }
      
       // #if(phat < q < 1) wqm_quant=infinity
          func_1 = big;
          if(!((funarg <= zero) or (funarg >= pminus))){
            
                  if(kfuncf > 0) goto line1025;
                  
               // Return location parameter from quantile estimate
                  fquant = gmquan(funarg / p,gamme,kdist);
                  goto line1026;
                  
               // Regular quantile from parameters
                  line1025:  fquant = gquant(funarg / p,gamme,kdist);
                  line1026:  if(llog == 1) fquant = dexpc(fquant);
                  
                  func_1 = fquant;
          }

}
   
// Population failure rate times 1.0e09
   if(kfunca == 3){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      phis = gpdf(tfarg,gamme,kdist);
      phib = gcdf(tfarg,gamme,kdist);
      phibm = one - p * phib;
      dcheck(phibm,1.0e-30,1.0e00,1.0e-30,1.0e00,ier,-98210);
      frate = (p * phis) / (phibm) * xbill;
      if(llog != 0) {
        
         // This needs to be checked
            frate = frate / (funarg);
        
      }
      func_1 = frate;
      
   }
   
// Subpopulation failure probability as a function of time
   if(kfunca == 4){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      func_1 = gcdf(tfarg,gamme,kdist);
      
   }
   
// Subpopulation distribution quantiles
   if(kfunca == 5){
     
      if((funarg <= zero) or (funarg >= one)) {
        
          Rcpp::stop("\nFUNARG ERROR IN FUNC1\n");
        
      }
      
      if(kfuncf > 0) goto line1055;
      
      // Return location parameter from subpopulation quantile estimate
         fquant = gmquan(funarg,gamme,kdist);
         goto line1056;
         
      // Regular subpopulation quantile from parameters
         line1055: fquant = gquant(funarg,gamme,kdist);
         line1056: if(llog == 1)fquant = dexpc(fquant);
                  
         func_1 = fquant;
  
   }
   
// Subpopulation failure rate times 1.0e09
   if(kfunca == 6){
     
      tfarg = funarg;
      if(llog == 1) tfarg = dlogc(funarg);
      phis = gpdf(tfarg,gamme,kdist);
      phibm = gcdfm(tfarg,gamme,kdist);
      dcheck(phibm,1.0e-30,1.0e00,1.0e-30,1.0e00,ier,-98211);
      frate = (phis / phibm) * xbill;
      
      if(llog != 0) {
        
         // This needs to be checked
            frate = frate / (funarg);
        
      }
      
      func_1 = frate;
        
   }
   
if(debug::kprint >= 6){
  
   Rcpp::Rcout << "\nFUNC1**6**\n" << std::endl;
   Rcpp::Rcout << "kfuncf = " << kfuncf << std::endl;
   Rcpp::Rcout << "kdist = " << kdist << std::endl;
   Rcpp::Rcout << "llog = " << llog << std::endl;
   Rcpp::Rcout << "func1 = " << func_1 << std::endl;
   Rcpp::Rcout << "funarg = " << funarg << std::endl;
   Rcpp::Rcout << "scale = " << scale << std::endl;
   Rcpp::Rcout << "gamme = " << gamme << std::endl;
  
}

return func_1;

}