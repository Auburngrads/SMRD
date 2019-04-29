#include <base/base.hpp>
#include <genmax/kpopgt.hpp>
#include <genmax/fxupes.hpp>
#include <genmax/fxpest.hpp>

using namespace genx08;
using namespace genx07;

//' Set up possible constraints for regular estimation
//' 
//' if there is a transformable location parameter, pick
//' an appropriate percentile at the center of the
//' data set to estimate instead
//' 
//' we will later compute thetasp=mu(x)+upest(x)*scale(x)
//' with special short cut if upest and scale do not depend on x
//' 
//'  klmode = 2 constraining a quantile estimate
//'            set kparm=1, upest=wqm_quant(p,idist)
//'            pass kpoint

void fxest(double &pfail,
           double &pestp,
           int &kdist,
           int &kmccde,
           int &kmod){
   
int klmode,kpoint,kparm,kpopu,lupest;
double pest,upest;
   
// kparm probably not needed here,
// but we must set it and bring it down for the common save below
   klmode = 2;
   kpoint = 0;
   kparm = 0;
   
// Get default population for estimation conversion
   kpopu = kpopgt(kmod);
   
// Get pest
   fxpest(pestp,pfail,kmccde,pest);
   
// Get upest
   fxupes(pest,kmccde,kmod,kdist,upest,lupest);
   genx08::g_pest = pest;
   genx08::g_upest = upest;
   genx08::g_kparm = kparm;
   genx08::g_klmode = klmode;
   genx08::g_kpoint = kpoint;
   genx07::g_kmccde = kmccde;
   genx07::g_kpopu = kpopu;
   
   if(debug::kprint >= 4){
      
      Rcpp::Rcout << "\nFXEST**4**\n"      << std::endl;
      Rcpp::Rcout << "klmode = " << klmode << std::endl;
      Rcpp::Rcout << "kmccde = " << kmccde << std::endl;
      Rcpp::Rcout << "kpoint = " << kpoint << std::endl;
      Rcpp::Rcout << "kparm = "  << kparm  << std::endl;
      Rcpp::Rcout << "pfail = "  << pfail  << std::endl;
      Rcpp::Rcout << "pest = "   << pest   << std::endl;
      Rcpp::Rcout << "upest = "  << upest  << std::endl;
      Rcpp::Rcout << "kpopu = "  << kpopu  << std::endl;
      
   }
   
return;
   
}
