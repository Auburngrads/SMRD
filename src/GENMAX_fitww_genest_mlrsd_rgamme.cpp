#include <base/base.hpp>
#include <genmax/rgami.hpp>

using namespace genx03;
using namespace genx20;
using namespace genx21;

//' get the gamme vector ready to send to the distribution level
//' we are inside all summations here
//' this is important**************
//' 
//'   gamme description                       action wrt summation
//' nterg     nxg      type                inside                 outside
//' -----------------------------------------------------------------------
//'  1        0     constant           copy from gamms        compute below
//' >0       >0     real regr          compute below          compute below
//'  0        0     derived const.     copy from gamms        picked up above
//'  0       >0     derived regr.      picked up above        picked up above

void rgamme(int &kpnow,
            Rcpp::NumericVector &thetas,
            Rcpp::NumericVector &gamme){

Rcpp::IntegerVector xcols;
Rcpp::IntegerVector NXG = clone(genx20::g_nxg);
Rcpp::IntegerVector INTG = clone(genx20::g_intg);
Rcpp::IntegerVector NTERG = clone(genx20::g_nterg);
Rcpp::IntegerVector IGTYG = clone(genx21::g_igtyg);
Rcpp::IntegerVector IPTHET = clone(genx21::g_ipthet);
Rcpp::NumericVector IPSD = clone(genx05::g_ipsd);
Rcpp::IntegerVector IRELAG = clone(genx21::g_irelag);
Rcpp::NumericVector GAMMS = clone(genx21::g_gamms);

// Iterate over all parameters and functions of parameters
   for(int igam = 1; igam <= genx03::g_ngame; igam++){
      
       // If the parameter does not depend on any explanatory
       // variables, skip and get saved value from gamms
          if(genx20::g_nxg.at(igam - 1) != 0) {
          
             // If the 'parameter' is a direct function, it is already in place so skip
                if(genx20::g_nterg.at(igam - 1) == 0) continue;
                
             // Otherwise take relationship info below to get the values from exp vars
                SEXP x = genx20::g_ipxcg[igam - 1];
                Rcpp::IntegerVector xcols(x);
                
                if(debug::kprint >= 5){
                   
                   Rcpp::Rcout << "\nRGAMME BEFORE RGAMI\n" << std::endl;
                   Rcpp::Rcout << "       kpnow = " << kpnow << std::endl;
                   Rcpp::Rcout << "        igam = " << igam << std::endl;
                   Rcpp::Rcout << "   nxg(igam) = " << NXG.at(igam - 1) << std::endl;
                   Rcpp::Rcout << " nterg(igam) = " << NTERG.at(igam - 1) << std::endl;
                   Rcpp::Rcout << "  intg(igam) = " << INTG.at(igam - 1) << std::endl;
                   Rcpp::Rcout << "       xcols = " << xcols << std::endl;
                   Rcpp::Rcout << "ipthet(igam) = " << IPTHET.at(igam - 1) << std::endl;
                   Rcpp::Rcout << " igtyg(igam) = " << IGTYG.at(igam - 1) << std::endl;
                   Rcpp::Rcout << "irelag(igam) = " << IRELAG.at(igam - 1) << std::endl;
                   Rcpp::Rcout << "      thetas = " << thetas << std::endl;
                   Rcpp::Rcout << "       gamme = " << gamme << std::endl;
                   
                }
                
                rgami(kpnow,igam,NXG.at(igam - 1),NTERG.at(igam - 1),
                      INTG.at(igam - 1),xcols,IPTHET.at(igam - 1),
                      IGTYG.at(igam - 1),IRELAG.at(igam - 1),thetas,gamme);
                
                continue;
          
          }
          
          gamme.at(igam - 1) = genx21::g_gamms.at(igam - 1);
         
   }
   
   genx20::g_nxg = clone(NXG);
   genx20::g_intg = clone(INTG);
   genx20::g_nterg = clone(NTERG);
   genx21::g_igtyg = clone(IGTYG);
   genx21::g_ipthet = clone(IPTHET);
   genx05::g_ipsd = clone(IPSD);
   genx21::g_irelag = clone(IRELAG);
   genx21::g_gamms = clone(GAMMS);

   
   if(debug::kprint >= 7){
      
      Rcpp::Rcout << "\nRGAMME**7**\n"   << std::endl;
      Rcpp::Rcout << " kpnow = " << kpnow << std::endl;
      Rcpp::Rcout << " gamme = " << gamme << std::endl;
      Rcpp::Rcout << "thetas = " << thetas << std::endl;
      
   }
   
return;

}
