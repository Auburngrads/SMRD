#include <base/base.hpp>
#include <genmax/rgami.hpp>

using namespace genx03;
using namespace genx20;
using namespace genx21;

//' Set the gamme vector for constants outside of the summations

void ptgame(Rcpp::NumericVector &thetas){
   
Rcpp::IntegerVector xpoint;
int now;
Rcpp::IntegerVector NXG = clone(genx20::g_nxg);
Rcpp::IntegerVector INTG = clone(genx20::g_intg);
Rcpp::IntegerVector NTERG = clone(genx20::g_nterg);
Rcpp::IntegerVector IGTYG = clone(genx21::g_igtyg);
Rcpp::IntegerVector IPTHET = clone(genx21::g_ipthet);
Rcpp::NumericVector IPSD = clone(genx05::g_ipsd);
Rcpp::IntegerVector IRELAG = clone(genx21::g_irelag);
Rcpp::NumericVector GAMMS = clone(genx21::g_gamms);
Rcpp::List IPXCG = clone(genx20::g_ipxcg);
   
// Iterate over all parameters and functions of parameters
   for(int igam = 1; igam <= genx03::g_ngame; igam++){
      
       // Skip if the 'parameter' is one that is computed as a direct function
       // of a regular distribution parameter (nterg=0) because we just got it
          if(NTERG.at(igam - 1) == 0) continue;
           
       // Otherwise take relationship info below to get the values from exp vars
       // the observation number is not important here, so use the first observation
          SEXP x = IPXCG[igam - 1];
          Rcpp::IntegerVector xpoint(x);
       
          if(debug::kprint >= 5){
             
             Rcpp::Rcout << "\nPTGAME BEFORE RGAMI\n" << std::endl;
             Rcpp::Rcout << "      igam = " << igam - 1 << std::endl;
             Rcpp::Rcout << " nxg(igam) = " << NXG.at(igam - 1) << std::endl;
             Rcpp::Rcout << "     nterg = " << NTERG << std::endl;
             Rcpp::Rcout << "intg(igam) = " << INTG.at(igam - 1) << std::endl;
             Rcpp::Rcout << "    xpoint = " << xpoint << std::endl;
             Rcpp::Rcout << "    ipthet = " << IPTHET << std::endl;
             Rcpp::Rcout << "     igtyg = " << IGTYG << std::endl;
             Rcpp::Rcout << "    irelag = " << IRELAG << std::endl;
             Rcpp::Rcout << "    thetas = " << thetas << std::endl;
             Rcpp::Rcout << "     gamms = " << GAMMS << std::endl;
             
          }
 
          now = 1;
          rgami(now,igam,NXG.at(igam - 1),NTERG.at(igam - 1),
                INTG.at(igam - 1),xpoint,IPTHET.at(igam - 1),
                IGTYG.at(igam - 1),IRELAG.at(igam - 1),thetas,GAMMS);
         
   }
   
   genx20::g_nxg = clone(NXG);
   genx20::g_intg = clone(INTG);
   genx20::g_nterg = clone(NTERG);
   genx21::g_igtyg = clone(IGTYG);
   genx21::g_ipthet = clone(IPTHET);
   genx05::g_ipsd = clone(IPSD);
   genx21::g_irelag = clone(IRELAG);
   genx21::g_gamms = clone(GAMMS);
   genx20::g_ipxcg = clone(IPXCG);

if(debug::kprint >= 6){
   
   Rcpp::Rcout << "\nPTGAME**6**\n" << std::endl;
   Rcpp::Rcout << "thetas = " << thetas << std::endl;
   Rcpp::Rcout << " gamms = " << genx21::g_gamms << std::endl;
   
}

return;

}