#include <base/base.hpp>
#include <genmax/rgami.hpp>

using namespace genx03;
using namespace genx20;
using namespace genx21;

//' Set the gamme vector for constants outside of the summations

void ptgame(Rcpp::NumericVector &thetas){
   
Rcpp::IntegerVector xpoint;
int now;
   
// Iterate over all parameters and functions of parameters
   for(int igam = 1; igam <= genx03::g_ngame; igam++){
      
       // Skip if the 'parameter' is one that is computed as a direct function
       // of a regular distribution parameter (nterg=0) because we just got it
          if(nterg.at(igam - 1) == 0) continue;
           
       // Otherwise take relationship info below to get the values from exp vars
       // the observation number is not important here, so use the first observation
          SEXP x = genx20::ipxcg[igam - 1];
          Rcpp::IntegerVector xpoint(x);
       
          if(debug::kprint >= 5){
             
             Rcpp::Rcout << "\nPTGAME BEFORE RGAMI\n" << std::endl;
             Rcpp::Rcout << "igam = " << igam << std::endl;
             Rcpp::Rcout << "nxg.at(igam) = " << genx20::nxg.at(igam - 1) << std::endl;
             Rcpp::Rcout << "nterg = " << genx20::nterg << std::endl;
             Rcpp::Rcout << "intg.at(igam) = " << genx20::intg.at(igam - 1) << std::endl;
             Rcpp::Rcout << "xpoint = " << xpoint << std::endl;
             Rcpp::Rcout << "ipthet = " << genx21::ipthet << std::endl;
             Rcpp::Rcout << "igtyg = " << genx21::igtyg << std::endl;
             Rcpp::Rcout << "irelag = " << genx21::irelag << std::endl;
             Rcpp::Rcout << "thetas = " << thetas << std::endl;
             Rcpp::Rcout << "gamms = " << genx21::gamms << std::endl;
             
          }
 
          now = 1;
          rgami(now,igam,genx20::nxg.at(igam - 1),genx20::nterg.at(igam - 1),
                genx20::intg.at(igam - 1),xpoint,
                genx21::ipthet.at(igam - 1),genx21::igtyg.at(igam - 1),
                genx21::irelag.at(igam - 1),thetas,genx21::gamms);
         
   }
   
if(debug::kprint >= 6){
   
   Rcpp::Rcout << "\nPTGAME**6**\n" << std::endl;
   Rcpp::Rcout << "thetas = " << thetas << std::endl;
   Rcpp::Rcout << "gamms = " << genx21::gamms << std::endl;
   
}

return;

}