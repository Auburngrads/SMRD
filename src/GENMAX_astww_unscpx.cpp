#include <base/base.hpp>
#include <genmax/unsclp.hpp>

using namespace genx03;
using namespace genx20;
using namespace genx21;
// Get the entire unscaled theta vector

void unscpx(Rcpp::NumericVector &thetas,
            Rcpp::NumericVector &theta){
   
int ipoint;
Rcpp::IntegerVector ipoinx;
Rcpp::IntegerVector NXG = clone(genx20::g_nxg);
Rcpp::IntegerVector INTG = clone(genx20::g_intg);
Rcpp::NumericVector IPXBRU = clone(genx05::g_ipxbru);
Rcpp::NumericVector IPSD = clone(genx05::g_ipsd);
Rcpp::IntegerVector IPISCD = clone(genx05::g_ipiscd);
Rcpp::List IPXCG = clone(genx20::g_ipxcg);
Rcpp::IntegerVector IPTHET = clone(genx21::g_ipthet);
Rcpp::IntegerVector NTERG = clone(genx20::g_nterg);
   
if(debug::kprint >= 6){
   
   Rcpp::Rcout << "\nUNSCPX**6**\n" << std::endl;
   
} 

for(int igame = 1; igame <= genx03::g_ngame; igame++){
   
    // Get pointer to current theta vector
       ipoint = IPTHET.at(igame - 1);
   
    // Check to see if we are dealing with a trivial parameter that needs no scaling:
    // nterg(igame)=0 indicates that this is a saved function of another parmeter
       if(NTERG.at(igame - 1) !=  0) {
       
          // nxg(igame)=0   indicates no regression--copy single
             if(NXG.at(igame - 1) != 0) {
             
                // Get pointer to the current x columns
                   SEXP l = IPXCG[igame - 1]; Rcpp::IntegerVector ipoinx(l);
                   
                   if(debug::kprint >= 3){
                      
                      Rcpp::Rcout << "\nUNSCPX ipoinx = " << ipoinx << std::endl;
                      
                   }
                   
                   unsclp(ipoint,thetas,NXG.at(igame - 1),
                          INTG.at(igame - 1),ipoinx,
                          IPXBRU,IPSD,theta,IPISCD);
                   
                   goto line21;
            
             }
             
            theta.at(ipoint - 1) = thetas.at(ipoint - 1);
      
       }
       
      line21: if(debug::kprint >= 6){
         
                 Rcpp::Rcout << "\nigame = " << igame - 1 << std::endl;
                 Rcpp::Rcout << "ipthet(igame) = " << IPTHET.at(igame - 1) << std::endl;
                 Rcpp::Rcout << "intg(igame) = " << INTG.at(igame - 1) << std::endl;
                 Rcpp::Rcout << "nxg(igame) = " << NXG.at(igame - 1) << std::endl;
                 Rcpp::Rcout << "nterg(igame) = " << NTERG.at(igame - 1) << std::endl;
                 
      }
      
}

genx20::g_nxg = clone(NXG);
genx20::g_intg = clone(INTG);
genx05::g_ipxbru = clone(IPXBRU);
genx05::g_ipsd = clone(IPSD);
genx05::g_ipiscd = clone(IPISCD);
genx20::g_ipxcg = clone(IPXCG);
genx21::g_ipthet = clone(IPTHET);
genx20::g_nterg = clone(NTERG);

if(debug::kprint >= 6){
   
   Rcpp::Rcout << "\nUNSCPX**end**\n"   << std::endl;
   Rcpp::Rcout << "theta = "  << theta  << std::endl;
   Rcpp::Rcout << "thetas = " << thetas << std::endl;
   
}

return;
      
}