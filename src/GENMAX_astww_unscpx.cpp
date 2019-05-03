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
   
if(debug::kprint >= 6){
   
   Rcpp::Rcout << "\nUNSCPX**6**\n" << std::endl;
   
} 

for(int igame = 1; igame <= genx03::g_ngame; igame++){
   
    // Get pointer to current theta vector
       ipoint = genx21::ipthet.at(igame - 1);
   
    // Check to see if we are dealing with a trivial parameter that needs no scaling:
    // nterg(igame)=0 indicates that this is a saved function of another parmeter
       if(genx20::nterg.at(igame - 1) !=  0) {
       
          // nxg(igame)=0   indicates no regression--copy single
             if(genx20::nxg.at(igame - 1) != 0) {
             
                // Get pointer to the current x columns
                   SEXP l = genx20::ipxcg[igame - 1]; Rcpp::IntegerVector ipoinx(l);
                   
                   if(debug::kprint >= 3){
                      
                      Rcpp::Rcout << "\nUNSCPX ipoinx = " << ipoinx << std::endl;
                      
                   }
                   
                   Rcpp::IntegerVector NXG = clone(genx20::nxg);
                   Rcpp::IntegerVector INTG = clone(genx20::intg);
                   Rcpp::NumericVector IPXBRU = clone(genx05::g_ipxbru);
                   Rcpp::NumericVector IPSD = clone(genx05::g_ipsd);
                   Rcpp::IntegerVector IPISCD = clone(genx05::g_ipiscd);
                   
                   unsclp(ipoint,thetas,NXG.at(igame - 1),
                          INTG.at(igame - 1),ipoinx,
                          IPXBRU,IPSD,theta,IPISCD);
                   
                   genx20::nxg = clone(NXG);
                   genx20::intg = clone(INTG);
                   genx05::g_ipxbru = clone(IPXBRU);
                   genx05::g_ipsd = clone(IPSD);
                   genx05::g_ipiscd = clone(IPISCD);

                   
                   goto line21;
            
             }
             
            theta.at(ipoint - 1) = thetas.at(ipoint - 1);
      
       }
       
      line21: if(debug::kprint >= 6){
         
                 Rcpp::Rcout << "\nigame = " << igame - 1 << std::endl;
                 Rcpp::Rcout << "ipthet(igame) = " << ipthet.at(igame - 1) << std::endl;
                 Rcpp::Rcout << "intg(igame) = " << intg.at(igame - 1) << std::endl;
                 Rcpp::Rcout << "nxg(igame) = " << nxg.at(igame - 1) << std::endl;
                 Rcpp::Rcout << "nterg(igame) = " << nterg.at(igame - 1) << std::endl;
                 
      }
      
}

if(debug::kprint >= 6){
   
   Rcpp::Rcout << "\nUNSCPX**end**\n"   << std::endl;
   Rcpp::Rcout << "theta = "  << theta  << std::endl;
   Rcpp::Rcout << "thetas = " << thetas << std::endl;
   
}

return;
      
}