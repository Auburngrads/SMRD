#include <base/base.hpp>
//#include <genmax/gtunsc.hpp>
#include <genmax/sclp.hpp>

using namespace genx20;
using namespace genx21;
using namespace genx03;
using namespace genx05;
using namespace genx07;

void scpx(Rcpp::NumericVector &theta,
          Rcpp::NumericVector &thetas){

 int ipoint;
 Rcpp::IntegerVector ipoinx;
 
for(int igame = 1; igame <= genx03::g_ngame; igame++){
  
// Pick up pointer to theta vector for the current gamma parameter
   ipoint = genx21::ipthet.at(igame - 1);
  
// Pick up pointer to the columns of x used in the explanatory
// variable relationship for the current gamma parameter
//   ipoinx = as<IntegerVector>(ipxcg)[igame];
   
// Check to see if we are dealing with a trivial
// parameter that needs no scaling:

// nterg(igame) = 0 indicates that this is  
// a saved function of another parmeter
   if(genx20::nterg.at(igame - 1) ==  0) goto line21;

//  nxg(igame) = 0   indicates no regression--copy single
    if(genx20::nxg.at(igame - 1) != 0) {

       sclp(ipoint,igame,theta,genx20::nxg.at(igame - 1),
            genx20::intg.at(igame - 1),genx20::ipxcg,
            genx05::g_ipxbru,genx05::g_ipsd,thetas,
            genx05::g_ipiscd);

      goto line21;

    }
    
    thetas.at(ipoint - 1) = theta.at(ipoint - 1);

line21:    
  
if(debug::kprint >= 4) {
  
   Rcpp::Rcout << "\nscpx**4**\n"                             << std::endl;
   Rcpp::Rcout << "igame = "    << igame - 1                << std::endl;
   Rcpp::Rcout << "ipthet = "   << genx21::ipthet.at(igame - 1) << std::endl;
   Rcpp::Rcout << "intg = "     << genx20::intg.at(igame - 1)   << std::endl;
   Rcpp::Rcout << "nxg = "      << genx20::nxg.at(igame - 1)    << std::endl;
   Rcpp::Rcout << "nterg = "    << genx20::nterg.at(igame - 1)  << std::endl;

   if(genx20::ipxcg[igame - 1] != R_NilValue) {
      
      SEXP l = genx20::ipxcg[igame - 1]; Rcpp::IntegerVector y(l);
      Rcpp::Rcout << "ipxcg(igame) = " << y << std::endl;
     
   }

}
}

if(debug::kprint >= 4) {
  
   for(int i = 1; i <= genx07::g_nparm; i++){
     
       Rcpp::Rcout << "\nscpx**4**\n"  << std::endl;
       Rcpp::Rcout << "i = " << i - 1  << std::endl;
       Rcpp::Rcout << "theta(i) = "    << theta.at(i - 1) << std::endl;
       Rcpp::Rcout << "thetas(i) = "   << thetas.at(i - 1) << std::endl;

     
   }
  
}

      return;

}
