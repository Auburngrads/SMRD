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
 
 if(debug::kprint >= 4) {
    
    Rcpp::Rcout << "\nSCPX START\n"                << std::endl;
    Rcpp::Rcout << "ipthet = " << genx21::g_ipthet << std::endl;
    Rcpp::Rcout << " ngame = " << genx03::g_ngame  << std::endl;
    Rcpp::Rcout << " nterg = " << genx20::g_nterg  << std::endl;
    Rcpp::Rcout << "   nxg = " << genx20::g_nxg    << std::endl;
    Rcpp::Rcout << "  intg = " << genx20::g_intg   << std::endl;
    Rcpp::Rcout << "  ipsd = " << genx05::g_ipsd   << std::endl;
    Rcpp::Rcout << "ipiscd = " << genx05::g_ipiscd << std::endl;
    Rcpp::Rcout << "ipxbru = " << genx05::g_ipxbru << std::endl;
    Rcpp::Rcout << " nparm = " << genx07::g_nparm  << std::endl;
    Rcpp::Rcout << "thetas = " << thetas           << std::endl;
    Rcpp::Rcout << " theta = " << theta            << std::endl;
    
 }
 
Rcpp::IntegerVector NXG = clone(genx20::g_nxg);
Rcpp::IntegerVector INTG = clone(genx20::g_intg);
Rcpp::List IPXCG = clone(genx20::g_ipxcg);
Rcpp::NumericVector IPXBRU = clone(genx05::g_ipxbru);
Rcpp::NumericVector IPSD = clone(genx05::g_ipsd);
Rcpp::IntegerVector IPISCD = clone(genx05::g_ipiscd);
Rcpp::IntegerVector IPTHET = clone(genx21::g_ipthet);
Rcpp::IntegerVector NTERG = clone(genx20::g_nterg);
 
for(int igame = 1; igame <= genx03::g_ngame; igame++){
  
// Pick up pointer to theta vector for the current gamma parameter
   ipoint = IPTHET.at(igame - 1);
  
// Pick up pointer to the columns of x used in the explanatory
// variable relationship for the current gamma parameter
//   ipoinx = as<IntegerVector>(ipxcg)[igame];
   
// Check to see if we are dealing with a trivial
// parameter that needs no scaling:

// nterg(igame) = 0 indicates that this is  
// a saved function of another parmeter
   if(NTERG.at(igame - 1) ==  0) goto line21;

//  nxg(igame) = 0   indicates no regression--copy single
    if(NXG.at(igame - 1) != 0) {

       if(debug::kprint >= 10) {
          
          Rcpp::Rcout << "\nENTERING SCLP\n" << std::endl;
          
       }
       
       sclp(ipoint,igame,theta,NXG.at(igame - 1),
            INTG.at(igame - 1),IPXCG,IPXBRU,
            IPSD,thetas,IPISCD);
       
      goto line21;

    }
    
    thetas.at(ipoint - 1) = theta.at(ipoint - 1);

line21: if(debug::kprint >= 4) {
  
           Rcpp::Rcout << "\nscpx**4**\n"                     << std::endl;
           Rcpp::Rcout << " igame = " << igame - 1            << std::endl;
           Rcpp::Rcout << "ipthet = " << IPTHET.at(igame - 1) << std::endl;
           Rcpp::Rcout << "  intg = " << INTG.at(igame - 1)   << std::endl;
           Rcpp::Rcout << "   nxg = " << NXG.at(igame - 1)    << std::endl;
           Rcpp::Rcout << " nterg = " << NTERG.at(igame - 1)  << std::endl;
           Rcpp::Rcout << " theta = " << theta                << std::endl;
           Rcpp::Rcout << "thetas = " << thetas               << std::endl;

           if(IPXCG[igame - 1] != R_NilValue) {

              SEXP l = IPXCG[igame - 1]; Rcpp::IntegerVector y(l);
              Rcpp::Rcout << "ipxcg(igame) = " << y << std::endl;

           }

        }
}

genx20::g_nxg = clone(NXG);
genx20::g_intg = clone(INTG);
genx20::g_ipxcg = clone(IPXCG);
genx05::g_ipxbru = clone(IPXBRU);
genx05::g_ipsd = clone(IPSD);
genx05::g_ipiscd = clone(IPISCD);
genx21::g_ipthet = clone(IPTHET);
genx20::g_nterg = clone(NTERG);

if(debug::kprint >= 4) {
  
   for(int i = 1; i <= genx07::g_nparm; i++){
     
       Rcpp::Rcout << "\nscpx**END**\n"                  << std::endl;
       Rcpp::Rcout << "        i = " << i - 1            << std::endl;
       Rcpp::Rcout << " theta(i) = " << theta.at(i - 1)  << std::endl;
       Rcpp::Rcout << "thetas(i) = " << thetas.at(i - 1) << std::endl;
     
   }
  
}

return;

}
