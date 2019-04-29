#include <base/base.hpp>
#include <genmax/vecexp.hpp>
#include <aplan/unsca.hpp>

using namespace genx03;
using namespace genx07;

//' Take the vector that poweld sees and get back to and print
//' all of the parameters in the user's model
//' note that fixed varaibles are in the upper portion of thetam
//' so that the dimension is nparm and not nparmm.

void powprn(int &iterc,
            int &nfcc,
            double &fmval,
            Rcpp::NumericVector &thetam,
            int &nparmm){
  
// Get pointer to scratch for printing
   Rcpp::NumericVector ist  = Rcpp::NumericVector(genx07::g_nparm);
   Rcpp::NumericVector istt = Rcpp::NumericVector(genx07::g_nparm);

// First expand the vector, bringing back fixed values
   vecexp(thetam,genx03::g_ipkode,genx07::g_nparm,istt,nparmm);
   unsca(istt,ist);
   
   if(debug::kprint >= 4){
     
      Rcpp::Rcout << "\nPOWELL LINE: 556 (POWPRN)\n" << std::endl;
      Rcpp::Rcout << "iterc = " << iterc << std::endl;
      Rcpp::Rcout << "nfcc = " << nfcc << std::endl;
      Rcpp::Rcout << "fmval = " << fmval << std::endl;
      Rcpp::Rcout << "x = " << ist << std::endl;
     
   }
        
return;
  
}