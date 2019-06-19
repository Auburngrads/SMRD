#include <base/base.hpp>
#include <genmax/ptgame.hpp>
#include <genfun/unfixp.hpp>

using namespace genx21;
using namespace genx03;
using namespace genx07;

//' Take input vector t, untransform to ts, call ptgame to put
//' paramters and functions in gamms (using ts), then store
//' the original gamms values at the end of ts

void gett(Rcpp::NumericVector &t,
          Rcpp::NumericVector &ts){
  
      unfixp(t,genx03::g_ipkode,genx07::g_nparm,ts);
      ptgame(ts);
  
// Set the gamma stuff in gamms and move to the end of ts for save
   for(int i = 1; i <= genx03::g_ngame; i++){
        
       ts.at(genx07::g_nparm + i - 1) = genx21::g_gamms.at(i - 1);
        
   }

   return;
   
}

#include <base/base.hpp>

using namespace genx21;
using namespace genx03;
using namespace genx07;

//' Restore values in xsave from the end of ts

void sett(Rcpp::NumericVector &ts){

     for(int i = 1; i <= genx03::g_ngame; i++){
        
         genx21::g_gamms.at(i - 1) = ts.at(genx07::g_nparm + i - 1);
        
     }
  
     return;
     
}
