#include <base/base.hpp>
#include <gensiz/distxx.hpp>

using namespace fdnprd_g;

//'  Return npard (number of distribution parameters
//'  as a function of kdist and kmod). call just before
//'  fit to get sizes.  If interactive, can call after
//'  kdist and kmod to find npard and parameter names etc.
//'
//'  If we do not know kmod, we can use 0 to pick up ilabd.
//'  If kgtall=1, we can assume that intd and nxd are full
//'  and that nparm is to be computed as well

void fdnprd(int &kmod,
            int &kdist,
            Rcpp::IntegerVector &intd,
            Rcpp::IntegerVector &nxd,
            int &nregr,
            int &npard,
            int &kgtall,
            int &nparm,
            Rcpp::IntegerVector &ilabp,
            Rcpp::IntegerVector &ilabd,
            int &llog,
            int &kmodp,
            int &ier,
            int &maxpd){

fdnprd_g::ipgty  = IntegerVector(maxpd,0);
fdnprd_g::ipmark = IntegerVector(maxpd,0);

distxx(kmod,kdist,llog,maxpd,fdnprd_g::ipgty,
       fdnprd_g::ipmark,npard,kmodp,ier);

if(ier > 0) return; // goto exit;

if(kgtall == 1){
  
   nparm = 0;
   nregr = 0;
   
   for(int i = 0; i < npard; i++){
     
      if(nxd.at(i) >= 1) nregr = nregr + 1;
      
      nparm = nparm + intd.at(i) + nxd.at(i);
        
   }
        
}

   return;

}
