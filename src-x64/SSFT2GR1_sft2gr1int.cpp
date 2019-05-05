#include <base/base.hpp>
#include <utility/wqm_dxerc.hpp>

using namespace passersft2gr1;


//' Function to compute integrand for computing the sf of t2 given r1
//'
//' @details The following variables are communicated through the 
//'          global namespace \code{passersft2gr1}
//'
//' \document{
//'   \item{tp}{time point for evaluation of the conditional sf}
//'   \item{mut2p}{mean of log(t2) when r2=1}
//'   \item{sigmat2p}{standard deviation of log(t2) when r2=1}
//'   \item{mur2gr1p}{mean of log(r2) given r1}
//'   \item{sigmar2gr1p}{standard deviation of log(r2) given r1}
//'   }

double sft2gr1int(double r2log){
  
double mut2gr2, sft2gr1_int, ztigr2, zr2gr1;

// Get the conditional mean of log(t2) given r2
   mut2gr2 = passersft2gr1::g_mut2p + r2log;
   ztigr2 = (passersft2gr1::g_tlogp - mut2gr2) / passersft2gr1::g_sigmat2p;
   zr2gr1 = (r2log - passersft2gr1::g_mur2gr1p) / passersft2gr1::g_sigmar2gr1p;
   sft2gr1_int = half * wqm_dxerc(ztigr2 * root) * R::dnorm(zr2gr1,0,1,false) / passersft2gr1::g_sigmar2gr1p;
  
return sft2gr1_int;
  
}
