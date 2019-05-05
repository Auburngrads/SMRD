#include <base/base.hpp>
#include <sfteval/gdeg.hpp>

using namespace passer;

//' Compute integrand for computing probability over the nonrectangular region

double fundeg(double beta1){

// get conditional mean and var of beta2 given beta1
   double zb1 = (beta1 - passer::g_xmu1p) / passer::g_sig1p;
   double xmu2pg = passer::g_xmu2p + passer::g_rhop * passer::g_sig2p * zb1;
   double sig2pg = passer::g_sig2p * passer::g_rootr;
   double zb2  = (gdeg(passer::g_kdmodp,passer::g_dfp,passer::g_d0p,passer::g_sfactp,passer::g_tfp,beta1) - xmu2pg) / sig2pg;
   double prob = R::pnorm(-zb2,0,1,true,false);

   return prob * R::dnorm(zb1,0,1,false) / passer::g_sig1p;

}
