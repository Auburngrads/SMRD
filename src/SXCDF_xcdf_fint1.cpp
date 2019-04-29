#include <base/base.hpp>
#include <sxcdf/rflpdf.hpp>
#include <sxcdf/rflcdf.hpp>

using namespace passer1;
using namespace passer2;

//' Function to compute integrand for computing probability involving the compound distribution
//'
//' @detail The following variables are communicated through namespace \code{passer1}
//' 
//' \document{
//'   \item{beta0p}{intercept of the mean log(time)}
//'   \item{beta1p}{slope of the mean log(time)}
//'   \item{xlogp}{log(stress level)}
//'   \item{sigmap}{std devn of log(time)}
//'   \item{ugammap}{mean of fatigue limit}
//'   \item{sgammap}{std devn of fatigue limit}
//'   \item{wp}{log(time)}
//'   \item{xlogtp}{log of time for which pr(T<tp) computed}
//'   }

double fint1(double x){

double y,z,fint_1;
  
y = (x - passer1::g_ugammap) / passer1::g_sgammap;
z = (passer1::g_wp - passer1::g_beta0p - passer1::g_beta1p * std::log(std::exp(passer1::g_xlogp) - std::exp(x))) / passer1::g_sigmap;

fint_1 = rflcdf(z,passer2::g_ndist1p) * rflpdf(y,passer2::g_ndist2p);

return fint_1;
  
}
