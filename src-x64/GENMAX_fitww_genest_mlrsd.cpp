#include <base/base.hpp>
#include <genmax/mlrsd1.hpp>

using namespace genx03;

//' Compute residuals for ml fit

void mlrsd(Rcpp::NumericMatrix &ipy,
           int &ncoly,
           int &nrownw,
           Rcpp::NumericVector &thetas,
           int &kdist,
           Rcpp::NumericVector &resid,
           Rcpp::NumericVector &yhat){
   
// Grab space for the gamma vector
   Rcpp::NumericVector ipgame = Rcpp::NumericVector(genx03::g_ngame);
   
   mlrsd1(ipy,ncoly,nrownw,thetas,ipgame,kdist,resid,yhat);
   
return;

}

#include <base/base.hpp>
#include <genmax/ptgame.hpp>
#include <genmax/rgamme.hpp>
#include <genmax/gzhat.hpp>

//' Compute residuals for ml fit

void mlrsd1(Rcpp::NumericMatrix &y,
            int &ncoly,
            int &nrownw,
            Rcpp::NumericVector &thetas,
            Rcpp::NumericVector &gamme,
            int &kdist,
            Rcpp::NumericVector &resid,
            Rcpp::NumericVector &yhat){
   
ptgame(thetas);
   
for(int kpnow = 1; kpnow <= nrownw; kpnow++){
   
    rgamme(kpnow,thetas,gamme);
    yhat.at(kpnow - 1) = gamme.at(0);
    resid.at(kpnow - 1) = gzhat(y.at(kpnow - 1,0),gamme,kdist);
   
}

return;

}