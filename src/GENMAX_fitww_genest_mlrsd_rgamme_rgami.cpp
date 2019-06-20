#include <base/base.hpp>
#include <utility/dlogc.hpp>
#include <utility/dexpc.hpp>
#include <utility/dcheck.hpp>
#include <utility/dsqrtc.hpp>
#include <genmax/dfbx.hpp>
#include <postkp/dlgama.hpp>
#include <utility/wqm_filld.hpp>

//' Pickup gamme as a function of thetas and x

void rgami(int &kpnow,
           int &igam,
           int &nxg,
           int &nterg,
           int &intg,
           Rcpp::IntegerVector &ipxcg,
           int &ipthta,
           int &igtype,
           int &irelag,
           Rcpp::NumericVector &thetas,
           Rcpp::NumericVector &gamme){
   
double tmp,exptmp,xk;
int ier = 0;
// igtype      parameter and functions
//-------------------------------------------------------------------
//   1    unrestricted  location          m
//   2    positive scale                  s  ls
//   3    0-1                             p  lp
//   4    unrestricted shape              q  k   sk  lk  gk
//
// irelag    type of relationship
//-------------------------------------------------------------------
//   0      constant
//   1      linear
//   100    user specified
//

if(debug::kprint >= 7){
   
   Rcpp::Rcout << "\nRGAMI**1**\n" << std::endl;
   Rcpp::Rcout << "kpnow = " << kpnow << std::endl;
   Rcpp::Rcout << "igam = " << igam - 1 << std::endl;
   Rcpp::Rcout << "nxg = " << nxg << std::endl;
   Rcpp::Rcout << "nterg = " << nterg << std::endl;
   Rcpp::Rcout << "intg = " << intg << std::endl;
   Rcpp::Rcout << "ipxcg = " << ipxcg << std::endl;
   Rcpp::Rcout << "ipthta = " << ipthta << std::endl;
   Rcpp::Rcout << "igtype = " << igtype << std::endl;
   Rcpp::Rcout << "irelag = " << irelag << std::endl;
   Rcpp::Rcout << "thetas = " << thetas << std::endl;
   Rcpp::Rcout << "gamme = " << gamme << std::endl;
   
}

// Get unrestricted location parameter
   if((igtype == 1) or (igtype == 5)){
      
       tmp = dfbx(kpnow,thetas,ipthta,ipxcg,nterg,intg,irelag);
      
       gamme.at(igam - 1) = tmp;
      
   }
   
// Get positive parameter and its logarithm
   if(igtype == 2){
      
      tmp = dfbx(kpnow,thetas,ipthta,ipxcg,nterg,intg,irelag);
      
      if(nxg == 0) tmp   = dlogc(tmp);
      gamme.at(igam - 1) = dexpc(tmp);
      gamme.at(igam)     = tmp;
      
   }
   
// Get 0-1 parameter and its logarithm
   if(igtype == 3){
      
      tmp = dfbx(kpnow,thetas,ipthta,ipxcg,nterg,intg,irelag);
      if(nxg > 0) goto line301;
      
      // With no regression we should have a prob coming down, check it
         dcheck(tmp,1.0e-25,one,1.0e-10,one,ier,2112);
         gamme.at(igam - 1) = tmp;
         goto line303;
         
      // For regression, we need to invert the logodds transformation
         line301: exptmp = dexpc(tmp);
         
      // Get probability for regression problems
         gamme.at(igam - 1) = exptmp / (one + exptmp);
         
      // Get log(prob)
         line303: gamme.at(igam) = dlogc(gamme.at(igam - 1));
      
   }

// Get unrestricted shape parameter
   if(igtype == 4){
      
      tmp = dfbx(kpnow,thetas,ipthta,
                 ipxcg,nterg,intg,irelag);
      
      // Fill gamme with default zeros in case shape=0 first time around
         for(int i = 0; i < 5; i++){
            
             gamme.at(igam + i - 1) = zero;
         }
         
         gamme.at(igam - 1) = tmp;
         
      // If shape paraqmeter is 0., xk=infinity so use asymptotic results
         if(tmp != zero){
            
            xk = one / (tmp * tmp);
            gamme.at(igam + 1 - 1) = xk;
            gamme.at(igam + 2 - 1) = dsqrtc(xk);
            gamme.at(igam + 3 - 1) = std::log(xk);
            gamme.at(igam + 4 - 1) = dlgama(xk);
            
         }
   
      
   }

if(debug::kprint >= 6){
   
   Rcpp::Rcout << "\nRGAMI**2**\n" << std::endl;
   Rcpp::Rcout << "kpnow = " << kpnow << std::endl;
   Rcpp::Rcout << "igam = " << igam << std::endl;
   Rcpp::Rcout << "nxg = " << nxg << std::endl;
   Rcpp::Rcout << "nterg = " << nterg << std::endl;
   Rcpp::Rcout << "intg = " << intg << std::endl;
   Rcpp::Rcout << "ipxcg = " << ipxcg << std::endl;
   Rcpp::Rcout << "ipthta = " << ipthta << std::endl;
   Rcpp::Rcout << "igtype = " << igtype << std::endl;
   Rcpp::Rcout << "irelag = " << irelag << std::endl;
   Rcpp::Rcout << "gamme(igam) = " << gamme.at(igam - 1) << std::endl;

}

return;

}