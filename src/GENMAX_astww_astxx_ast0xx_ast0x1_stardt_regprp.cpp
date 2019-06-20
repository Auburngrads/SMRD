#include <base/base.hpp>
#include <utility/wqm_filld.hpp>
#include <genmax/fxpi.hpp>

// Put in estimate or set regression dummys for the parameter

void regprp(double &est,
            int &ipard,
            Rcpp::NumericVector &thetas,
            Rcpp::IntegerVector &intd,
            Rcpp::IntegerVector &nxd,
            int &kodeti){

int index = 1,ier = 0;
double estp;

if(ipard != 1) {
  
   int ipardm = ipard - 1;
  
   // #get the starting index in thetas for current parameter;
   for(int i = 1; i <= ipardm; i++){
     
       index = index + intd.at(i - 1) + nxd.at(i - 1);
     
   }

}
// Check for no regression
   estp = fxpi(est,kodeti,ier);

   if(nxd.at(ipard - 1) == 0) goto line50;
   if(intd.at(ipard - 1) == 0) goto line45;

// Fill intercept with trans center estimate and others with zero
   thetas.at(index - 1) = estp;
   wqm_filld(0.0e00,thetas,index + 1,nxd.at(ipard - 1));
   goto line199;

// No intercept--fill all with ttrans estimate
   line45: for(int i = 0; i < nxd.at(ipard - 1); i++){
      
               thetas.at(index + i - 1) = estp;
      
           }
   
           goto line199;

line50: thetas.at(index - 1) = est;

line199: if(debug::kprint >= 3){
   
            Rcpp::Rcout << "\nREGPRP**3**\n" << std::endl;
            Rcpp::Rcout << "ipard = " << ipard - 1 << std::endl;
            Rcpp::Rcout << "kodeti = " << kodeti << std::endl;
            Rcpp::Rcout << "intd(ipard) = " << intd.at(ipard - 1) << std::endl;
            Rcpp::Rcout << "nxd(ipard) = "  << nxd.at(ipard - 1) << std::endl;
            Rcpp::Rcout << "index = " << index - 1 << std::endl;
            Rcpp::Rcout << "est = " << est << std::endl;
            Rcpp::Rcout << "estp = " << estp << std::endl;
            
         }

return;

}
