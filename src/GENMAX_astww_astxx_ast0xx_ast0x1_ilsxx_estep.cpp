#include <base/base.hpp>
#include <utility/icheck.hpp>
#include <wqmsphiall/wqm_phiall.hpp>

//' Do the e-step of expectation-maximization 
//' according to the Schmee-Hahn ils algorithm.

void estep(int &nter,
           int &nparm,
           Rcpp::NumericMatrix &y,
           double &sigma,
           Rcpp::NumericMatrix &times,
           Rcpp::NumericMatrix &yhat,
           Rcpp::IntegerVector &codes,
           int &ncoly,
           int &nrownw,
           int &ier){
  
int kdist = 3;
double xmu = 0.0e00, factor = 10.0e10, diff = 0.0e00;
double zz    = 0.0e00, zz2    = 0.0e00;
double phib  = 0.0e00, phib2  = 0.0e00;
double phibm = 0.0e00, phibm2 = 0.0e00;
double phis  = 0.0e00, phis2  = 0.0e00;
double phip  = 0.0e00, phip2  = 0.0e00;

if(debug::kprint >= 5) Rcpp::Rcout << "\nESTEP**5**\n" << std::endl;
  
for(int i = 1; i <= nrownw; i++){
  
    icheck(codes.at(i - 1),0,5,0,0,ier,-7622);
    
    if((codes.at(i - 1) == 5)) continue;
    
    if((codes.at(i - 1) == 2)) { // Right censored observation
      
        xmu = yhat.at(i - 1,0);
      
        zz  = (y.at(i - 1,0) - xmu) / sigma;
        wqm_phiall(phib,phibm,phis,phip,zz,kdist);
        
        if(phibm > zero) factor = phis / phibm;
        
        times.at(i - 1,0) = xmu + sigma * factor;
       
    }
    if((codes.at(i - 1) == 3)) { // Left censored observation
      
        xmu = yhat.at(i - 1,0);
      
        zz = (y(i - 1,0) - xmu) / sigma;
        wqm_phiall(phib,phibm,phis,phip,zz,kdist);
        
        if(phib > zero) factor = phis / phib;
        
        times.at(i - 1,0) = xmu - sigma * factor;
    
    }
    if((codes.at(i - 1) == 4)) { // Interval censored observation
    
        xmu = yhat.at(i - 1,0);
      
        zz = (y.at(i - 1,0) - xmu) / sigma;
        wqm_phiall(phib,phibm,phis,phip,zz,kdist);
        
        zz2 = (y.at(i - 1,1) - xmu) / sigma;
        wqm_phiall(phib2,phibm2,phis2,phip2,zz2,kdist);
        
        diff = phib2 - phib;
        
        if(diff > zero) {
          
           times.at(i - 1,0) = xmu + sigma * ((phis - phis2) / (phib2 - phib));
          
        } else {
          
           times.at(i - 1,0) = (y.at(i - 1,0) + y.at(i - 1,1)) / 2.0;
           Rcpp::stop("\nDiff error in estep -- Diff = %f", times.at(i - 1,0));
        
        }
       
    }

    if(debug::kprint >= 5) {
      
       Rcpp::NumericVector yrowi = y.row(i - 1); 
       Rcpp::Rcout << "i = "        << i - 1         << std::endl;
       Rcpp::Rcout << "codes(i) = " << codes.at(i - 1)   << std::endl;
       Rcpp::Rcout << "phibm = "    << phibm         << std::endl;
       Rcpp::Rcout << "phib = "     << phib          << std::endl;
       Rcpp::Rcout << "phis = "     << phis          << std::endl;
       Rcpp::Rcout << "xmu = "      << xmu           << std::endl;
       Rcpp::Rcout << "sigma = "    << sigma         << std::endl;
       Rcpp::Rcout << "y(i,) = "    << yrowi         << std::endl;
       Rcpp::Rcout << "times(i) = " << times.at(i - 1,0) << std::endl;

    }
    
}
    
return;

}
