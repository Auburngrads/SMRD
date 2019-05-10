#include <base/base.hpp>
#include <wqmmlesss/wqm_phibf.hpp>

//' Compute the information for the prediction/risk analysis
//'  centim(ngroup)       censor time of cohort
//'  nsamsz(ngroup)       sample size of cohort
//'  prdelt               length of future time interval
//'  krfail(ngroup)       number of observed failures in cohort
void prisk(int &kdist,
           double &xmu,
           double &sigma,
           Rcpp::IntegerVector &nsamsz,
           Rcpp::NumericVector &centim,
           int &ngroup,
           double &prdelt,
           Rcpp::NumericVector &rhostr,
           Rcpp::IntegerVector &krfail,
           Rcpp::IntegerVector &nmrvec,
           double &crisk){
  
double zpstar,zcstar,plttpd,pltct,htime,grisk;
  
// Go over groups to get the future failure info for calibration
   if(debug::kprint >= 1){
     
      Rcpp::Rcout << "\nPRISK**1**\n" << std::endl;
      Rcpp::Rcout << "xmu = " << xmu << std::endl;
      Rcpp::Rcout << "sigma = " << sigma << std::endl;
      Rcpp::Rcout << "prdelt = " << prdelt << std::endl;
     
   }
   
   crisk = 0.0;
   
   for(int igroup = 1; igroup <= ngroup; igroup++){
     
       zpstar = (std::log(centim.at(igroup - 1) + prdelt) - xmu) / sigma;
       zcstar = (std::log(centim.at(igroup - 1)) - xmu) / sigma;
       plttpd = wqm_phibf(zpstar, kdist);
       pltct =  wqm_phibf(zcstar, kdist);
       
       // Compute the conditional binomial probability
          rhostr.at(igroup - 1) = (plttpd - pltct) / (1.0e00 - pltct);
       
       // Subtract out the number failing to get
       // the number at risk in the delta interval
          nmrvec.at(igroup - 1) = nsamsz.at(igroup - 1) - krfail.at(igroup - 1);
          htime = centim.at(igroup - 1) + prdelt;
          grisk = rhostr.at(igroup - 1) * nmrvec.at(igroup - 1);
          crisk = crisk + grisk;
       
       // Print the risk table
          if(debug::kprint >= 1){
            
             Rcpp::Rcout << "\nPRISK**2**\n" << std::endl;
             Rcpp::Rcout << "igroup = "         << igroup - 1            << std::endl;
             Rcpp::Rcout << "nsamsz(igroup) = " << nsamsz.at(igroup - 1) << std::endl;
             Rcpp::Rcout << "nmrvec(igroup) = " << nmrvec.at(igroup - 1) << std::endl;
             Rcpp::Rcout << "centim(igroup) = " << centim.at(igroup - 1) << std::endl;
             Rcpp::Rcout << "htime = "          << htime                 << std::endl;
             Rcpp::Rcout << "pltct = "          << pltct                 << std::endl;
             Rcpp::Rcout << "plttpd = "         << plttpd                << std::endl;
             Rcpp::Rcout << "rhostr(igroup) = " << rhostr.at(igroup - 1) << std::endl;
             Rcpp::Rcout << "grisk = "          << grisk                 << std::endl;
             Rcpp::Rcout << "crisk = "          << crisk                 << std::endl;
            
          }
              
   }
   
return;

}