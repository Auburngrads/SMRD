#include <base/base.hpp>
#include <wqmsphiall/wqm_phiall.hpp>

//' Compute empirical the predictive distributions 
//' (pdf, cdf) of a single future observation from 
//' distribution kdist
//' 
//' @name postpr
//' 
//' @param xltime The time vector
//' @param ntvec The length of the tvector (at which evaluations will be done
//' @param xmu The mu* values from the posterior sample 
//' @param sigma The sigma* values from the posterior sample
//' @param nsim The value M*
// [[Rcpp::export]]
Rcpp::List POSTPR(Rcpp::NumericVector xltime,
                  int ntvec,
                  Rcpp::NumericVector xmu,
                  Rcpp::NumericVector sigma,
                  int nsim,
                  int kdist,
                  Rcpp::NumericVector pdf,
                  Rcpp::NumericVector cdf){

double phib = 0,phibm = 0,phis = 0,phip = 0;
double z, factx;
  
for(int i = 0; i < ntvec; i++){
  
    pdf.at(i) = zero;
    cdf.at(i) = zero;
    
    for(int j = 0; j < nsim; j++){
      
        z = (xltime.at(i) - xmu.at(j)) / sigma.at(j);
        factx = one / (sigma.at(j));
        
        if((kdist % 2) == 0) {
          
           factx = one / (std::exp(xltime.at(i)) * sigma.at(j));
          
        }
        
        wqm_phiall(phib,phibm,phis,phip,z,kdist);
        
        pdf.at(i) = pdf.at(i) + factx * phis;
        cdf.at(i) = cdf.at(i) + phib;
           
    }
    
      pdf.at(i) = pdf.at(i) / float(nsim);
      cdf.at(i) = cdf.at(i) / float(nsim);
}

      return Rcpp::List::create(Named("cdf") = cdf,
                                Named("pdf") = pdf);
  
}