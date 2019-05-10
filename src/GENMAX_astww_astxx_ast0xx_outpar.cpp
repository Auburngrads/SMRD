#include <base/base.hpp>
#include <genmax/corrg.hpp>
#include <genmax/pargen.hpp>

//' Print summary of ml estimation

void outpar(int ifit,
            int kmod,
            int kdist,
            double xlike,
            Rcpp::NumericVector theta,
            Rcpp::IntegerVector kodet,
            Rcpp::IntegerVector ilabp,
            Rcpp::NumericMatrix vcv,
            Rcpp::NumericMatrix r,
            int nparm,
            double conlev,
            int idim){
  
if(kdist == 0) goto line500;
  
Rcpp::Rcout << "\nSummary of maximum likelihood estimation\n"  << std::endl;
Rcpp::Rcout << "kdist = " << kdist << std::endl;
Rcpp::Rcout << "kmod = " << kmod << std::endl;
Rcpp::Rcout << "xlike = " << xlike << std::endl;
goto line501;
  
line500: Rcpp::Rcout << "\nSummary of least squares estimation\n" << std::endl;
line501: pargen(theta,kodet,nparm,ilabp,conlev,vcv,idim);

if(ifit <= 1) return;

Rcpp::Rcout << "\nEstimated variance-covariance matrix\n" << vcv << std::endl;

// Compute and print correlation matrix
   corrg(vcv,r,nparm);

Rcpp::Rcout << "\nEstimated correlation matrix\n" << r << std::endl;

return;
      
}
