#include <base/base.hpp>

//' Check convergence in \code{thetas} 
//' from old values in \code{thtmp}

void ccheck(Rcpp::NumericVector &thetas,
            Rcpp::NumericVector &thtmp,
            int &nter,
            double &tol,
            int &lrel,
            int &lconv){

if(debug::kprint >= 5) Rcpp::Rcout << "\nCCHECK**5**\n" << std::endl;

double diff;

for(int i = 1; i <= nter; i++){
  
    diff = std::fabs(thetas.at(i - 1) - thtmp.at(i - 1));
  
    if(debug::kprint >= 5) {
      
       double reldiff = (diff / std::max(std::fabs(thtmp.at(i - 1)),tol));
       Rcpp::Rcout << "i = "         << i - 1           << std::endl;
       Rcpp::Rcout << "thetas(i) = " << thetas.at(i - 1) << std::endl;
       Rcpp::Rcout << "thtmp(i) = "  << thtmp.at(i - 1)  << std::endl;
       Rcpp::Rcout << "diff = "      << diff         << std::endl;
       Rcpp::Rcout << "reldiff = "   << reldiff      << std::endl;
       Rcpp::Rcout << "lrel = "      << lrel         << std::endl;
       Rcpp::Rcout << "lconv = "     << lconv        << std::endl;
       
    }
       
    if(lrel != 0) {
      
       // Relative difference
          if((diff / std::max(std::fabs(thtmp.at(i - 1)),tol)) < tol) continue;
          lconv = 0;
          return;
    
    }
    
    // Absolute difference
       if(diff < tol) continue;
       lconv = 0;
       return;
}

return;

}
