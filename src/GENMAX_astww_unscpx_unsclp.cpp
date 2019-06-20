#include <base/base.hpp>

// Unscale subset of theta for a particulat relationship

void unsclp(int &point,
            Rcpp::NumericVector &thetas,
            int &nx,
            int &intcpt,
            Rcpp::IntegerVector &icolx,
            Rcpp::NumericVector &xbar,
            Rcpp::NumericVector &sd,
            Rcpp::NumericVector &theta,
            Rcpp::IntegerVector &iscd){
   
double Const,xbaru;
int nter,nstart,jj,j;

nter = nx + intcpt;
nstart = intcpt + 1;

Const = 0.0e00;

for(int j = nstart; j <= nter; j++){
   
    jj = icolx.at(j - 1) + 1;
    theta.at((point - 1) + j - 1) = thetas.at((point - 1) + j - 1) / sd.at(jj - 1);
    xbaru = xbar.at(jj - 1);
    if(iscd.at(j - 1) == 0) xbaru = 0.0;
    
    // Check this
       Const = Const + theta.at((point - 1) + j - 1) * xbaru;
       
       if(debug::kprint >= 7) {
          
          Rcpp::Rcout << "\nUNSCLP**7**\n" << std::endl;
          Rcpp::Rcout << "j = " << j - 1 << std::endl;
          Rcpp::Rcout << "icolx(j) = " << icolx.at(j - 1) << std::endl;
          Rcpp::Rcout << "iscd(j) = " << iscd.at(j - 1) << std::endl;
          Rcpp::Rcout << "intcpt = " << intcpt << std::endl;
          Rcpp::Rcout << "nter = " << nter << std::endl;
          Rcpp::Rcout << "nstart = " << nstart << std::endl;
          Rcpp::Rcout << "theta(j) = " << theta.at((point - 1) + j - 1) << std::endl;
          Rcpp::Rcout << "thetas(j) = " << theta.at((point - 1) + j - 1) << std::endl;
          Rcpp::Rcout << "xbar(j) = " << xbar.at(j - 1) << std::endl;
          Rcpp::Rcout << "xbaru = " << xbaru << std::endl;
          Rcpp::Rcout << "sd(j) = " << sd.at(j - 1) << std::endl;
          Rcpp::Rcout << "point = " << point       << std::endl;
          Rcpp::Rcout << "Const = " << Const << std::endl;
          
       }
      
}

if(intcpt == 1) theta.at((point - 1) + 0) = thetas.at((point - 1) + 0) - Const;
if((debug::kprint < 4) or (intcpt == 0)) return;
xbaru = 1.0;
j = 1;

// Now print for intercept if there is one in the model
   if(debug::kprint >= 7) {
          
      Rcpp::Rcout << "\nUNSCLP**end**\n" << std::endl;
      Rcpp::Rcout << "j = " << j - 1 << std::endl;
      Rcpp::Rcout << "icolx(j) = " << icolx.at(j - 1) << std::endl;
      Rcpp::Rcout << "iscd(j) = " << iscd.at(j - 1) << std::endl;
      Rcpp::Rcout << "intcpt = " << intcpt << std::endl;
      Rcpp::Rcout << "nter = " << nter << std::endl;
      Rcpp::Rcout << "nstart = " << nstart << std::endl;
      Rcpp::Rcout << "theta(j) = " << theta.at((point - 1) + j - 1) << std::endl;
      Rcpp::Rcout << "thetas(j) = " << theta.at((point - 1) + j - 1) << std::endl;
      Rcpp::Rcout << "xbar(j) = " << xbar.at(j - 1) << std::endl;
      Rcpp::Rcout << "xbaru = " << xbaru << std::endl;
      Rcpp::Rcout << "sd(j) = " << sd.at(j - 1) << std::endl;
      Rcpp::Rcout << "Const = " << Const << std::endl;
          
       }

return;

}
