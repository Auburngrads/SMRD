#include <base/base.hpp>

// Scale subset of theta for a particular relationship

void sclp(int &ipoint,
          int &igame,
          Rcpp::NumericVector &theta,
          int &nx,
          int &Int,
          Rcpp::List &ipxcg,
          Rcpp::NumericVector &xbar,
          Rcpp::NumericVector &sd,
          Rcpp::NumericVector &thetas,
          Rcpp::IntegerVector &iscd){

      int nter = nx + Int, jj;
      int nstart = Int + 1;
      double Const = 0.0e00, xbaru;
      SEXP ll = ipxcg[igame - 1];
      Rcpp::IntegerVector icolx(ll);
      
      for(int j = nstart; j <= nter; j++){
        
         jj = icolx.at(j - 1) + 1;
         xbaru = xbar.at(jj - 1);
         
         if(iscd.at(j - 1) == 0) xbaru = 0.0;
         
         Const = Const + theta.at((j - 1) + (ipoint - 1)) * xbaru;
         thetas.at((j - 1) + (ipoint - 1)) = theta.at((j - 1) + (ipoint - 1)) * sd.at(jj - 1);
         
         if(debug::kprint >= 5) {
           
           Rcpp::Rcout << "\nsclp**5**\n" << std::endl;
           Rcpp::Rcout << "j = " << j - 1 << std::endl;
           Rcpp::Rcout << "icolx(j) = " << icolx.at(j - 1) << std::endl;
           Rcpp::Rcout << "iscd(j) = " << iscd.at(j - 1) <<  std::endl;
           Rcpp::Rcout << "Int = " << Int << std::endl;
           Rcpp::Rcout << "nter = " <<  nter << std::endl;
           Rcpp::Rcout << "nstart = " <<  nstart << std::endl;
           Rcpp::Rcout << "theta = " <<  theta << std::endl;
           Rcpp::Rcout << "thetas = " << thetas <<  std::endl;
           Rcpp::Rcout << "xbar(j) = " << xbar.at(j - 1) <<  std::endl;
           Rcpp::Rcout << "xbaru = " << xbaru <<  std::endl;
           Rcpp::Rcout << "sd(j) = " << sd.at(j - 1) <<  std::endl;
           Rcpp::Rcout << "Const = " << Const <<  std::endl;
           Rcpp::Rcout << "ipoint = " << ipoint <<  std::endl;
           
         }
        
}
      
// This should keep from changing theta when centered for ph
   if(Int == 1) thetas.at(0 + (ipoint - 1)) = theta.at(0 + (ipoint - 1)) + Const;
   if((debug::kprint < 4) or (Int == 0)) return;
   
   xbaru = 1.0;
   
if(debug::kprint >= 5) {
 
   Rcpp::Rcout << "\nsclp**5**\n" << std::endl;
   Rcpp::Rcout << "icolx(0) = " << icolx.at(0) << std::endl;
   Rcpp::Rcout << "iscd(0) = " << iscd.at(0) <<  std::endl;
   Rcpp::Rcout << "Int = " << Int << std::endl;
   Rcpp::Rcout << "nter = " <<  nter << std::endl;
   Rcpp::Rcout << "nstart = " <<  nstart << std::endl;
   Rcpp::Rcout << "theta = " <<  theta << std::endl;
   Rcpp::Rcout << "thetas = " << thetas <<  std::endl;
   Rcpp::Rcout << "xbar(0) = " << xbar.at(0) <<  std::endl;
   Rcpp::Rcout << "xbaru = " << xbaru <<  std::endl;
   Rcpp::Rcout << "sd(0) = " << sd.at(0) <<  std::endl;
   Rcpp::Rcout << "Const = " << Const <<  std::endl;
   
}

return;

}
