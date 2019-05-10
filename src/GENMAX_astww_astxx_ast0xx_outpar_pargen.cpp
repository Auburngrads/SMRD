#include <base/base.hpp>
#include <genfun/intgen.hpp>

//' Print model parameter estimates, standard errors, and confidence limits
void pargen(Rcpp::NumericVector &theta,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            Rcpp::IntegerVector &ilabp,
            double &conlev,
            Rcpp::NumericMatrix &vcvs,
            int &idim){
  
double per = 100 * conlev;
double s = 0.0e00, xlow = 0.0e00,xu = 0.0e00;
  
Rcpp::Rcout << "\nmodel parameter" << per << "% confidence limits\n" << std::endl;
     
// Get parameter confidence interval and print in table
   for(int j = 1; j <= nparm; j++){
     
       intgen(theta.at(j - 1),vcvs.at(j - 1,j - 1),
              kodet.at(j - 1),conlev,s,xlow,xu);
     
       Rcpp::Rcout << "number = " << j - 1 << std::endl;
       Rcpp::Rcout << "type = " << kodet.at(j - 1) << std::endl;
       Rcpp::Rcout << "estimate = " << theta.at(j - 1) << std::endl;
       Rcpp::Rcout << "stderr = " << s << std::endl;
       Rcpp::Rcout << "lower = " << xlow << std::endl;
       Rcpp::Rcout << "upper = " << xu << std::endl;
     
   }
   
return;
  
}