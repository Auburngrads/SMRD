#include <base/base.hpp>
#include <sfmcf/fmcf.hpp>

//' compute the vector of nhpp mcf values
//' @name sfmcf
// [[Rcpp::export]]
Rcpp::NumericVector sfmcf(Rcpp::NumericVector time,
                          int kform,
                          Rcpp::NumericVector theta,
                          int ntimes,
                          Rcpp::NumericVector answer){

for(int i = 0; i < ntimes; i++){
  
    answer.at(i) = fmcf(time.at(i), kform, theta);
  
}

return answer;

}

#include <base/base.hpp>
#include <sfmcf/fmcfpower.hpp>
#include <sfmcf/fmcfloglin.hpp>

//' Compute the nhpp mcf for a given
//' form of the rate function

double fmcf(double time,
            int kform,
            Rcpp::NumericVector theta){

if(kform == 1) { return fmcfpower(time, theta) ; }

if(kform == 2) { return fmcfloglin(time, theta); }

if(kform == 3) { return 0.0e00; }

if(kform == 4) { return 0.0e00; }

if(kform == 5) { return 0.0e00; }

if(kform == 6) { return 0.0e00; }

      return 0.0e00;

}

#include <base/base.hpp>

//' Function to compute the loglin nhpp mcf

double fmcfloglin(double time,
                  Rcpp::NumericVector theta){

      double gamma0  = theta.at(0);
      double gamma1  = theta.at(1);

      return (std::exp(gamma0) / gamma1) * (std::exp(gamma1 * time) - 1.0e00);
      
}

#include <base/base.hpp>

//' Compute the power law NHPP mcf

double fmcfpower(double time,
                 Rcpp::NumericVector theta){

double eta  = theta.at(0);
double beta = theta.at(1);

return std::pow((time / eta), beta);
      
}
