#include <base/base.hpp>
#include <sloglikenhpp/floglikenhpp.hpp>

//' Compute a vector of NHPP log likelihood values 
//' corresponding to a thetav matrix
//' @name sloglikenhpp
// [[Rcpp::export]]
Rcpp::NumericVector sloglikenhpp(Rcpp::NumericVector time, 
                                 int ntimes, 
                                 Rcpp::NumericVector recurrcosts,
                                 Rcpp::NumericVector timel, 
                                 Rcpp::NumericVector timeu, 
                                 Rcpp::IntegerVector kwcount,
                                 int nwindows, 
                                 int kform, 
                                 Rcpp::NumericMatrix thetav, 
                                 int nparm, 
                                 int ntheta, 
                                 Rcpp::NumericVector answer){
  
for(int itheta = 0; itheta < ntheta; itheta++){
  
    answer.at(itheta) = floglikenhpp(time, ntimes,recurrcosts,
                                     timel, timeu,kwcount, nwindows,
                                     kform,thetav.column(itheta));
  
}

return answer;
  
}

#include <base/base.hpp>
#include <sloglikenhpp/flogrecurrate.hpp>
#include <sloglikenhpp/fmcfdiff.hpp>

//' Compute the NHPP log likelihood

double floglikenhpp(Rcpp::NumericVector time, 
                    int ntimes, 
                    Rcpp::NumericVector recurrcosts,
                    Rcpp::NumericVector timel, 
                    Rcpp::NumericVector timeu, 
                    Rcpp::IntegerVector kwcount, 
                    int nwindows, 
                    int kform, 
                    Rcpp::NumericVector theta){

double floglike_nhpp = 0.0e00;
  
// accumulate terms for the recurrences;
for(int i = 0; i < ntimes; i++){
  
    floglike_nhpp = floglike_nhpp + recurrcosts.at(i) * flogrecurrate(time.at(i), kform, theta);
  
}

// accumulate terms for the observation windows;
for(int k = 0; k < nwindows; k++){
  
    floglike_nhpp = floglike_nhpp - float(kwcount.at(k)) * fmcfdiff(timel.at(k), timeu.at(k), kform, theta);
  
}

return floglike_nhpp;

}

#include <base/base.hpp>
#include <sloglikenhpp/flogrecurratepower.hpp>
#include <sloglikenhpp/flogrecurrateloglin.hpp>

//' Compute the log nhpp recurrence rate
//' for a given form of the rate function.

double flogrecurrate(double time, 
                     int kform, 
                     Rcpp::NumericVector theta){

double flog_recurrate = 0.0e00;
  
if(kform == 1){
  
  flog_recurrate = flogrecurratepower(time, theta);
  
}

if(kform == 2){
  
  flog_recurrate = flogrecurrateloglin(time, theta);
  
}

return flog_recurrate;

}

#include <base/base.hpp>

//' Compute the log loglin NHPP recurrence rate

double flogrecurrateloglin(double time, 
                           Rcpp::NumericVector theta){

  return theta.at(0) + theta.at(1) * time;
  
}

#include <base/base.hpp>

//' Compute the log power law nhpp recurrence rate

double flogrecurratepower(double time, 
                          Rcpp::NumericVector theta){
  
double  eta = theta.at(0);
double beta = theta.at(1);

return std::log(beta / eta) + (beta - 1.0e00) * std::log(time / eta);

}

#include <base/base.hpp>
#include <sfmcf/fmcf.hpp>

//' Compute the NHPP mcf difference

double fmcfdiff(double timel, 
                double timeu, 
                int kform,
                Rcpp::NumericVector theta){
  
double fmcf_diff = 0.0e00;
  
if(timel <= 0.0e00) {
  
   fmcf_diff = fmcf(timeu, kform, theta);
  
 } else {
  
   fmcf_diff = fmcf(timeu, kform, theta) - fmcf(timel, kform, theta);
  
}
 
return fmcf_diff;
 
}
