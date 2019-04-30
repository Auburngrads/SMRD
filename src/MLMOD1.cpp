#include <base/base.hpp>

//' Meeker-LuValle Model 1
//' 
//' @name mlmod1
//' 
//' @description Compute growth of filament according 
//'              to Meeker-LuValle Model 1 different 
//'              rate parameter at each time interval
// [[Rcpp::export]]
Rcpp::List MLMOD1(Rcpp::NumericVector times, 
                           int number_times,
                           double a2_init, 
                           double a2_limit, 
                           double rate, 
                           Rcpp::NumericVector rate_factor,  
                           Rcpp::NumericVector a2){

double time_difference, a1_left, rate_now, a22;
  
// initilize the process
   a2.at(0) = a2_init;

// loop over times where solution is needed
for(int itime = 1; itime < number_times; itime++){
  
    time_difference = times.at(itime) - times.at(itime - 1);
    a1_left  = a2_limit - a2.at(itime - 1);
    rate_now = rate_factor.at(itime) * rate;

// diffyq solution
   a22 = one - std::exp(-rate_now * time_difference);
   a2.at(itime) = a2.at(itime - 1) + a1_left * a22;
    
}

return Rcpp::List::create(Named("a2") = a2);

}