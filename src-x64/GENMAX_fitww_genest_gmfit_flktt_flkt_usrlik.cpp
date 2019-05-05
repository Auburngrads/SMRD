#include <base/base.hpp>

//' user specified likelihood to compute likelihood for a single
//' observation as a function of the following.
//' 
//' thetas(nparm)     vector of parameter values
//' nparm            length of thetas
//' kdist            distribution code
//' kmod             model code
//' yl               (log) failure or censoring time or lower limit of interval
//' yu               upper limit of (log) time interval
//' kccode           censoring code
//' tryl             (log) truncation time or lower limit of a truncation interv
//' tryu             upper limit of truncation interval
//' ktcode           truncation code

double usrlik(Rcpp::NumericVector thetas,
              int nparm,
              int kdist,
              int kmod,
              double yl,
              double yu,
              int kccode,
              double tryl,
              double tryu,
              int ktcode){

double usr_lik = 0.0e00;   
Rcpp::warning("Function usrlik is not yet finished");

return usr_lik;

}