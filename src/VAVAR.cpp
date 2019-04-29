#include <base/base.hpp>
#include <slsinf/lsinf.hpp>

// [[Rcpp::export]]
Rcpp::NumericVector vavar(int idist,
                          int nrows,
                          Rcpp::NumericVector zc,
                          Rcpp::NumericVector ze,
                          Rcpp::NumericVector avar){
double two = 2.0e00;
NumericVector F11(nrows,0.0);
NumericVector F12(nrows,0.0);
NumericVector F22(nrows,0.0);
int ifault = 0;
double det;
double f11, f12, f22;

for(int irow = 0; irow < nrows; irow++) {

Rcpp::List LSINF = lsinf(idist,2,
                         zc.at(irow),
                         zc.at(irow),
                         F11.at(irow),
                         F12.at(irow),
                         F22.at(irow),
                         ifault);

f11 = Rcpp::as<double>(Rcpp::as<Rcpp::List>(LSINF)["f11"]);
f12 = Rcpp::as<double>(Rcpp::as<Rcpp::List>(LSINF)["f12"]);
f22 = Rcpp::as<double>(Rcpp::as<Rcpp::List>(LSINF)["f22"]);

      det = f11 * f22 - f12 * f12;
      avar.at(irow) = (f22 - two * ze.at(irow) * f12 + ze.at(irow) * ze.at(irow) * f11) / det;
      
}

      return avar;

}