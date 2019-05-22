#include <base/base.hpp>
#include <sxcdf/xcdf.hpp>

// [[Rcpp::export]]
Rcpp::List SXCDF(int ndist1,
                 int ndist2,
                 Rcpp::NumericVector beta0,
                 Rcpp::NumericVector beta1,
                 Rcpp::NumericVector xstr,
                 Rcpp::NumericVector sigma,
                 Rcpp::NumericVector ugamma,
                 Rcpp::NumericVector sgamma,
                 Rcpp::NumericVector w,
                 int num,
                 Rcpp::NumericVector answer,
                 Rcpp::IntegerVector ier,
                 int kprint){

debug::kprint = kprint;
  
Rcpp::NumericVector beta0p = clone(beta0);
Rcpp::NumericVector beta1p = clone(beta1);
Rcpp::NumericVector xstrp = clone(xstr);
Rcpp::NumericVector sigmap = clone(sigma);
Rcpp::NumericVector ugammap = clone(ugamma);
Rcpp::NumericVector sgammap = clone(sgamma);
Rcpp::NumericVector wp = clone(w);
Rcpp::NumericVector answerp = clone(answer);
Rcpp::IntegerVector ierp = clone(ier);

for(int i = 0; i < num; i++){
  
    xcdf(ndist1,ndist2,beta0p.at(i),beta1p.at(i),
         xstrp.at(i),sigmap.at(i),ugammap.at(i),
         sgammap.at(i),wp.at(i),answerp.at(i),
         ierp.at(i));
  
}

return Rcpp::List::create(Named("ier") = ierp,
                          Named("ndist1") = ndist1,
                          Named("ndist2") = ndist2,
                          Named("beta0") = beta0p,
                          Named("beta1") = beta1p,
                          Named("xstr") = xstrp,
                          Named("sigma") = sigmap,
                          Named("ugamma") = ugammap,
                          Named("sgamma") = sgammap,
                          Named("w") = wp,
                          Named("num") = num,
                          Named("answer") = answerp);
  
}