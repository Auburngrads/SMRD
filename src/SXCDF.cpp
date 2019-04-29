#include <base/base.hpp>
#include <sxcdf/xcdf.hpp>

// [[Rcpp::export]]
Rcpp::List sxcdf(int ndist1,
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
                 Rcpp::IntegerVector ier){

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

return Rcpp::List::create(Named("ier") = ier,
                          Named("ndist1") = ndist1,
                          Named("ndist2") = ndist2,
                          Named("beta0") = beta0,
                          Named("beta1") = beta1,
                          Named("xstr") = xstr,
                          Named("sigma") = sigma,
                          Named("ugamma") = ugamma,
                          Named("sgamma") = sgamma,
                          Named("w") = w,
                          Named("num") = num,
                          Named("answer") = answer);
  
}

/*** R
library(smrdfortran)
ndist1 = 2
ndist2 = 2
beta0 = 30.27241
beta1 = -5.100121
stress = 270
sigma = 0.2894549
ugamma = 5.365834
sgamma = 0.03140004
w = logb(5000)
debug1= F
    
      max.length <- max(length(beta0), length(beta1), length(sigma),
                        length(ugamma), length(sgamma), length(stress), length(w))
      beta0  <- smrdfortran:::expand.vec(beta0, max.length)
      beta1  <- smrdfortran:::expand.vec(beta1, max.length)
      sigma  <- smrdfortran:::expand.vec(sigma, max.length)
      ugamma <- smrdfortran:::expand.vec(ugamma, max.length)
      sgamma <- smrdfortran:::expand.vec(sgamma, max.length)
      stress <- smrdfortran:::expand.vec(stress, max.length)
      w      <- smrdfortran:::expand.vec(w, max.length)
      if (debug1) browser()
      zout <- .Fortran("sxcdf", as.integer(ndist1), as.integer(ndist2),
                       as.double(beta0), as.double(beta1), as.double(stress),
                       as.double(sigma), as.double(ugamma), as.double(sgamma),
                       as.double(w), as.integer(max.length), answer = double(max.length),
                       ier = integer(max.length))
      
      new = wqmmlesss::sxcdf(as.integer(ndist1), as.integer(ndist2),
                       as.double(beta0), as.double(beta1), as.double(stress),
                       as.double(sigma), as.double(ugamma), as.double(sgamma),
                       as.double(w), as.integer(max.length), answer = double(max.length),
                       ier = integer(max.length))

*/