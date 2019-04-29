#include <base/base.hpp>

//' Fix lower cdf confidence limits to be monotone
//' 
//' @name bfixl
// [[Rcpp::export]]
Rcpp::NumericVector bfixl(Rcpp::NumericVector vec){

  int n = vec.length();

for(int i = 1; i < n; i++){

      if(vec.at(i) < vec.at(i - 1)) {

         vec.at(i) = vec.at(i - 1);

      }
}
  return vec;
}



#include <base/base.hpp>

//' Fix upper cdf confidence limits to be monotone
//' 
//' @name bfixu
// [[Rcpp::export]]
NumericVector bfixu(Rcpp::NumericVector vec){

  int n = vec.length();

  for(int i = (n - 1); i > 0; i--){

    // do in reverse
    if(vec.at(i - 1) > vec.at(i)) {

       vec.at(i - 1) = vec.at(i);

    }
  }
  
return vec;
  
}

/*** R
test_cpp <- T
library(SMRD)
DATA <- lzbearing
lz.ld <- frame.to.ld(DATA, response.column = 1)

cdfest.out = cdfest(lz.ld)
band.type = 'pointwise'
conf.level = GetSMRDDefault("SMRD.ConfLevel")/100
a.limit = 0.001
b.limit = 0.999
mono.tran = T

  cdpoints.out <- SMRD:::cdpoints(cdfest.out)
  ok <- cdpoints.out$sdplot > 0
  times <- cdpoints.out$yplot[ok]
  fhat.point <- cdpoints.out$pplot
  dist.probs <- cdpoints.out$pplot[ok]
  stderrq <- cdpoints.out$sdplot[ok]/(dist.probs[ok] * (1 -
    dist.probs[ok]))
  nux.squared <- cdpoints.out$number.observations * (cdpoints.out$sdplot[ok]/dist.probs)^2
  kx <- nux.squared/(1 + nux.squared)
  zvalue <- 0
  if (is.null(band.type) || band.type == "")
    band.type <- "none"
  switch(casefold(band.type), s = , simultaneous = {
  band.type <- "Simultaneous"
    bands.over <- kx > a.limit & kx < b.limit
    zvalue <- SMRD:::evalue(a = a.limit, b = b.limit, conf.level = conf.level)
  }, `Point-wise` = , pointwise = , p = , `point-wise` = {
    band.type <- "Pointwise"
    bands.over <- kx > 0 & kx < 1
    zvalue <- qnorm(1 - (1 - conf.level)/2)
  }, none = {
    band.type <- "none"
  }, {
    warning("band.type not recognized")
    band.type <- "none"
  })
    if (band.type == "none") {
      return(list(times = times, fhat = dist.probs, lower = NULL,
                  upper = NULL, bands.over = NULL, band.type = band.type))
    }
    lower <- plogis(qlogis(dist.probs) - zvalue * stderrq)
    upper <- plogis(qlogis(dist.probs) + zvalue * stderrq)
    loweR <- plogis(qlogis(dist.probs) - zvalue * stderrq)
    uppeR <- plogis(qlogis(dist.probs) + zvalue * stderrq)

            if (mono.tran) {
        loweR[!is.na(lower)] <- SMRD:::mono.lower(loweR[!is.na(loweR)])
        uppeR[!is.na(upper)] <- SMRD:::mono.upper(uppeR[!is.na(uppeR)])
            }


  lower[!is.na(lower)] <- bfixl(lower[!is.na(lower)])
  upper[!is.na(upper)] <- bfixu(upper[!is.na(upper)])


lower ; loweR
upper ; uppeR
*/
