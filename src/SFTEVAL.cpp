#include <base/base.hpp>
#include <sfteval/fteval.hpp>

//' Does good stuff
//' @name sfteval
// [[Rcpp::export]]
Rcpp::List sfteval(int kdmod,
                   Rcpp::NumericVector xmu2,
                   Rcpp::NumericVector sig2,
                   Rcpp::NumericVector xmu3,
                   Rcpp::NumericVector sig3,
                   Rcpp::NumericVector rho,
                   Rcpp::NumericVector df,
                   Rcpp::NumericVector d0,
                   Rcpp::NumericVector sfact,
                   Rcpp::NumericVector tf,
                   int number,
                   Rcpp::NumericVector answer,
                   Rcpp::IntegerVector ier,
                   int kprint){

Rcpp::NumericVector RES(number);
Rcpp::List ints,numvec,intvec;

for(int i = 0; i < number; i++) {

 fteval(kdmod,
        xmu2.at(i),
        sig2.at(i),
        xmu3.at(i),
        sig3.at(i),
        rho.at(i),
        df.at(i),
        d0.at(i),
        sfact.at(i),
        tf.at(i),
        answer.at(i),
        ier.at(i));
  
}

ints = Rcpp::List::create(Named("number") = number,
                          Named("kdmod") = kdmod);

intvec = Rcpp::List::create(Named("ier") = ier);

numvec = Rcpp::List::create(Named("answer") = answer,
                            Named("xmu2") = xmu2,
                            Named("sig2") = sig2,
                            Named("xmu3") = xmu3,
                            Named("sig3") = sig3,
                            Named("rho") = rho,
                            Named("df") = df,
                            Named("d0") = d0,
                            Named("sfact") = sfact,
                            Named("tf") = tf);

return Rcpp::List::create(Named("ints") = ints,
                          Named("intvec") = intvec,
                          Named("numvec") = numvec);

}

/***R
library(smrdfortran)
kdmod = 1
mu1 = c(0.5, 0.25, 0.015, 5)
sig1 = 0.25
mu2 = 1.1
sig2  = 0.55
rho = .5
df = -0.5
d0 = 0
sfact = 0
tf = 1
kprint = 0
debug1 = F

  max.length <- max(length(mu1) , length(sig1) , length(mu2),
                    length(sig2), length(rho)  , length(df),
                    length(d0)  , length(sfact), length(tf))
  mu1    <- smrdfortran:::expand.vec(mu1, max.length)
  sig1   <- smrdfortran:::expand.vec(sig1, max.length)
  mu2    <- smrdfortran:::expand.vec(mu2, max.length)
  sig2   <- smrdfortran:::expand.vec(sig2, max.length)
  rho    <- smrdfortran:::expand.vec(rho, max.length)
  df     <- smrdfortran:::expand.vec(df, max.length)
  d0     <- smrdfortran:::expand.vec(d0, max.length)
  sfact  <- smrdfortran:::expand.vec(sfact, max.length)
  tf     <- smrdfortran:::expand.vec(tf, max.length)
  zout <- .Fortran("sfteval", as.integer(kdmod), as.double(mu1),
                   as.double(sig1), as.double(mu2), as.double(sig2), as.double(rho),
                   as.double(df), as.double(d0), as.double(sfact), as.double(tf),
                   as.integer(max.length), answer = double(max.length),
                   ier = integer(max.length), as.integer(kprint))
  new <- wqmmlesss::sfteval(as.integer(kdmod), as.double(mu1),
                   as.double(sig1), as.double(mu2), as.double(sig2), as.double(rho),
                   as.double(df), as.double(d0), as.double(sfact), as.double(tf),
                   as.integer(max.length), answer = double(max.length),
                   ier = integer(max.length), as.integer(kprint))

  sfteval_test <- function(kdmod = 1,
                           mu1 = 0.5,
                           sig1 = 0.25,
                           mu2 = 1,
                           sig2 = 0.5,
                           rho = 0.5,
                           df = -0.5,
                           d0 = 0,
                           sfact = 0,
                           tf = 1,
                           kprint = 0){

    max.length <- max(length(mu1) , length(sig1) , length(mu2),
                    length(sig2), length(rho)  , length(df),
                    length(d0)  , length(sfact), length(tf))
  mu1    <- smrdfortran:::expand.vec(mu1, max.length)
  sig1   <- smrdfortran:::expand.vec(sig1, max.length)
  mu2    <- smrdfortran:::expand.vec(mu2, max.length)
  sig2   <- smrdfortran:::expand.vec(sig2, max.length)
  rho    <- smrdfortran:::expand.vec(rho, max.length)
  df     <- smrdfortran:::expand.vec(df, max.length)
  d0     <- smrdfortran:::expand.vec(d0, max.length)
  sfact  <- smrdfortran:::expand.vec(sfact, max.length)
  tf     <- smrdfortran:::expand.vec(tf, max.length)

  new <- wqmmlesss::sfteval(as.integer(kdmod),
                         as.double(mu1),
                         as.double(sig1),
                         as.double(mu2),
                         as.double(sig2),
                         as.double(rho),
                         as.double(df),
                         as.double(d0),
                         as.double(sfact),
                         as.double(tf),
                         as.integer(max.length),
                         answer = double(max.length),
                         ier = integer(max.length),
                         as.integer(kprint))
  old <- smrdfortran:::fteval(kdmod,
                       mu1,
                       sig1,
                       mu2,
                       sig2,
                       rho,
                       df,
                       d0,
                       sfact,
                       tf,
                       kprint,
                       debug1= F)

    return(abs(old - new[,1]))

                           }
*/
