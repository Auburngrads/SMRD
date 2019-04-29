#include <base/base.hpp>
#include <wqm_cdfest/wqm_cdfest.hpp>

//' Compute Nonparametric CDF Estimates
//'
//' @name wqm_cdfest
//' @description Computes nonparametric estimates of the cumulative distribution function
//'              using maximum likelihood. If possible, estimation this is done with a
//'              generalized version of the Kaplan-Meier estimate, and otherwise, by using
//'              Turnbull's e-m algorithm
//' @param y Numeric matrix \code{[n x ny]} containing the lower and upper limits for group
//'          censored observations.
//' @param ny Number of columns in \code{y} (either 1 or 2)
//' @param codes Integer vector of censor codes (see Details)
//' @param weight Vector of observation weights or multiplicities
//' @param ty Numeric matrix containing lower and upper truncation limits
//' @param nty Number of columns in ty
//' @param tcodes Integer vector of truncation codes (see details)
//' @param n Number of rows in y
//' @param dscrat Numeric scratch vector (length = 3 * n + 2)
//' @param scrat Numeric scratch array (length = max((7 * n), maxmsd * (maxmsd - 1) / 2))
//' @param iscrat Integer scratch array (length = 6 * n + 4)
//' @param iprint Print level for debug dump
//' @param maxit Maximum number of iterations for s-c algorithm
//' @param tol Desired estimation accuracy (0 < \code{tol} < 0.1)
//' @param maxmsd Maximum \code{m} for which the full information matrix estimates of the
//'        standard errors can be computed
//' @param nstart If \code{nstart = 0} set automatic start values, otherwise send down \code{nstart}
//'        values in \code{prob} for restart
//'
//' @return A \code{list} of length 6
//' \itemize{
//'  \item{p}{Numeric vector of lower limits of intervals for the cdf estimate (length = n + 1)}
//'  \item{q}{Numeric vector of upper limits of intervals for the cdf estimate (length = n + 1)}
//'  \item{prob}{Numeric vector of cdf estimates corresponding to intervals \code{p} and \code{q}}
//'  \item{m}{Actual length of \code{p}, \code{q}, and \code{prob} (depends on data)}
//'  \item{pchmax}{Max change in last iteration (if greater than tol, 0.0 if Kaplan-Meier was used)}
//'  \item{ier}{Error code (see details)}
//' }
//'              0     no error
//'              1     n <= 0 on input
//'              2     ny not equal to 1 or 2
//'              3     nty not between 0 and 2
//'              4     tol outside range (0, 0.1)
//'              6     censor code out of range 0 to 4
//'              7     y(i,1) != y(i,2) in a type 1, 2, or 3 observation
//'              8     y(i,1) <  y(i,2) in type 4 obs
//'              9     ny=1 but type 4 obs found
//'             10     tcode outside range 1 to 4
//'             11     tcode = 1, 2, or 3 but tyl != tyu
//'             12     backwards truncation interval
//'             12     ty(i,1) < ty(i,2) in type 4 obs
//'             13     only 1 col of truncation values, code 4 found
//'             14     not enough data to estimate distribution
//'                       (e.g., all right censored observations)
//'             15     nstart>0 does not agree with computed m
//'             16     observation not within the truncation interval
//'            *21     product-limit estimate could not be computed
//'                    directly and maxmsd was too small to allow the
//'                    full information matrix to be computed
//'                    an approximation was computed under the assumption
//'                    that the individual hazard
//'                    estimates are uncorrelated
//'            *22     information matrix not positive definite
//'            *23     only one non-zero s probability
//'
//'         * signifies warning message only
//'
//' @details
//' \itemize{
//'  \item{0}{dummy observation}
//'  \item{1}{exact failure time}
//'  \item{2}{right censored observation}
//'  \item{3}{left censored observation}
//'  \item{4}{interval or group censored observation}
//'  \item{5}{exact failure time recoded as a small interval}
//'  }
//' \itemize{
//'  \item{1}{no truncation}
//'  \item{2}{right truncated observation}
//'  \item{3}{left truncated observation}
//'  \item{4}{interval truncated observation}
//'  }
//'
//'  \code{iprint = 0} for no debug output, if > 0 dump setup and every iprint iteration,
//'  \code{iprint = 1} gives the maximum amount of output, \code{iprint = 10} is a good choice to debug
// [[Rcpp::export]]
Rcpp::List wqmcdfest(NumericMatrix &y,
                      int &ny,
                      IntegerVector &codes,
                      IntegerVector &weight,
                      NumericMatrix &ty,
                      int &nty,
                      IntegerVector &tcodes,
                      int &n,
                      int &nstart,
                      NumericVector &dscrat,
                      NumericVector &scrat,
                      IntegerVector &iscrat,
                      int &kprint,
                      int &maxit,
                      double &tol,
                      int &maxmsd,
                      NumericVector &p,
                      NumericVector &q,
                      NumericVector &prob,
                      NumericVector &sd,
                      int &m,
                      double &pchmax,
                      bool &lsd,
                      int &ier){

debug::kprint = kprint;

Rcpp::List nummat, numvec, intvec, others;
  
Rcpp::IntegerVector ilcv   = IntegerVector(n);  
Rcpp::IntegerVector iucv   = IntegerVector(n);  
Rcpp::IntegerVector iltv   = IntegerVector(n);  
Rcpp::IntegerVector iutv   = IntegerVector(n);  
Rcpp::IntegerVector iorder = IntegerVector(2 * n);

Rcpp::NumericVector xlcen = NumericVector(n);
Rcpp::NumericVector xrcen = NumericVector(n);
Rcpp::NumericVector fail  = NumericVector(n);
Rcpp::NumericVector xltru = NumericVector(n);
Rcpp::NumericVector xrtru = NumericVector(n);
Rcpp::NumericVector ys    = NumericVector(2 * n); 
Rcpp::NumericVector pgrad = NumericVector(n);
Rcpp::NumericVector s     = NumericVector(n);
Rcpp::NumericVector probd = NumericVector(n);
Rcpp::NumericVector fscrat = NumericVector(n);

  wqm_cdfest(y,ny,codes,weight,ty,nty,tcodes,n,nstart,
             dscrat,scrat,iscrat,maxit,tol,maxmsd,
             p,q,prob,sd,m,pchmax,lsd,ier,ilcv,iucv,iltv,
             iutv,iorder,xlcen,xrcen,fail,xltru,xrtru,ys,
             pgrad,s,probd,fscrat);
             
nummat = List::create(Named("y")  = y,
                      Named("ty") = ty);

intvec = List::create(Named("codes")  = codes,
                      Named("weight") = weight,
                      Named("tcodes") = tcodes,
                      Named("ilcv")   = ilcv,   
                      Named("iucv")   = iucv,   
                      Named("iltv")   = iltv,   
                      Named("iutv")   = iutv,   
                      Named("iorder") = iorder);

numvec = List::create(Named("xlcen") = xlcen,
                      Named("xrcen") = xrcen,
                      Named("fail")  = fail,
                      Named("xltru") = xltru,
                      Named("xrtru") = xrtru,
                      Named("ys")    = ys,
                      Named("p")     = p,
                      Named("q")     = q,
                      Named("pgrad") = pgrad,
                      Named("prob")  = prob,
                      Named("sd")    = sd,
                      Named("s")     = s,
                      Named("probd") = probd,
                      Named("fscrat") = fscrat);

others = List::create(Named("ny")     = y.ncol(),
                      Named("nty")    = nty,
                      Named("n")      = y.nrow(),
                      Named("nstart") = nstart,
                      Named("maxit")  = maxit,
                      Named("tol")    = tol,
                      Named("maxmsd") = maxmsd,
                      Named("m")      = m,
                      Named("pchmax") = pchmax,
                      Named("lsd")    = lsd,
                      Named("ier")    = ier);
  
   return List::create(Named("nummat") = nummat,
                       Named("numvec") = numvec,
                       Named("intvec") = intvec,
                       Named("others") = others);
                       
}