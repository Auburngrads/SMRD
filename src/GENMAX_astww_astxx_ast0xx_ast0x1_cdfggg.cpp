#include <base/base.hpp>
#include <genmax/cdfes2.hpp>
#include <genmax/cdfcvf.hpp>

//' @description Genmax subroutine to compute the nonparametric 
//'              maximum likelihood estimate of the cumulative 
//'              distribution function. If possible, this is done 
//'              with a generalized version of the Kaplan-Meier 
//'              estimate, and otherwise, by using Turnbull's E-M 
//'              algorithm.
//'              
//' @param y(n,ny) first (second) col contains lower (upper) limits;
//' # for type 4 (group censored) observations.;
//' # if ny=2, for all other types set y(i,1)=y(i,2);
//' # ny number of cols in y (1 or 2);
//' # codes(n) vector of censor codes;
//' # 0 dummy observation;
//' # 1 exact failure time;
//' # 2 right censored observation;
//' # 3 left censored observation;
//' # 4 interval or group censored obser.;
//' # 5 small interval obser (treat as an exact).;
//' # weight(n) vector of observation weight or multiplicities;
//' # ty(n,nty) first (second) col contains lower (upper) truncation limits;
//' # for type 4 (interval truncated) observations.;
//' # if nty=2, for all other types set ty(i,1)=ty(i,2);
//' # nty number of cols in ty (0, 1 or 2);
//' # if nty=0, there is no truncation and ty is not accessed;
//' # tcodes(n) vector of truncation codes;
//' # 1 no truncation;
//' # 2 right truncated observation;
//' # 3 left truncated observation;
//' # 4 interval truncated obser.;
//' # n number of rows in y;
//' # length max((7*n),maxmsd*(maxmsd-1)/2);
//' # kprint print level for debug dump;
//' # kprint=0 for no debug output;
//' # if>0 debug output;
//' # kprint=10 gives the maximum amount of output;
//' # kprint=4 is a good choice to debug;
//' # maxit maximum number of iterations for s-c algorithm;
//' # tol desired estimation accuracy (0<tol<.1);
//' # maxmsd maximum m for which the full information matrix;
//' # estimates of the standard errors can be computed;
//' # nstart =0 for automatic start values;
//' # otherwise send down nstart values in prob;
//' # (for restart);
//' #outputs:;
//' # p(n+1) lower limits of intervals for cdf estimate;
//' # q(n+1) upper limits of intervals for cdf estimate;
//' # prob(n+1) cdf estimate corresponding to intervals p,q;
//' # sd(n+1) se of cdf estimate corresponding to intervals p,q;
//' # m return actual length of p, q, and prob (depends on data);
//' # xllgkm returns the actual value of the likelihood;
//' # pchmax returns max change in last iter if it was more than tol;
//' # (0.0 if kaplan-meier was used);
//' # ier error code return;
//' # 0 no error;
//' # 1 n <= 0 on input;
//' # 2 ny not equal to 1 or 2;
//' # 3 nty not between 0 and 2;
//' # 4 tol outside range (0, 0.1);
//' # 6 censor code out of range 0 to 4;
//' # 7 y(i,1).ne.y(i,2) in a type 1, 2, or 3 observation;
//' # 8 y(i,1) < y(i,2) in type 4 obs;
//' # 9 ny=1 but type 4 obs found;
//' # 10 tcode outside range 1 to 4;
//' # 11 tcode=1, 2, or 3 but tyl.ne.tyu;
//' # 12 backwards truncation interval;
//' # 12 ty(i,1) < ty(i,2) in type 4 obs;
//' # 13 only 1 col of truncation values, code 4 found;
//' # 14 not enough data to estimate distribution;
//' # (e.g., all right censored observations);
//' # 15 nstart>0 does not agree with computed m;
//' # 16 observation not within the truncation interval;
//' # *21 product-limit estimate could not be computed;
//' # directly and maxmsd was too small to allow the;
//' # full information matrix to be computed;
//' # an approximation was computed under the assumption;
//' # that the individual hazard estimates are uncorrelated;
//' # *22 information matrix not positive definite;
//' # *23 only one non-zero s probability;
//' #;
//' # * signifies warning message only;
//' #;
//' # william q. meeker, jr.;
//' # department of statistics;
//' # iowa state university;
//' # ames, iowa 50011;
//' #;
//' # may 18, 1984;
//' #modified 28 jan 1989 to return loglikelihood;

void cdfggg(Rcpp::NumericMatrix &y,
            int &ny,
            Rcpp::IntegerVector &codes,
            Rcpp::IntegerVector &weight,
            Rcpp::NumericMatrix &ty,
            int &nty,
            Rcpp::IntegerVector &tcodes,
            int &n,
            int &nstart,
            int &maxit,
            double &tol,
            int &maxmsd,
            Rcpp::NumericVector &p,
            Rcpp::NumericVector &q,
            Rcpp::NumericVector &prob,
            Rcpp::NumericVector &sd,
            int &m,
            double &pchmax,
            int &lsd,
            int &ier){
  
// Set up scratch array
   int np3 = n + 3;
   double maxmat = maxmsd * (maxmsd - 1) / 2;
   double xllgkm;

Rcpp::NumericVector isave  = Rcpp::NumericVector(np3);
Rcpp::NumericVector iprobd = Rcpp::NumericVector(np3);
Rcpp::NumericVector ixlcen = Rcpp::NumericVector(np3);
Rcpp::NumericVector ipgrad = Rcpp::NumericVector(np3);
Rcpp::NumericVector ixrcen = Rcpp::NumericVector(np3);
Rcpp::NumericVector ifscr  = Rcpp::NumericVector(maxmat);
Rcpp::NumericVector ifail  = Rcpp::NumericVector(np3);
Rcpp::NumericVector ixltru = Rcpp::NumericVector(np3);
Rcpp::NumericVector ixrtru = Rcpp::NumericVector(np3);
Rcpp::NumericVector iys    = Rcpp::NumericVector(2 * np3);

Rcpp::IntegerVector ilcv   = Rcpp::IntegerVector(np3);
Rcpp::IntegerVector iucv   = Rcpp::IntegerVector(np3);
Rcpp::IntegerVector iltv   = Rcpp::IntegerVector(np3);
Rcpp::IntegerVector iutv   = Rcpp::IntegerVector(np3);
Rcpp::IntegerVector iorder = Rcpp::IntegerVector(2 * np3);

 cdfes2(y,ny,codes,weight,ty,nty,tcodes,n,nstart,maxit,tol,
        maxmsd,ixlcen,ixrcen,ifail,ixltru,
        ixrtru,iys,p,q,ipgrad,prob,sd,isave,
        iprobd,m,xllgkm,ilcv,iucv,iltv,iutv,
        iorder,ifscr,pchmax,lsd,ier);
 
// cover up first jump at a time point;
   if(ier <= 0) { 
   
    cdfcvf(p,q,prob,sd,lsd,m);
     
   }

  return;

}
