#include <base/base.hpp>
#include <wqmcpoints/wqm_cpoints.hpp>

//' Wrapper for wqm_cpoints
//' @details Need wrapper to be able to call from 
//'          debugging mail program
// [[Rcpp::export]]
Rcpp::List wqmcpoints(Rcpp::NumericMatrix y,
                      int ny,
                      Rcpp::IntegerVector codes,
                      Rcpp::NumericVector codes2,
                      Rcpp::IntegerVector weight,
                      Rcpp::NumericVector weight2,
                      Rcpp::NumericMatrix ty,
                      int nty,
                      Rcpp::IntegerVector tcodes,
                      int n,
                      int nstart,
                      Rcpp::NumericVector dscrat,
                      Rcpp::NumericVector scrat,
                      Rcpp::IntegerVector iscrat,
                      int iprint,
                      int maxit,
                      double tol,
                      int maxmsd,
                      Rcpp::NumericVector p,
                      Rcpp::NumericVector q,
                      Rcpp::NumericVector prob,
                      Rcpp::NumericVector sd,
                      int m,
                      double pchmax,
                      bool lsd,
                      int ier,
                      Rcpp::IntegerVector ilcv,
                      Rcpp::IntegerVector iucv,
                      Rcpp::IntegerVector iltv,
                      Rcpp::IntegerVector iutv,
                      Rcpp::IntegerVector iorder,
                      Rcpp::NumericVector xlcen,
                      Rcpp::NumericVector xrcen,
                      Rcpp::NumericVector fail,
                      Rcpp::NumericVector xltru,
                      Rcpp::NumericVector xrtru,
                      Rcpp::NumericVector ys,
                      Rcpp::NumericVector pgrad,
                      Rcpp::NumericVector s,
                      Rcpp::NumericVector probd,
                      Rcpp::NumericVector fscrat){

debug::kprint = iprint;
Rcpp::List ints, doubs, bools, nummat, numvec, intvec;

wqm_cpoints(y,ny,codes,codes2,weight,weight2,ty,nty,tcodes,
            n,nstart,dscrat,scrat,iscrat,maxit,tol,maxmsd,
            p,q,prob,sd,m,pchmax,lsd,ier,ilcv,iucv,iltv,
            iutv,iorder,xlcen,xrcen,fail,xltru,xrtru,ys,
            pgrad,s,probd,fscrat);
            
ints = Rcpp::List::create(Named("m") = m,
                          Named("ier") = ier,
                          Named("maxmsd") = maxmsd,
                          Named("maxit") = maxit,
                          Named("nstart") = nstart);

doubs = Rcpp::List::create(Named("pchmax") = pchmax,
                           Named("tol") = tol);

bools = Rcpp::List::create(Named("lsd") = lsd);

numvec = Rcpp::List::create(Named("p") = p,
                            Named("q") = q,
                            Named("sd") = sd,
                            Named("prob") = prob,
                            Named("dscrat") = dscrat,
                            Named("scrat") = scrat,
                            Named("xlcen") = xlcen,
                            Named("xrcen") = xrcen,
                            Named("fail") = fail,
                            Named("xltru") = xltru,
                            Named("xrtru") = xrtru,
                            Named("ys") = ys,
                            Named("pgrad") = pgrad,
                            Named("s") = s,
                            Named("probd") = probd,
                            Named("fscrat") = fscrat,
                            Named("codes2") = codes2,
                            Named("weight2") = weight2);

intvec = Rcpp::List::create(Named("codes") = codes,
                            Named("wt") = weight,
                            Named("iscrat") = iscrat,
                            Named("tcodes") = tcodes,
                            Named("ilcv") = ilcv,
                            Named("iucv") = iucv,
                            Named("iltv") = iltv,
                            Named("iutv") = iutv,
                            Named("iorder") = iorder);

nummat = Rcpp::List::create(Named("y") = y,
                            Named("ty") = ty);
                  
return Rcpp::List::create(Named("ints") = ints,
                          Named("doubs") = doubs,
                          Named("bools") = bools,
                          Named("intvec") = intvec,
                          Named("numvec") = numvec,
                          Named("nummat") = nummat);
      
}

#include <base/base.hpp>
#include <wqmpoints/wqm_points.hpp>
#include <wqm_cdfest/wqm_cdfest.hpp>

// Call cdfest and points
//
// subroutine to compute the nonparametric maximum likelihood
// estimate of the cumulative distribution function
// and map to plotting points
// if possible, this is done with a generalized version of the
// kaplan-meier estimate, and otherwise, by using
// turnbull's e-m algorithm
// this shell routine sets up scratch array pointers and
// calls the basic routine
// inputs:
//   y(n,ny)      for simple data this is just a vector of length n (ny=1)
//                   more generally, first (second) col
//                   contains lower (upper) limits
//                   for type 4 (group censored) observations.
//                   if ny=2, for all other types set y(i,1)=y(i,2)
//   ny          number of cols in y (1 or 2)   ny = 1 for simple data
//   codes(n)    vector of censor codes
//                   0       dummy observation
//                   1       exact failure time
//                   2       right censored observation
//                   3       left censored observation
//                   4       interval or group censored obser.
//                   5       exact failure time recoded as a small interval
//   weight(n)   vector of observation weight or multiplicities
//   ty(n,nty)     first (second) col contains lower (upper) truncation limits
//                   for type 4 (interval truncated) observations.
//                   if nty=2, for all other types set ty(i,1)=ty(i,2)
//   nty          number of cols in ty (0, 1 or 2)
//                if nty=0, there is no truncation and ty is not accessed
//   tcodes(n)    vector of truncation codes
//                   1       no truncation
//                   2       right truncated observation
//                   3       left truncated observation
//                   4       interval truncated obser.
//   n           number of rows in y
//   dscrat(1)   double precision scratch array length (3n+2)
//   scrat(1)    real scratch array of
//                   length max((7*n),maxmsd*(maxmsd-1)/2)
//   iscrat(1)   integer scratch array of length (6*n+4)
//   iprint       print level for debug dump
//                   iprint=0 for no debug output
//                    if>0 dump setup and every iprint iteration
//                 iprint=1 gives the maximum amount of output
//                 iprint=10 is a good choice to debug
//   maxit       maximum number of iterations for s-c algorithm
//   tol         desired estimation accuracy (0<tol<.1)
//   maxmsd          maximum m for which the full information matrix
//                   estimates of the standard errors can be computed
//   nstart      =0 for automatic start values
//                  otherwise send down nstart values in prob
//                  (for restart)
// outputs:
//   x(n+1)      point on data scale x(i)=y(i) for simple data
//   q(n+1)      scratch space
//   prob(n+1)   prob(i) is an estimate of probability less than x(i)
//   m           return actual length of p, q, and prob (depends on data)
//   pchmax      returns max change in last iter if it was more than tol
//                   (0.0 if kaplan-meier was used)
//   ier         error code return
//               0     no error
//               1     n.le.0 on input
//               2     ny not equal to 1 or 2
//               3     nty not between 0 and 2
//               4     tol outside range (0, 0.1)
//               6     censor code out of range 0 to 4
//               7     y(i,1).ne.y(i,2) in a type 1, 2, or 3 observation
//               8     y(i,1).lt.y(i,2) in type 4 obs
//               9     ny=1 but type 4 obs found
//              10     tcode outside range 1 to 4
//              11     tcode=1, 2, or 3 but tyl.ne.tyu
//              12     backwards truncation interval
//              12     ty(i,1).lt.ty(i,2) in type 4 obs
//              13     only 1 col of truncation values, code 4 found
//              14     not enough data to estimate distribution
//                        (e.g., all right censored observations)
//              15     nstart>0 does not agree with computed m
//              16     observation not within the truncation interval
//             *21     product-limit estimate could not be computed
//                     directly and maxmsd was too small to allow the
//                     full information matrix to be computed
//                     an approximation was computed under the assumption
//                     that the individual hazard
//                     estimates are uncorrelated
//             *22     information matrix not positive definite
//             *23     only one non-zero s probability
// 
//          * signifies warning message only
// 
// ***************************************************************
// ****************************************************
// *                                                   *
// *   copyright c  1975-2008                          *
// *                     by william q. meeker, jr.     *
// *                        5697 arrasmith trail       *
// *                        ames, iowa     50010       *
// *                                                   *
// ****************************************************
//
void wqm_cpoints(Rcpp::NumericMatrix &y,
                 int &ny,
                 Rcpp::IntegerVector &codes,
                 Rcpp::NumericVector &codes2,
                 Rcpp::IntegerVector &weight,
                 Rcpp::NumericVector &weight2,
                 Rcpp::NumericMatrix &ty,
                 int &nty,
                 Rcpp::IntegerVector &tcodes,
                 int &n,
                 int &nstart,
                 Rcpp::NumericVector &dscrat,
                 Rcpp::NumericVector &scrat,
                 Rcpp::IntegerVector &iscrat,
                 int &maxit,
                 double &tol,
                 int &maxmsd,
                 Rcpp::NumericVector &p,
                 Rcpp::NumericVector &q,
                 Rcpp::NumericVector &prob,
                 Rcpp::NumericVector &sd,
                 int &m,
                 double &pchmax,
                 bool &lsd,
                 int &ier,
                 Rcpp::IntegerVector &ilcv,
                 Rcpp::IntegerVector &iucv,
                 Rcpp::IntegerVector &iltv,
                 Rcpp::IntegerVector &iutv,
                 Rcpp::IntegerVector &iorder,
                 Rcpp::NumericVector &xlcen,
                 Rcpp::NumericVector &xrcen,
                 Rcpp::NumericVector &fail,
                 Rcpp::NumericVector &xltru,
                 Rcpp::NumericVector &xrtru,
                 Rcpp::NumericVector &ys,
                 Rcpp::NumericVector &pgrad,
                 Rcpp::NumericVector &s,
                 Rcpp::NumericVector &probd,
                 Rcpp::NumericVector &fscrat){

int mplot = 0;

// First get the km estimator
   wqm_cdfest(y,ny,codes,weight,ty,nty,tcodes,n,nstart,
              dscrat,scrat,iscrat,maxit,tol,maxmsd,p,q,
              prob,sd,m,pchmax,lsd,ier,ilcv,iucv,iltv,
              iutv,iorder,xlcen,xrcen,fail,xltru,xrtru,ys,
              pgrad,s,probd,fscrat);

// Now get the points

// Aliases:
//  yplot = codes2
//  pplot = weight2
//  sdplot = ty
   int lsd2 = (int)lsd;
   codes2  = Rcpp::as<NumericVector>(codes);
   weight2 = Rcpp::as<NumericVector>(weight);
   
   wqm_points(q,p,prob,sd,lsd2,m,codes2,weight2,ty,mplot);

// Move results
   m = mplot;
   for(int i = 1; i <= m; i++){
   
       p.at(i - 1) = codes2.at(i - 1);
       prob.at(i - 1) = weight2.at(i - 1);
       sd.at(i - 1) = ty.at(i - 1,0);
       
   }
   
   return;

}