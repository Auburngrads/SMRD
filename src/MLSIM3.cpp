#include <base/base.hpp>
#include <mlsim2/mthcdf.hpp>
#include <mlsim2/simnp.hpp>
#include <mlsim2/snset.hpp>
#include <wqm_cdfest/wqm_cdfest.hpp>

// #method 3 simulation---nonparametric sampling/parametric inference;
// #;
// #must send down a complete data set;
// #;
// # parameter meanings same as in wqm_cdfest except the following:;
// #;
// #;
// #iarray(marray) scratch space needed to generate random weights;
// #;
// #marray length of scratch space. must be at least equal;
// # in length to the number of rows in wt;
// # a computationally more efficient method is used;
// # if the number is larger than the sum of the weights;
// #;
// #wtnew vector for the random weights for the simulation;
// #;
// #xnew hold space for x;
// #;
// #ynew hold space for y;
// #;
// #;
// #retmat(numret,numsim) matrix to return the results,;
// # one result per col;
// #;
// #numsim number of simulations to be run;
// #;
// #nrowr number of rows being returned in retmat;
// #;
// #tspass(33) should be able to eliminate;
// #;
// #lrand should be able to eliminate;
// [[Rcpp::export]]
Rcpp::List mlsim3(Rcpp::NumericMatrix y,
                  Rcpp::IntegerVector cen,
                  Rcpp::IntegerVector wt,
                  int nrow,
                  int ny,
                  int nty,
                  Rcpp::NumericMatrix ty,
                  Rcpp::IntegerVector tcodes,
                  Rcpp::NumericVector gamthr,
                  int maxit,
                  int kprint,
                  Rcpp::NumericVector dscrat,
                  Rcpp::IntegerVector iscrat,
                  Rcpp::NumericVector scrat,
                  Rcpp::NumericVector p,
                  Rcpp::NumericVector q,
                  Rcpp::NumericVector prob,
                  Rcpp::NumericVector sd,
                  int m,
                  Rcpp::NumericVector pnew,
                  Rcpp::NumericVector qnew,
                  Rcpp::NumericVector prbnew,
                  Rcpp::NumericVector sdnew,
                  Rcpp::IntegerVector iarray,
                  int marray,
                  Rcpp::IntegerVector wtnew,
                  Rcpp::NumericMatrix ynew,
                  Rcpp::NumericMatrix retmat,
                  int numsim,
                  int numret,
                  Rcpp::NumericVector tspass,
                  bool lrand,
                  int iersim){

debug::kprint = kprint;
  
bool lcheck,lsd;
int kount, method = 0,nstart, ier = 0;
int maxmsd, mnew, nrowr;
double tol,pchmax;
Rcpp::NumericVector prtvec,srtvec;
Rcpp::IntegerVector ilcv,iucv,iltv,iutv,iorder;
Rcpp::NumericVector xlcen,xrcen,fail,xltru,xrtru,ys;
Rcpp::NumericVector pgrad,s,probd,fscrat;

if(debug::kprint >= 2) {
  
   Rcpp::Rcout << "y =\n" << y << std::endl;
   Rcpp::Rcout << "cen = " << cen << std::endl;
   Rcpp::Rcout << "wt = " << wt << std::endl;
   Rcpp::Rcout << "nrow = " << nrow << std::endl;
   Rcpp::Rcout << "ny = " << ny << std::endl;
   Rcpp::Rcout << "nty = " << nty << std::endl;
   Rcpp::Rcout << "ty =\n" << ty << std::endl;
   Rcpp::Rcout << "tcodes = " << tcodes << std::endl;
   Rcpp::Rcout << "gamthr = " << gamthr << std::endl;
   Rcpp::Rcout << "maxit = " << maxit << std::endl;
   Rcpp::Rcout << "kprint = " << kprint << std::endl;
   Rcpp::Rcout << "numsim = " << numsim << std::endl;
   Rcpp::Rcout << "numret = " << numret << std::endl;
   Rcpp::Rcout << "iersim = " << iersim << std::endl;

}

// Program to simulate survival data
// Fix up things to keep this version simple

// Setup generator
   snset(wt,nrow,kount,method,iarray,marray,iersim);

if(iersim > 2000) goto exit;

for(int isim = 1; isim <= numsim; isim++){

// Print progress information
   if((debug::kprint > 0) & ((isim % 50) == 1)) {
     
       Rcpp::Rcout << "\nSimulation number = " << isim << std::endl;
       
     }

// Simulate the data with new weights
   simnp(iarray,marray,method,kount,wtnew,nrow);
   
if(debug::kprint >= 11){
  
   Rcpp::Rcout << "\nbeginning simulation = " << isim << std::endl;
   Rcpp::Rcout << "wtnew = " << wtnew << std::endl;
   
}

// Copy over y (because it may have been destroyed)
   ynew = clone(y);

if(debug::kprint >= 2) {
  
   Rcpp::Rcout << "y =\n" << y << std::endl;
   Rcpp::Rcout << "cen = " << cen << std::endl;
   Rcpp::Rcout << "wt = " << wt << std::endl;
   Rcpp::Rcout << "nrow = " << nrow << std::endl;
   Rcpp::Rcout << "ny = " << ny << std::endl;
   Rcpp::Rcout << "nty = " << nty << std::endl;
   Rcpp::Rcout << "ty = " << ty << std::endl;
   Rcpp::Rcout << "tcodes = " << tcodes << std::endl;
   Rcpp::Rcout << "gamthr = " << gamthr << std::endl;
   Rcpp::Rcout << "maxit = " << maxit << std::endl;
   Rcpp::Rcout << "kprint = " << kprint << std::endl;

}

tol = 0.001;
nstart = 0;

ilcv   = Rcpp::IntegerVector(nrow);  
iucv   = Rcpp::IntegerVector(nrow);  
iltv   = Rcpp::IntegerVector(nrow);  
iutv   = Rcpp::IntegerVector(nrow);  
iorder = Rcpp::IntegerVector(2 * nrow);

xlcen = Rcpp::NumericVector(nrow);
xrcen = Rcpp::NumericVector(nrow);
fail  = Rcpp::NumericVector(nrow);
xltru = Rcpp::NumericVector(nrow);
xrtru = Rcpp::NumericVector(nrow);
ys    = Rcpp::NumericVector(2 * nrow); 
pgrad = Rcpp::NumericVector(nrow);
s     = Rcpp::NumericVector(nrow);
probd = Rcpp::NumericVector(nrow);
fscrat = Rcpp::NumericVector(nrow);

mnew = 0;

 wqm_cdfest(ynew,ny,cen,wtnew,ty,nty,tcodes,nrow,
            nstart,dscrat,scrat,iscrat,maxit,tol,
            maxmsd,pnew,qnew,prbnew,sdnew,mnew,
            pchmax,lsd,ier,ilcv,iucv,iltv,
            iutv,iorder,xlcen,xrcen,fail,xltru,xrtru,ys,
            pgrad,s,probd,fscrat);

// Save the results
// always save error and redistributed prbnew, sdnew;
   retmat.at(0,isim - 1) = ier;

   Rcpp::Rcout << "Here" << std::endl;
   Rcpp::Rcout << "mnew = " << mnew << std::endl;
// Match and redistributed prbnew, sdnew
   prtvec = Rcpp::NumericVector(m);
   srtvec = Rcpp::NumericVector(m);
   
   if(debug::kprint > 8) {
      
      Rcpp::Rcout << "prtvec = " << prtvec << std::endl;
      Rcpp::Rcout << "srtvec = " << srtvec << std::endl;
      
   }
   
    mthcdf(p,q,prob,sd,m,pnew,qnew,prbnew,
           sdnew,mnew,prtvec,srtvec);
   
   for(int k = 1; k <= m; k++){
      
       retmat.at(    k, isim - 1) = prtvec.at(k - 1);
       retmat.at(m + k, isim - 1) = srtvec.at(k - 1);
      
   }
   
   nrowr = 2 * m + 1;

}

if(numret != nrowr) iersim = 4;

if(debug::kprint >= 2) {
  
   Rcpp::Rcout << "final iersim = " << iersim << std::endl;
  
}

exit:

Rcpp::List ints = Rcpp::List::create(Named("nrow") = nrow,
                                     Named("nrowr") = nrowr,
                                     Named("ny") = ny,
                                     Named("nty") = nty,
                                     Named("maxit") = maxit,
                                     Named("kprint") = kprint,
                                     Named("marray") = marray,
                                     Named("numsim,") = numsim,
                                     Named("numret") = numret,
                                     Named("iersim") = iersim,
                                     Named("method") = method);

Rcpp::List doubs = Rcpp::List::create(Named("pchmax") = pchmax);

Rcpp::List bools = Rcpp::List::create(Named("lrand")  = lrand,
                                      Named("lcheck") = lcheck,
                                      Named("lsd")    = lsd);

Rcpp::List intvec = Rcpp::List::create(Named("cen") = cen,
                                       Named("wt") = wt,
                                       Named("tcodes") = tcodes,
                                       Named("iscrat") = iscrat,
                                       Named("iarray") = iarray,
                                       Named("wtnew") = wtnew);

Rcpp::List numvec = Rcpp::List::create(Named("gamthr") = gamthr,
                                       Named("dscrat") = dscrat,
                                       Named("tspass") = tspass);

Rcpp::List nummat = Rcpp::List::create(Named("y") = y,
                                       Named("ty") = ty,
                                       Named("ynew") = ynew,
                                       Named("retmat") = retmat);

return Rcpp::List::create(Named("ints") = ints,
                          Named("doubs") = doubs,
                          Named("bools") = bools,
                          Named("intvec") = intvec,
                          Named("numvec") = numvec,
                          Named("nummat") = nummat);


}

/*** R
library(smrdfortran)
test = 2
if(test == 1) {
data.ld <- frame.to.ld(heatexchanger,
                       response.column = c(1,2),
                       censor.column = 3,
                       case.weight.column = 4)
}
if(test == 2) data.ld <- frame.to.ld(lzbearing, response.column = 1)
if(test == 3) {
  
   data.ld <- frame.to.ld(superalloy,
                          response.column = 1,
                          censor.column = 2,
                          case.weight.column = 3)
   
}
if(test == 4) {
  
data.ld <- frame.to.ld(smrdfortran::doatrun,
                       response.column = c(1,2),
                       censor.column = 3,
                       case.weight.column = 4,
                       truncation.response.column = 5,
                       truncation.type.column = 6)

}

number.sim = 2000
kprint = 0 
maxit = 500 
max.sim.scratch.space = 1000
maxmsd = 100
debug1 = F
randomize = !T 
  
    `if`(randomize,
         tspass <- runif(33),
         tspass <- seq(0.1, 0.4, length = 33))
    
    nty <- 0
    the.censor.codes <- smrdfortran:::censor.codes(data.ld)
    the.case.weights <- smrdfortran:::case.weights(data.ld)
    y <-Response(data.ld)
    ny <- ncol(y)
    number.cases <- length(the.case.weights)
    ndscrat <- 3 * number.cases + 4
    niscrat <- 6 * number.cases + 7
    nrscrat <- max(7 * (number.cases + 1), (maxmsd * (maxmsd - 1))/2 + 1)
    sim.scratch.space <- min(sum(the.case.weights), 
                             max(max.sim.scratch.space, number.cases))
    cdfest.out <- cdfest(data.ld)
    m <- length(cdfest.out$p)
    number.things.returned <- 2 * m + 1
    if (debug1) browser()
    zout <- .Fortran("mlsim3", 
                     as.single(y), 
                     as.single(the.censor.codes), 
                     as.single(the.case.weights), 
                     as.integer(number.cases), 
                     as.integer(ny), 
                     as.integer(nty), 
                     ty = single(number.cases), 
                     tc = single(number.cases), 
                     gamthr = single(number.cases), 
                     as.integer(maxit), 
                     as.integer(kprint), 
                     double(ndscrat), 
                     integer(niscrat), 
                     single(nrscrat), 
                     as.single(cdfest.out$p), 
                     as.single(cdfest.out$q), 
                     single(m), 
                     single(m), 
                     as.integer(m), 
                     single(m), 
                     single(m), 
                     single(m), 
                     single(m), 
                     integer(sim.scratch.space), 
                     as.integer(sim.scratch.space),
                     wtnew = single(number.cases), 
                     ynew = single(number.cases * ny), 
                     return.matrix = single(number.sim * number.things.returned), 
                     as.integer(number.sim), 
                     as.integer(number.things.returned), 
                     as.single(tspass), 
                     as.logical(randomize), 
                     iersim = integer(1))

new = wqmmlesss::mlsim3(y, 
                        the.censor.codes, 
                        the.case.weights,
                        number.cases, 
                        ny, 
                        nty, 
                        matrix(0, nrow = number.cases, ncol = 1), 
                        integer(number.cases), 
                     gamthr = single(number.cases), 
                     as.integer(maxit), 
                     as.integer(kprint), 
                     double(ndscrat), 
                     integer(niscrat), 
                     double(nrscrat), 
                     as.double(cdfest.out$p), 
                     as.double(cdfest.out$q), 
                     double(m + 1), 
                     double(m + 1), 
                     as.integer(m), 
                     double(m + 1), 
                     double(m + 1), 
                     double(m + 1), 
                     double(m + 1), 
                     integer(sim.scratch.space), 
                     as.integer(sim.scratch.space),
                     wtnew = double(number.cases), 
                     ynew  = matrix(3,nrow = number.cases, ncol = ny), 
                     retmat = matrix(4, ncol = number.sim, nrow =  number.things.returned), 
                     as.integer(number.sim), 
                     as.integer(number.things.returned), 
                     as.double(tspass), 
                     as.logical(randomize), 
                     iersim = integer(1))

old.return.matrix <- t(matrix(zout$return.matrix, nrow = number.things.returned))
new.return.matrix <- t(new$nummat$retmat)
old.f.hat.star <- old.return.matrix[, 2:(m + 1)]
new.f.hat.star <- new.return.matrix[, 2:(m + 1)]
old.standard.errors <- old.return.matrix[, (m + 2):(2 * m + 1)]
new.standard.errors <- new.return.matrix[, (m + 2):(2 * m + 1)]
old.ierstuff <- as.integer(old.return.matrix[, 1])
new.ierstuff <- as.integer(new.return.matrix[, 1])
old.results <- list(cdfest.out = cdfest.out, 
                    p = cdfest.out$p, 
                    q = cdfest.out$q, 
                    f.hat = cdfest.out$prob, 
                    sd = cdfest.out$sd, 
                    f.hat.star = old.f.hat.star, 
                    stderror.star = old.standard.errors, 
                    ierstuff = old.ierstuff)
new.results <- list(cdfest.out = cdfest.out, 
                    p = cdfest.out$p, 
                    q = cdfest.out$q, 
                    f.hat = cdfest.out$prob, 
                    sd = cdfest.out$sd, 
                    f.hat.star = new.f.hat.star, 
                    stderror.star = new.standard.errors, 
                    ierstuff = new.ierstuff)

oldClass(new.results) <- "boot.npar.npar.out"

par(mfrow = c(1,2))
smrdfortran:::plot.boot.npar.npar.out(old.results)
smrdfortran:::plot.boot.npar.npar.out(new.results)
par(mfrow = c(1,1))

*/