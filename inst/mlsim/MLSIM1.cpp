#include <base/base.h>
#include <wqmmlesss/wqm_mlboth.h>
#include <mlsim2/smldat.h>

// # method 1;
// #;
// # parametric parametric;
// #;
// #method 1 simulation---parametric (spec censoring);
// # sampling/parametric inference;
// #;
// # parameter meanings same as in wqm_mlboth except the following:;
// #;
// #;
// #theta(nparm) true value of theta;
// #;
// #thetah(nparm) space for thetahat;
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
// #retmat(numret,numsim) matrix to return the results, one result per col;
// #;
// #numsim number of simulations to be run;
// #;
// #nrowr number of rows being returned in retmat;
// #;
// #tspass(33) should be able to eliminate;
// #;
// #lrand should be able to eliminate;
// #;
// #;
// #iersim data space too small;
// #;
// # need to send down data space big enough to cover all data situations;

Rcpp::List mlsim1(Rcpp::NumericMatrix x,
                  Rcpp::NumericMatrix y,
                  Rcpp::IntegerVector cen,
                  Rcpp::IntegerVector wt,
                  int nrow,
                  int nter,
                  int ny,
                  int nty,
                  Rcpp::NumericMatrix ty,
                  Rcpp::IntegerVector tcodes,
                  int kdist,
                  Rcpp::NumericVector gamthr,
                  Rcpp::LogicalVector lfix,
                  int nparm,
                  int intcpt,
                  double escale,
                  Rcpp::NumericVector e,
                  int maxit,
                  int kprint,
                  Rcpp::NumericVector dscrat,
                  Rcpp::IntegerVector iscrat,
                  Rcpp::NumericMatrix devian,
                  Rcpp::NumericVector thetah,
                  Rcpp::NumericVector fsder,
                  Rcpp::NumericMatrix vcv,
                  Rcpp::NumericMatrix r,
                  Rcpp::NumericMatrix res,
                  Rcpp::NumericVector fv,
                  Rcpp::NumericVector theta,
                  Rcpp::IntegerVector iarray,
                  int marray,
                  Rcpp::IntegerVector wtnew,
                  Rcpp::NumericMatrix xnew,
                  Rcpp::NumericMatrix ynew,
                  int centim,
                  Rcpp::NumericMatrix retmat,
                  int numsim,
                  int numret,
                  Rcpp::NumericVector tspass,
                  bool lrand,
                  int iersim){

int nsamsz, ii,nrowr,nrownw,krfail,kpred; 
double pretim;
Rcpp::NumericMatrix ipxnew,iptmat,ipvcvb,ipvcvg,ivcvd,ivcvdd;
Rcpp::NumericVector iprv1,ipdiag,ipthb,ipthg,ipfsd,ipnext;
Rcpp::NumericVector itd,itf,ied,iw,ivd;
Rcpp::IntegerVector iir, ijc;


if(debug::kprint > 2) {
  
   Rcpp::Rcout << "x = "         << x << std::endl;
   Rcpp::Rcout << "y = "         << y << std::endl;
   Rcpp::Rcout << "cen ="        << cen <<  std::endl;
   Rcpp::Rcout << "weights = "   << wt << std::endl;
   Rcpp::Rcout << "nrow = "      << nrow << std::endl;
   Rcpp::Rcout << "nter = "      << nter << std::endl;
   Rcpp::Rcout << "ny = "        << ny << std::endl;
   Rcpp::Rcout << "nty = "       << nty << std::endl;
   Rcpp::Rcout << "ty = "        << ty << std::endl;
   Rcpp::Rcout << "tcodes = "    << tcodes << std::endl;
   Rcpp::Rcout << "kdist = "       << kdist << std::endl;
   Rcpp::Rcout << "gamthr = "    << gamthr << std::endl;
   Rcpp::Rcout << "lfix = "      << lfix << std::endl;
   Rcpp::Rcout << "nparm = "     << nparm << std::endl;
   Rcpp::Rcout << "intcpt = "    << intcpt << std::endl;
   Rcpp::Rcout << "escale = "    << escale << std::endl;
   Rcpp::Rcout << "e = "         << e << std::endl;
   Rcpp::Rcout << "maxit = "     << maxit << std::endl;
   Rcpp::Rcout << "kprint = "    << kprint << std::endl;
   Rcpp::Rcout << "dscrat = "    << dscrat << std::endl;
   Rcpp::Rcout << "iscrat = "    << iscrat << std::endl;
   Rcpp::Rcout << "devian = "    << devian << std::endl;
   Rcpp::Rcout << "thetahat = "  << thetah << std::endl;
   Rcpp::Rcout << "fsder = "     << fsder << std::endl;
   Rcpp::Rcout << "vcv = "       << vcv <<std::endl;
   Rcpp::Rcout << "r = "         << r << std::endl;
   Rcpp::Rcout << "res = "       << res << std::endl;
   Rcpp::Rcout << "fv = "        << fv << std::endl;
   Rcpp::Rcout << "thetareal = " << thetah << std::endl;
   Rcpp::Rcout << "nsamsz = "    << nsamsz << std::endl;
   Rcpp::Rcout << "centim = "    << centim << std::endl;
   Rcpp::Rcout << "nsamsz = "    << nsamsz << std::endl;
   Rcpp::Rcout << "numsim = "    << numsim << std::endl;
   Rcpp::Rcout << "numret = "    << numret << std::endl;
   Rcpp::Rcout << "iersim = "    << iersim << std::endl;

}

int wtsum = 0;

for(int i = 0; i < nrow; i++){
  
    if(cen.at(i) > 0) wtsum = wtsum + wt.at(i);

}

pretim = centim;
nsamsz = wtsum;

// Program to simulate survival data
// First cut is single distribution only

// Need to initialize the random number generator above for randomization
// Fix up things to keep this version simple
bool lcheck = false;
int nvcv = (nparm) * (nparm + 1) / 2;

// Simulate the data and count number failing in intervals
// kctype = 1 for type 1 censored data

int kctype = 1;

for(int isim = 1; isim <= numsim; isim++){

// Print progress information

if(debug::kprint > 1) {
  
   if(isim % 50 == 1) {
     
      Rcpp::Rcout << "Beginning simulation number = " << isim << std::endl;
     
   }

}

smldat(theta,nparm,nsamsz,kctype,centim,pretim,kdist,
       x,y,cen,wt,nrow,nter,ny,nty,ty,tcodes,nrownw,
       krfail,igroup,iobs,kpred,iersim);

//xx call simpar(theta,nparm,nsamsz,centim,pretim,kdist,;
//xx & x,y,cen,wt,nrow,nter,ny,nty,ty,tcodes,;
//xx & nrownw,krfail,kpred,iersim);

// Copy over x and y (because they may have been destroyed)
xnew = clone(x);
ynew = clone(y);

// Get start values from true parameters
for(int i = 0; i < nparm; i++){
  
    thetah.at(i) = theta.at(i);
  
}

// should give this a junk number below too, probably;
   double xlike = 7777.0;
   int iervcv = 0;
   int ierfit = 0;
   ipxnew = Rcpp::NumericMatrix(nrow, nter);
   iprv1 = Rcpp::NumericVector(nparm);
   ipdiag = Rcpp::NumericVector(nparm);
   iptmat = Rcpp::NumericMatrix(nparm, nparm);
   ipthb = Rcpp::NumericVector(nparm);
   ipthg = Rcpp::NumericVector(nparm);
   ipfsd = Rcpp::NumericVector(nparm);
   ipvcvb = Rcpp::NumericMatrix(nparm, nparm);
   ipvcvg = Rcpp::NumericMatrix(nparm, nparm);
   ipnext = Rcpp::NumericVector(nparm);
   itd = Rcpp::NumericVector(nparm);
   itf = Rcpp::NumericVector(nparm);
   ied = Rcpp::NumericVector(nparm);
   iw = Rcpp::NumericVector(nparm * nparm + 3 * nparm);
   ivd = Rcpp::NumericVector(nparm);
   ivcvd = Rcpp::NumericMatrix(nparm, nparm);
   ivcvdd = Rcpp::NumericMatrix(nparm + 1, nparm + 1);
   iir = Rcpp::IntegerVector(nparm + 1);
   ijc = Rcpp::IntegerVector(nparm + 1);

   wqm_mlboth(x,y,cen,wt,nrownw,nter,ny,nty,ty,tcodes,
              kdist,gamthr,lfix,lcheck,nparm,intcpt,escale,
              e,maxit,dscrat,iscrat,xlike,devian,
              thetah,fsder,vcv,r,res,fv,ierfit,iervcv,
              ipxnew,iprv1,ipdiag,iptmat,
              ipthb,ipthg,ipfsd,ipvcvb,ipvcvg,ipnext,itd,itf,
              ied,iw,ivd,ivcvd,ivcvdd,iir,ijc);
   
if(debug::kprint >= 3) {
  
   Rcpp::Rcout << "xnew = "   << xnew << std::endl;
   Rcpp::Rcout << "ynew = "   << ynew << std::endl;
   Rcpp::Rcout << "cen "      << cen << std::endl;
   Rcpp::Rcout << "wtnew = "  << wtnew << std::endl;
   Rcpp::Rcout << "nrow = "   << nrow << std::endl;
   Rcpp::Rcout << "nter = "   << nter << std::endl;
   Rcpp::Rcout << "ny = "     << ny << std::endl;
   Rcpp::Rcout << "nty = "    << nty << std::endl;
   Rcpp::Rcout << "ty = "     << ty << std::endl;
   Rcpp::Rcout << "tcodes = " << tcodes << std::endl;
   Rcpp::Rcout << "kdist = "  << kdist << std::endl;
   Rcpp::Rcout << "gamthr = " << gamthr << std::endl;
   Rcpp::Rcout << "lfix = "   << lfix << std::endl;
   Rcpp::Rcout << "lcheck = " << lcheck << std::endl;
   Rcpp::Rcout << "nparm = "  << nparm << std::endl;
   Rcpp::Rcout << "intcpt = " << intcpt << std::endl;
   Rcpp::Rcout << "escale = " << escale << std::endl;
   Rcpp::Rcout << "e = "      << e << std::endl;
   Rcpp::Rcout << "maxit = "  << maxit << std::endl;
   Rcpp::Rcout << "kprint = " << kprint << std::endl;
   Rcpp::Rcout << "dscrat = " << dscrat << std::endl;
   Rcpp::Rcout << "iscrat = " << iscrat << std::endl;
   Rcpp::Rcout << "xlike = "  << xlike << std::endl;
   Rcpp::Rcout << "devian = " << devian << std::endl;
   Rcpp::Rcout << "theta = "  << theta << std::endl;
   Rcpp::Rcout << "fsder = "  << fsder << std::endl;
   Rcpp::Rcout << "vcv = "    << vcv << std::endl;
   Rcpp::Rcout << "r = "      << r << std::endl;
   Rcpp::Rcout << "res = "    << res << std::endl;
   Rcpp::Rcout << "fv = "     << fv << std::endl;
   Rcpp::Rcout << "ierfit = " << ierfit << std::endl;
   Rcpp::Rcout << "iervcv = " << iervcv << std::endl;

}


// Save the results
// Always save thetah and error
   retmat.at(0,isim - 1) = 10 * iervcv + ierfit;
   
   for(int i = 1; i <= nparm; i++){
      
       retmat.at(i,isim - 1) = thetah.at(i - 1);
      
   }

// now check to see what else to save;
retmat.at((nparm + 2 - 1),isim - 1) = xlike;
ii = nparm + 2;

for(int iii = 1; iii <= nparm; iii++){
  
    for(int jjj = 1; jjj <= iii; jjj++){
      
        ii = ii + 1;
        retmat.at(ii - 1,isim - 1) = vcv.at(iii - 1,jjj - 1);
        
    }
}

}

if(debug::kprint >= 2) {
  
   Rcpp::Rcout << "iersim = " << iersim << std::endl;
   
}

// Record the number of rows coming back
   nrowr = nparm + (nparm) * (nparm + 1) / 2 + 2;

   if(numret != nrowr) iersim = 4;

return Rcpp::List::create(Named("nrowr") = nrowr);

}

/*** R
data.ld 
distribution 
number.sim
escale = 10000 
intercept = T
kprint = 0
maxit = 500 
max.sim.scratch.space = 1000
debug1 = F
randomize = T 
    
    `if`(randomize,
         tspass <- runif(33),
         tspass <- seq(0.1, 0.4, length = 33))
    
    the.censor.codes <- censor.codes(data.ld)
    the.case.weights <- case.weights(data.ld)
    nty <- 0
    nter <- 1
    int <- 1
    mlest.out <- mlest(data.ld, 
                       distribution = distribution, 
                       kprint = kprint)
    
    theta.start <- mlest.out$theta.hat
    theta.hat <- theta.start
    distribution.number <- numdist(distribution)
    param.names <- c("mu", "sigma")
    number.parameters <- 2
    
    if (generic.distribution(distribution) == "exponential") {
      
       distribution.number <- 2
       parameter.fixed[number.parameters] <- T
       number.parametersx <- 1
       
    }
    iret <- 3
    number.things.returned <- number.parameters + ((number.parameters) * 
                                                     (number.parameters + 1))/2 + 2
    y <-Response(data.ld)
    ny <- ncol(y)
    number.cases <- length(the.case.weights)
    the.xmat <- matrix(1, nrow = number.cases, ncol = 1)
    ndscrat <- number.parameters * 
               number.cases + 5 * 
               number.parameters *
               number.parameters + 12 * 
               number.parameters + 1
    niscrat <- 2 * (number.parameters + 1)
    sim.scratch.space <- min(sum(the.case.weights), 
                             max(max.sim.scratch.space, number.cases))
    
e = rep(1e-04, number.parameters)
parameter.fixed = rep(F, number.parameters)
    
    if (debug1) browser()
    
    zout <- .Fortran("mlsim2", 
                     as.single(the.xmat), 
                     as.single(y), 
                     as.single(the.censor.codes), 
                     as.single(the.case.weights), 
                     as.integer(number.cases), 
                     as.integer(nter), 
                     as.integer(ny), 
                     as.integer(nty), 
                     ty = single(number.cases), 
                     tc = single(number.cases), 
                     distribution.number = as.integer(distribution.number), 
                     gamthr = single(number.cases), 
                     parameter.fixed = as.logical(parameter.fixed), 
                     number.parameters = as.integer(number.parameters), int = as.integer(int), 
                     escale = as.single(escale), 
                     e = as.single(e), 
                     maxit = as.integer(maxit), 
                     kprint = as.integer(kprint), 
                     dscrat = double(ndscrat), 
                     iscrat = integer(niscrat), 
                     devian = single(number.cases * 3), 
                     thetah = as.single(theta.start), 
                     first.derivative = single(number.parameters), 
                     vcv.matrix = single(number.parameters * number.parameters), 
                     correlation.matrix = single(number.parameters * number.parameters), 
                     residuals = single(ny * number.cases), 
                     fitted.values = single(ny * number.cases), 
                     theta = single(number.parameters), 
                     iarray = integer(sim.scratch.space), 
                     marray = as.integer(sim.scratch.space), 
                     wtnew = single(number.cases), 
                     xnew = single(number.cases * nter), 
                     ynew = single(number.cases * ny), 
                     iret = as.integer(iret), 
                     return.matrix = single(number.sim * number.things.returned), 
                     number.sim = as.integer(number.sim), 
                     numret = as.integer(number.things.returned), 
                     tspass = as.single(tspass), 
                     lrand = as.logical(randomize), 
                     iersim = integer(1))

*/