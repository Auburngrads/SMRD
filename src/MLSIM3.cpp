#include <base/base.hpp>
#include <mlsim2/mthcdf.hpp>
#include <mlsim2/simnp.hpp>
#include <mlsim2/snset.hpp>
#include <wqmcdfest/wqm_cdfest.hpp>

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
Rcpp::List MLSIM3(Rcpp::NumericMatrix y,
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
            pgrad,s,probd);

// Save the results
// always save error and redistributed prbnew, sdnew;
   retmat.at(0,isim - 1) = ier;

// Match and redistributed prbnew, sdnew
   prtvec = Rcpp::NumericVector(m);
   srtvec = Rcpp::NumericVector(m);
   
   if(debug::kprint > 8) {
      
      Rcpp::Rcout << "  mnew = " << mnew << std::endl;
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