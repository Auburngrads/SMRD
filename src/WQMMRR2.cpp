#include <base/base.hpp>
#include <wqmmrr2/mrr2.hpp>

// [[Rcpp::export]]
Rcpp::List WQMMRR2(Rcpp::NumericMatrix y,
                   Rcpp::IntegerVector cen,
                   Rcpp::IntegerVector wt,
                   Rcpp::IntegerVector iscrat,
                   Rcpp::NumericVector rscrat,
                   int nrownw,
                   Rcpp::NumericVector thetamrr){
  
Rcpp::IntegerVector ikey      = Rcpp::IntegerVector(nrownw);
Rcpp::IntegerVector ikrevrank = Rcpp::IntegerVector(nrownw);
Rcpp::NumericVector iynew     = Rcpp::NumericVector(nrownw);
Rcpp::NumericVector irmdrank  = Rcpp::NumericVector(nrownw);
Rcpp::IntegerVector icenstar  = Rcpp::IntegerVector(nrownw);
Rcpp::NumericVector ixstar    = Rcpp::NumericVector(nrownw);
Rcpp::NumericVector ix        = Rcpp::NumericVector(nrownw);
Rcpp::IntegerVector irorder   = Rcpp::IntegerVector(nrownw);
Rcpp::IntegerVector icenstarn = Rcpp::IntegerVector(nrownw);
Rcpp::IntegerVector iwtnew    = Rcpp::IntegerVector(nrownw);

  mrr2(y,cen,wt,iscrat,rscrat,nrownw,thetamrr,ikey,
       ikrevrank,iynew,irmdrank,icenstar,ixstar,ix,
       irorder,icenstarn,iwtnew);

  return Rcpp::List::create(Named("y") = y,
                            Named("cen") = cen,
                            Named("wt") = wt,
                            Named("iscrat") = iscrat,
                            Named("rscrat") = rscrat,
                            Named("nrownw") = nrownw,
                            Named("thetamrr") = thetamrr,
                            Named("ikey") = ikey,
                            Named("ikrevrank") = ikrevrank,
                            Named("iynew") = iynew,
                            Named("irmdrank") = irmdrank,
                            Named("icenstar") = icenstar,
                            Named("ixstar") = ixstar,
                            Named("ix") = ix,
                            Named("irorder") = irorder,
                            Named("icenstarn") = icenstarn,
                            Named("iwtnew") = iwtnew);
      
}

#include <base/base.hpp>
#include <wqmmrr2/mrr.hpp>

// initial statements
// specification statements
//
// y    vector of failure/right censoring times
//
// ynew   vector of ordered failure/rightcensoring times (log(y))
//
// cen  cen=1 indicates failure time, cen=2 right censored observation
//
// censtar  modified cen for comparisons
//
// censtarn  ordered vector of rcenstar
//
// wt    vector of weights of single failure/right censoring times
//
// the weight for a failure time is always assumed to be 1 due to
//
//
// iscrat must have length   integer scratch 2*nrownw
//
// rscrat must have length   real scratch 8*nrownw
//
// simulation techniques using order statistics, censoring times
//
// are allowed weights greater than 1
//
// wtnew  ordered vector of weights
//
// key    a vector of integers returning the order vector
//
// krevrank vector of reverse ranks of y
//
// rorder  vector of adjusted order numbers computed for failure
//
// times  only,  neccessary for computing median ranks
//
// rmdrank  vector of median ranks computed for failure times
//
// xstar vector  of plotting positions
//
// x  vector of log(log(rxstar)) needed to  perform regression

void mrr2(Rcpp::NumericVector &y,
          Rcpp::IntegerVector &cen,
          Rcpp::IntegerVector &wt,
          Rcpp::IntegerVector &iscrat,
          Rcpp::NumericVector &rscrat,
          int &nrownw,
          Rcpp::NumericVector &thetamrr,
          Rcpp::IntegerVector &ikey,
          Rcpp::IntegerVector &ikrevrank,
          Rcpp::NumericVector &iynew,
          Rcpp::NumericVector &irmdrank,
          Rcpp::IntegerVector &icenstar,
          Rcpp::NumericVector &ixstar,
          Rcpp::NumericVector &ix,
          Rcpp::IntegerVector &irorder,
          Rcpp::IntegerVector &icenstarn,
          Rcpp::IntegerVector &iwtnew) {
          
  mrr(ikey,ikrevrank,y,iynew,cen,
      irmdrank,wt,icenstar,ixstar,
      ix,irorder,icenstarn,iwtnew,
      nrownw,thetamrr);

  return;
      
}

#include <base/base.hpp>
#include <utility/wqm_sortd.hpp>

// specification statements
//
// y    vector of failure/right censoring times
//
// ynew   vector of ordered failure/rightcensoring times (log(y))
//
// cen  cen=1 indicates failure time, cen=2 right censored observation
//
// censtar  modified cen for comparisons
//
// censtarn  ordered vector of rcenstar
//
// wt    vector of weights of single failure/right censoring times
//
// the weight for a failure time is always assumed to be 1 due to
//
// simulation techniques using order statistics, censoring times
//
// are allowed weights greater than 1
//
// wtnew  ordered vector of weights
//
// key    a vector of integers returning the order vector
//
// krevrank vector of reverse ranks of y
//
// rorder  vector of adjusted order numbers computed for failure
//
// times  only,  neccessary for computing median ranks
//
// rmdrank  vector of median ranks computed for failure times
//
// xstar vector  of plotting positions
//
// x  vector of log(log(rxstar)) needed to  perform regression

void mrr(Rcpp::IntegerVector &key, 
         Rcpp::IntegerVector &krevrank, 
         Rcpp::NumericVector &y, 
         Rcpp::NumericVector &ynew,
         Rcpp::IntegerVector &cen,
         Rcpp::NumericVector &rmdrank,
         Rcpp::IntegerVector &wt,
         Rcpp::IntegerVector &censtar,
         Rcpp::NumericVector &xstar,
         Rcpp::NumericVector &x,
         Rcpp::IntegerVector &rorder,
         Rcpp::IntegerVector &censtarn, 
         Rcpp::IntegerVector &wtnew, 
         int &nrownw,
         Rcpp::NumericVector &thetamrr){

double csumx = 0.0;
double csumxy = 0.0;
int sumc = 0,sumw = 0,sumww = 0,isub;
double sumx = 0.0,sumy = 0.0;
double xbar, ybar;
double d = 0.0;
double betastar,alphastar,beta,eta;

// kprint=-9
// Count number of failures assuming that 'cen' is 1-2 variable,
// 1-failure, 2-right censoring
   for(int j = 1; j <= nrownw; j++){
   
       censtar.at(j - 1) = cen.at(j - 1);
      
   }
        
   if(debug::kprint >= 1) {
   
      Rcpp::Rcout << "\nCENSTAR:\n" << std::endl;
      Rcpp::Rcout << "censtar = " << censtar << std::endl;
   
   } 
   
   for(int j = 1; j <= nrownw; j++){
   
       sumc = sumc + std::abs(cen.at(j - 1) - 2);
   
   }
        
   if(debug::kprint >= 1) {
   
      Rcpp::Rcout << "\nSUMC:\n" << std::endl;
      Rcpp::Rcout << "sumc = " << sumc << std::endl;
   
   } 

// If data contain less than 2 failures then return
   if(sumc < 2) return;

// Compute total number of observations in simulation
   for(int j = 1; j <= nrownw; j++){
   
       if(cen.at(j - 1) > 1.5) y.at(j - 1) = y.at(j - 1) * 1.00001;
       sumw = sumw + wt.at(j - 1);
      
   }
        
   if(debug::kprint >= 1) {
   
      Rcpp::Rcout << "\nSUMW:\n" << std::endl;
      Rcpp::Rcout << "sumw = " << sumw << std::endl;
   
   } 

// Order data using subroutine 'sortrr' from smallest to largest,
   wqm_sortd(y,nrownw,key);

// call ord(y,nrownw,key);
// call sort(y,key,nrownw);

for(int j = 1; j <= nrownw; j++){

    isub = key.at(j - 1);
    ynew.at(j - 1) = y.at(isub - 1);
    censtarn.at(j - 1) = censtar.at(isub - 1);
    wtnew.at(j - 1) = wt.at(isub - 1);
      
}
        
   if(debug::kprint >= 1) {
   
      Rcpp::Rcout << "\nKEY:\n"                  << std::endl;
      Rcpp::Rcout << "key = "        << key      << std::endl;
      Rcpp::Rcout << "\nYNEW:\n"                 << std::endl;
      Rcpp::Rcout << "ynew = "       << ynew     << std::endl;
      Rcpp::Rcout << "\nCENSTARN:\n"             << std::endl;
      Rcpp::Rcout << "censtarn = "   << censtarn << std::endl;
      Rcpp::Rcout << "\nWTNEW:\n"                << std::endl;
      Rcpp::Rcout << "wtnew = "      << wtnew    << std::endl;

   
   } 

// Obtain reverse ranks to get order number for median ranks
   for(int j = 1; j <= nrownw; j++){
   
       for(int i = 1; i <= j; i++){
       
           sumww = sumww + wtnew.at(i - 1);
       
       }
       
       krevrank.at(j - 1) = sumw - sumww + 1;
       sumww = 0;
       
   }

   if(debug::kprint >= 1) {
   
      Rcpp::Rcout << "\nREVERSE RANKS\n" << std::endl;
      Rcpp::Rcout << "krevrank = " << krevrank << std::endl;
   
   } 
        
// Obtain order number, assuming '2' indicates censoring
   for(int j = 1; j <= nrownw; j++){
        
       if(censtarn.at(j - 1) > 1.5) {
       
          rorder.at(j - 1) = 0;
          rmdrank.at(j - 1) = 0;
          
       }
        
       if(censtarn.at(j - 1) < 1.5) {
       
          rorder.at(j - 1) = (krevrank.at(j - 1) * d + (sumw + 1)) / (krevrank.at(j - 1) + 1);
          rmdrank.at(j - 1) = (rorder.at(j - 1) - 0.3) / (sumw + 0.4);
          d = rorder.at(j - 1);
          
       }
        
}

   if(debug::kprint >= 1) {
   
      Rcpp::Rcout << "\nADJUSTED ORDER NUMBERS\n" << std::endl;
      Rcpp::Rcout << "rorder = " << rorder << std::endl;
      Rcpp::Rcout << "\nMEDIAN RANK POSITIONS\n" << std::endl;
      Rcpp::Rcout << "rmdrank = " << rmdrank << std::endl;
   
   } 

// Drop out suspension times and transform variables
// for computation of means of x and y
   if(debug::kprint >= 1) {
   
      Rcpp::Rcout << "nrownw = " << nrownw << std::endl;
   
   } 

for(int j = 1; j <= nrownw; j++){

    if(censtarn.at(j - 1) > 1.5) {
    
       ynew.at(j - 1) = 0.0;
       x.at(j - 1) = 0.0;
    
    } else {
        
       ynew.at(j - 1) = std::log(ynew.at(j - 1));
       xstar.at(j - 1) = 1.0 / (1.0 - rmdrank.at(j - 1));
       x.at(j - 1) = std::log(std::log(xstar.at(j - 1)));
    
    }
      
    sumx = sumx + x.at(j - 1);
    sumy = sumy + ynew.at(j - 1);
        
}

   if(debug::kprint >= 1) {
   
      Rcpp::Rcout << "log(x)" << x << std::endl;
      Rcpp::Rcout << "log(ynew)" << ynew << std::endl;
   
   } 
       
// Obtain mean of x and y
   xbar = sumx / sumc;
   ybar = sumy / sumc;
        
   if(debug::kprint >= 1) {
   
      Rcpp::Rcout << "sumx = " << sumx << std::endl;
      Rcpp::Rcout << "sumy = " << sumy << std::endl;
      Rcpp::Rcout << "ybar = " << ybar << std::endl;
      Rcpp::Rcout << "xbar = " << xbar << std::endl;
   
   } 
       
// Do regression analysis using corrected sum of squares,
// eliminate suspension times by setting them equal to their
// mean observation value
   for(int j = 1; j <= nrownw; j++){
   
       if(censtarn.at(j - 1) > 1.5) {
       
          ynew.at(j - 1) = ybar;
          x.at(j - 1) = xbar;
      
       } else {
          
          ynew.at(j - 1) = ynew.at(j - 1);
          x.at(j - 1) = x.at(j - 1);
         
       }

       csumxy = csumxy + (x.at(j - 1) - xbar) * (ynew.at(j - 1) - ybar);
       csumx = csumx + (x.at(j - 1) - xbar) * (x.at(j - 1) - xbar);
        
   }
   
   if(debug::kprint >= 1) {
   
      Rcpp::Rcout << "csumx  = " << csumx  << std::endl;
      Rcpp::Rcout << "csumxy = " << csumxy << std::endl;
   
   } 

// Obtain estimates
   betastar  = csumxy / csumx;
   alphastar = ybar - betastar * xbar;
   beta = 1.0 / (betastar);
   eta = std::exp(alphastar);
   thetamrr.at(0) = eta;
   thetamrr.at(1) = beta;
   
   if(debug::kprint >= 1) {
   
      Rcpp::Rcout << "\nRANK REGRESSION ESTIMATES\n" << std::endl;
      Rcpp::Rcout << "Beta = " << beta << std::endl;
      Rcpp::Rcout << "eta = " << eta << std::endl;
   
   } 

return;

}
