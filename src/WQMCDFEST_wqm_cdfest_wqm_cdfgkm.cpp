#include <base/base.hpp>
#include <wqm_cdfest/wqm_plestx.hpp>
using namespace std;

//' @description Generalized kaplan estimator using
//'              turnbull's data structure.

void wqm_cdfgkm(Rcpp::IntegerVector &weight,
                Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                Rcpp::NumericVector &fail,
                Rcpp::NumericVector &xrcen,
                Rcpp::NumericVector &xlcen,
                Rcpp::NumericVector &xltru,
                Rcpp::NumericVector &xrtru,
                Rcpp::NumericVector &prob,
                Rcpp::NumericVector &sd,
                int &n,
                int &m,
                int &nty,
                int &ier){

bool llcen = false;
bool lrcen = false;
bool lltru = false;
bool lrtru = false;
int ilc,iuc,iut,ilt;
int xweig;

// zero the life table;
for(int j = 1; j <= m; j++){

    // debug mark for undefined probs in replacement;
       prob.at(j - 1)  = 5.0;
       sd.at(j - 1)    = 7.0;
       xlcen.at(j - 1) = zero;
       xrcen.at(j - 1) = zero;
       fail.at(j - 1)  = zero;
       xltru.at(j - 1) = zero;
       xrtru.at(j - 1) = zero;

}

int minilc = m;
int maxiuc = 1;
int maxilt = 1;
int miniut = m;

// Accumulate life table information and check data structure
for(int i = 1; i <= n; i++){

    ilc = ilcv.at(i - 1);
    iuc = iucv.at(i - 1);

// Skip dummy observations
   if(ilc == 0) continue;
   if((ilc == 1) and (iuc == m)) continue;

   ilt = 1;
   iut = m;

if(nty != 0) {

   ilt = iltv.at(i - 1);
   iut = iutv.at(i - 1);
   maxilt = std::max(maxilt,ilt);
   miniut = std::min(miniut,iut);

}

   xweig = weight.at(i - 1);

// Check to see if the observation interval contains the failure interval

if((ilc < ilt) or (iuc > iut)) { ier = 16; return; }

// Take obs in and out of risk set at the right time
   xrtru.at(iut - 1) = xrtru.at(iut - 1) + xweig;
   xltru.at(ilt - 1) = xltru.at(ilt - 1) + xweig;
   if(iut != m) lrtru = true;
   if(ilt != 1) lltru = true;

// look for a multiply left censored observation
   if((ilc == 1) and (iuc > 1)) {
   
       xlcen.at(iuc) = xlcen.at(iuc) + xweig;
       maxiuc = std::max(maxiuc,iuc);
       llcen = true;
       continue;
   
   }
   
// look for a multiply right censored observation
   if((iuc == m) and (ilc < m)) {
   
       xrcen.at(ilc - 2) = xrcen.at(ilc - 2) + xweig;
       minilc = std::min(minilc,ilc);
       lrcen = true;
       continue;
   
   }
   
// We have a bracketed failure -- check to  
// make surethat we know the exact interval 
// in which the failure occurred (ilc=iuc)
   if(ilc != iuc) { ier = -25; return; }

   fail.at(iuc - 1) = fail.at(iuc - 1) + xweig;

}

// Check to see if the generalized kaplan-meier can be used
if(debug::kprint > 4) {
  
   for(int j = 1; j <= m; j++){
     
       Rcpp::Rcout << "\nWQM_CDFGKM\n" << std::endl;
       Rcpp::Rcout << "j = "        << j - 1  << std::endl;
       Rcpp::Rcout << "xltru(j) = " << xltru.at(j - 1) << std::endl;
       Rcpp::Rcout << "xrtru(j) = " << xrtru.at(j - 1) << std::endl;
       Rcpp::Rcout << "xlcen(j) = " << xlcen.at(j - 1) << std::endl;
       Rcpp::Rcout << "xrcen(j) = " << xrcen.at(j - 1) << std::endl;
       Rcpp::Rcout << "fail(j) = "  << fail.at(j - 1) << std::endl;
      
   }
   
       Rcpp::Rcout << "maxiuc =" << maxiuc << std::endl;
       Rcpp::Rcout << "multiple left censoring =" << llcen << std::endl;
       Rcpp::Rcout << "minilc =" << minilc << std::endl;
       Rcpp::Rcout << "multiple right censoring =" << lrcen << std::endl;
       Rcpp::Rcout << "maxilt =" << maxilt << std::endl;
       Rcpp::Rcout << "left truncation =" << lltru << std::endl;
       Rcpp::Rcout << "miniut =" << miniut << std::endl;
       Rcpp::Rcout << "right truncation =" << lrtru << std::endl;
  
}

// #check for illegal combinations of censoring and truncation;
if((llcen) and (lltru)) { ier = -22; return; }
if((lrcen) and (lrtru)) { ier = -23; return; }
if((lltru) and (lrtru)) { ier = -23; return; }

// # reverse | agree | forward;
// # |||----------------+-----------------+---------------|||;
// # maxiuc minilc;
// # maxilt miniut;
// #check for illegal crossing or illegal combinations;
if(((maxiuc + 1) >= (minilc)) and (llcen and lrcen)){

  ier = -24;
  return;

}
// if((maxilt+1) >= (minilc)) goto line9824;
// if((maxiuc+1) >= (miniut)) goto line9824;

// If there is right truncation, then there 
// can not be right censoring, so do only 
// reverse plest.
if(!lrtru) {
  
   wqm_plestx(xltru,fail,xlcen,xrcen,m,maxiuc,minilc,0,
              prob,sd);

// if there was left truncation, then there 
// cannot be left censoring so skip reverse plest
   if(lltru) {
      
      if(debug::kprint > 4){

         double sprobx;  

         for(int j = 0; j < m; j++){
           
             sprobx = std::max(one - prob.at(j),zero);
           
             Rcpp::Rcout << "plest est j =" << j << std::endl;
             Rcpp::Rcout << "prob(j) =" << prob.at(j) << std::endl;
             Rcpp::Rcout << "sprobx =" << sprobx << std::endl;
             Rcpp::Rcout << "sd(j) =" << sd.at(j) << std::endl;
           
         }
     }
      
   }

}
// if no right censoring, then we want to go all the way in reverse
   if(!lrcen) minilc = m + 1;

   wqm_plestx(xrtru,fail,xrcen,xlcen,m,maxiuc,minilc,1,
              prob,sd);

   if(debug::kprint > 4){
      
      double sprobx;  
      
      for(int j = 0; j < m; j++){
        
          sprobx = std::max(one - prob.at(j),zero);
        
          Rcpp::Rcout << "\nplest est\n"               << std::endl; 
          Rcpp::Rcout << "j = "          << j          << std::endl;
          Rcpp::Rcout << "prob(j) = "    << prob.at(j) << std::endl;
          Rcpp::Rcout << "sprobx = "     << sprobx     << std::endl;
          Rcpp::Rcout << "sd(j) = "      << sd.at(j)   << std::endl;
        
      }
  }

 return;

}
