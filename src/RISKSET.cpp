#include <base/base.hpp>
#include <riskset/wqm_riskset.hpp>

//' Compute the risk set for a recurrence process within 
//' observation windows.
//' 
//' @name riskset
//' 
//' @description We want the number of units at risk 
//'              just before each event time it is 
//'              also possible to send down a longer 
//'              list of times for purposes of making 
//'              a riskset plot.
//' @param muniqrecurr number of unique recurrence times
//' @param tuniq length(muniqrecurr) unique recurrence 
//'        times (in increasing order);
//' @param nwindows length of twindowsl and twindowsu;
//' @param twindowsl vector giving observation window 
//'        start points
//' @param twindowsu window end points corresponding 
//'        to twindowsl
//' @param wcounts counts for the windows;
//' @param iordl scratch space ordering vector for twindowsl;
//' @param iordu scratch ordering vector for twindowsu;
//' 
//' @return delta length(muniqrecurr) size of the risk set 
//'         just before time tuniq(j)
//'         
//' @details Wrapper for wqm_riskset        
// [[Rcpp::export]]
Rcpp::List riskset(int muniqrecurr,
                   Rcpp::NumericVector tuniq,
                   int nwindows,
                   Rcpp::NumericVector twindowsl,
                   Rcpp::NumericVector twindowsu,
                   Rcpp::IntegerVector wcounts,
                   Rcpp::IntegerVector iordl,
                   Rcpp::IntegerVector iordu,
                   Rcpp::IntegerVector delta,
                   int kdebug,
                   Rcpp::IntegerVector iscrat){

  debug::kprint = kdebug;
  
  wqm_riskset(muniqrecurr, tuniq, nwindows,
              twindowsl, twindowsu, wcounts, 
              iordl, iordu, delta, iscrat);
  
  return Rcpp::List::create(Named("muniqrecurr") = muniqrecurr,
                            Named("tuniq")       = tuniq,
                            Named("nwindows")    = nwindows,
                            Named("twindowsl")   = twindowsl,
                            Named("twindowsu")   = twindowsu,
                            Named("wcounts")     = wcounts,
                            Named("iordl")       = iordl,
                            Named("iordu")       = iordu,
                            Named("delta")       = delta,
                            Named("kdebug")      = kdebug,
                            Named("iscrat")      = iscrat);

}

#include <base/base.hpp>
#include <utility/merge_sortd.hpp>

void wqm_riskset(int &muniqrecurr,
                 Rcpp::NumericVector &tuniq,
                 int &nwindows,
                 Rcpp::NumericVector &twindowsl,
                 Rcpp::NumericVector &twindowsu,
                 Rcpp::IntegerVector &wcounts,
                 Rcpp::IntegerVector &iordl,
                 Rcpp::IntegerVector &iordu,
                 Rcpp::IntegerVector &delta,
                 Rcpp::IntegerVector &iscrat){

double twinl = 0;
double twinu = 0;
int icurrentl, icurrentu,deltaold;
//int deltaold;

// Find the index vector to order the
// upper and lower window endpoints
   iordl = Rcpp::as<Rcpp::IntegerVector>(merge_sortd(twindowsl, IntegerVector(nwindows, 0), false));
   iordu = Rcpp::as<Rcpp::IntegerVector>(merge_sortd(twindowsu, iordl, true));

//call merge_sortd(twindowsl,nwindows,iordl, iscrat);
//call merge_sortd(twindowsu,nwindows,iordu, iscrat);

// Initilize the window pointers
   icurrentl = 0;
   icurrentu = 0;

// loop over the unique recurrence times
for(int i = 0; i < muniqrecurr; i++) {

// initilize the risk set accumulator
   if(i > 0) {
   
      delta.at(i) = delta.at(i - 1);
   
    } else {
   
      delta.at(i) = 0;
    }

if(debug::kprint > 4){
  
   Rcpp::Rcout << "\nBefore Index,time,delta\n" << std::endl;
   Rcpp::Rcout << "i = "        << i << std::endl;
   Rcpp::Rcout << "tuniq(i) = " << tuniq.at(i) << std::endl;
   Rcpp::Rcout << "delta(i) = " << delta.at(i) << std::endl;
  
}

// Add in counts at the start of each window
// Continue until twinl >= tuniq(i) or end of list
   if(icurrentl < nwindows) {
   
      twinl = twindowsl.at(iordl.at(icurrentl));
   
   }


while((twinl <= tuniq.at(i)) and (icurrentl < nwindows)) {

       deltaold = delta.at(i);
       delta.at(i) = delta.at(i) + wcounts.at(iordl.at(icurrentl));
       
       if(debug::kprint > 4){
  
          Rcpp::Rcout << "\nwqm_riskset Add**\n"        << std::endl;
          Rcpp::Rcout << "i = "         << i            << std::endl;
          Rcpp::Rcout << "twinl = "     << twinl        << std::endl;
          Rcpp::Rcout << "icurrentl = " << icurrentl    << std::endl;
          Rcpp::Rcout << "nwindows = "  << nwindows     << std::endl;
          Rcpp::Rcout << "deltaold = "  << deltaold     << std::endl;
          Rcpp::Rcout << "wcounts(iordl(icurrentl)) = " << wcounts.at(iordl.at(icurrentl)) << std::endl;
          Rcpp::Rcout << "delta(i) = "  << delta.at(i)  << std::endl;
  
       }

       
       icurrentl = icurrentl + 1;
       
       if(icurrentl < nwindows) {
       
          twinl = twindowsl.at(iordl.at(icurrentl));
       
       }
}

// Subtract out counts at the end of each interval
// Continue until twinu <= tuniq(i) or end of list
   if(icurrentu < nwindows) {
   
      twinu = twindowsu.at(iordu.at(icurrentu));
   
   }

while((twinu <= tuniq.at(i)) and (icurrentu < nwindows)) {

       deltaold    = delta.at(i);
       delta.at(i) = delta.at(i) - wcounts.at(iordu.at(icurrentu));

       if(debug::kprint > 6){
  
          Rcpp::Rcout << "\nwqm_riskset Subtract**\n"        << std::endl;
          Rcpp::Rcout << "i = "         << i            << std::endl;
          Rcpp::Rcout << "twinu = "     << twinu        << std::endl;
          Rcpp::Rcout << "icurrentu = " << icurrentu    << std::endl;
          Rcpp::Rcout << "nwindows = "  << nwindows     << std::endl;
          Rcpp::Rcout << "deltaold = "  << deltaold     << std::endl;
          Rcpp::Rcout << "wcounts(iordu(icurrentu)) = " << wcounts.at(iordu.at(icurrentu)) << std::endl;
          Rcpp::Rcout << "delta(i) = "  << delta.at(i)  << std::endl;
  
       }

       
       icurrentu = icurrentu + 1;

       if(icurrentu < nwindows) {
       
          twinu = twindowsu.at(iordu.at(icurrentu));
       
       }
}

if(debug::kprint > 4){
  
   Rcpp::Rcout << "\nAfter Index,time,delta\n" << std::endl;
   Rcpp::Rcout << "i = "        << i << std::endl;
   Rcpp::Rcout << "tuniq(i) = " << tuniq.at(i) << std::endl;
   Rcpp::Rcout << "delta(i) = " << delta.at(i) << std::endl;
  
}

}

return;

}
