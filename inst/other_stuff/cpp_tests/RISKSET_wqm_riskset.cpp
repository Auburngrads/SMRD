#include <base/base.h>
#include <utility/merge_sortd.h>

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