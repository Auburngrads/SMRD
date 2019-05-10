#include <base/base.hpp>

//' @description Find the product limit estimate going
//'             forward or in reverse - according to irev;

void wqm_plestx(Rcpp::NumericVector &enter,
                Rcpp::NumericVector &failr,
                Rcpp::NumericVector &failp,
                Rcpp::NumericVector &xlose,
                int &m,
                int &maxiuc,
                int &minilc,
                int irev,
                Rcpp::NumericVector &prob,
                Rcpp::NumericVector &sd){

double slast = zero;
double sprob = one;
double atrisk = zero;
double hazard, failt,sdd,fprob;
int j;
double denom;

for(int jx = 1; jx <= m; jx++){

    j = jx;

    if(irev == 1) j = (m - jx) + 1;

    atrisk = atrisk + enter.at(j - 1);

    hazard = zero;
    sdd = zero;
    failt = failr.at(j - 1) + failp.at(j - 1);

    if(atrisk > zero) hazard = failt / atrisk;

    sprob = sprob * (one - hazard);

    denom = atrisk * (atrisk - failt);

if(denom > zero) {

   slast = slast + failt / denom;
   sdd   = std::sqrt(sprob * sprob * slast);

} 
  
if(debug::kprint > 4) {
  
   fprob = one - sprob;
  
   Rcpp::Rcout << "\nWQM_PLESTX**\n" << std::endl;
   Rcpp::Rcout << "irev = "     << irev            << std::endl;
   Rcpp::Rcout << "enter(j) = " << enter.at(j - 1) << std::endl;
   Rcpp::Rcout << "failr(j) = " << failr.at(j - 1) << std::endl;
   Rcpp::Rcout << "failp(j) = " << failp.at(j - 1) << std::endl;
   Rcpp::Rcout << "failt = "    << failt           << std::endl;
   Rcpp::Rcout << "xlose(j) = " << xlose.at(j - 1) << std::endl;
   Rcpp::Rcout << "atrisk = "   << atrisk          << std::endl;
   Rcpp::Rcout << "hazard = "   << hazard          << std::endl;
   Rcpp::Rcout << "sprob = "    << sprob           << std::endl;
   Rcpp::Rcout << "fprob = "    << fprob           << std::endl;
   Rcpp::Rcout << "sdd = "      << sdd             << std::endl;

}

atrisk = atrisk - failt - xlose.at(j - 1);

if(irev == 0) goto line70; 

// Reverse
   if((j >= minilc) or (j == 1)) continue;
   prob.at(j - 2) = sprob;
   sd.at(j - 2) = sdd;
   continue;
   
// Forward
   line70: if(j < maxiuc) continue;
           prob.at(j - 1) = std::abs(one - sprob);
           sd.at(j - 1) = sdd;

}

prob.at(m - 1) = one;
sd.at(m - 1) = zero;

 return;
 
}
