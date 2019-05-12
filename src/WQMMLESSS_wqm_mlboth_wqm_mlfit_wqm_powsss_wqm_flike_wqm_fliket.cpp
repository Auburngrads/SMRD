#include <base/base.hpp>
#include <wqmmlesss/wqm_flikei.hpp>

//' Compute the loglikelihood for a possibly 
//' truncated observation

double wqm_fliket(int itype,
                  double z,
                  double z2,
                  double sigmal,
                  int nty,
                  int ittype,
                  double trl,
                  double tru,
                  int kdist){

double fliket = 0.0e00;
double denom;
  
if((nty == 1) or (ittype == 1)) {

   // If there is no truncation, things are simple
      fliket = wqm_flikei(itype,z,z2,sigmal,kdist);
      goto exit;

}

denom = wqm_flikei(-1 * ittype,trl,tru,sigmal,kdist);

// If there is truncation, we need to get
// the appropriate numerator interval

if((ittype == 2) and (itype == 2)) {

    // Right censoring and right truncation
    //
    // Set the lower end of interval to be the right censoring point
    // Set the upper end of interval to be the right truncation point

    z2 = trl;
    itype = 4;

}

if((ittype == 3) and (itype == 3)) {

    // Left censoring and left truncation
    //
    // Set the lower end of interval to be the left truncation point
    // Set the upper end of interval to be the left censoring

    z2 = z;
    z = trl;
    itype = 4;

}

fliket = wqm_flikei(itype,z,z2,sigmal,kdist) - denom;

if(debug::kprint >= 6){
  
   Rcpp::Rcout << "\nEND OF WQM_FLIKET\n" << std::endl;
   Rcpp::Rcout << "itype"  << itype       << std::endl;
   Rcpp::Rcout << "kdist"  << kdist       << std::endl;
   Rcpp::Rcout << "z"      << z           << std::endl;
   Rcpp::Rcout << "z2"     << z2          << std::endl;
   Rcpp::Rcout << "sigmal" << sigmal      << std::endl;
   Rcpp::Rcout << "nty"    << nty         << std::endl;
   Rcpp::Rcout << "ittype" << ittype      << std::endl;
   Rcpp::Rcout << "trl"    << trl         << std::endl;
   Rcpp::Rcout << "tru"    << tru         << std::endl;
   Rcpp::Rcout << "fliket" << fliket      << std::endl;
  
}

  exit: return fliket;
        
}
