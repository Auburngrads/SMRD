#include <base/base.hpp>
#include <utility/eps.hpp> // done

//' @description Subroutine to find the truncation indices

void wqm_cdftru(Rcpp::NumericVector &ys,
                Rcpp::NumericMatrix &ty,
                int &nty,
                Rcpp::IntegerVector &tcodes,
                int &n,
                Rcpp::NumericVector &p,
                Rcpp::NumericVector &q,
                int &m,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                int &ier){

bool ll;
double ysi;
double ysip;
double tlow = 0.0e00;
double thigh = 0.0e00;
int itype;

// Find the limits defining the p-q intervals
// within each truncation interval (bi sets)
for(int i = 1; i <= n; i++){

    ysi  = ys.at(i - 1);
    ysip = ys.at(i - 1 + n);
    iutv.at(i - 1) = m;
    iltv.at(i - 1) = 1;

// Skip over dummy obserations
   if((ysi >= big31) or (nty == 0)) {
   
       tlow  = -big32;
       thigh =  big32;
       continue;
   
   }

itype = tcodes.at(i - 1);

if(itype == 1) {

   tlow  = -big32;
   thigh =  big32;
   continue;

}

if(itype == 2) {

  tlow  = -big32;
  thigh =  ty.at(i - 1,0) + eps(ty.at(i - 1,0), 0.00004);
  continue;
}

if(itype == 3) {

  tlow  = ty.at(i - 1,nty - 1) - eps(ty.at(i - 1,nty - 1), 0.00004);
  thigh = big32;
  continue;
}

if(itype == 4) {

   tlow  = ty.at(i - 1,0)       - eps(ty.at(i - 1,0)      , 0.00004);
   thigh = ty.at(i - 1,nty - 1) + eps(ty.at(i - 1,nty - 1), 0.00004);
   continue;
   
}

if(nty == 0) continue;

if((tlow > ysi) or (thigh < ysip)) {

    ier = 16;
    return;

  }

ll = false;

for(int j = 1; j <= m; j++){

    if(ll) {
    
       if(q.at(j - 1) <= thigh) continue;
       iutv.at(i - 1) = j - 1;
       goto line32;
    
    }

    if(p.at(j - 1) < tlow) continue;
    
    iltv.at(i - 1) = j;
    ll = true;
    continue;

}

iutv.at(i - 1) = m;

line32:  continue;

  if(debug::kprint >= 10){
    
     double iltvi = iltv.at(i - 1);
     double iutvi = iutv.at(i - 1);
     
     Rcpp::Rcout << "\nfind iltv,iutv\n"            << std::endl;
     Rcpp::Rcout << "i = "       << i - 1           << std::endl;
     Rcpp::Rcout << "ysi = "     << ysi             << std::endl;
     Rcpp::Rcout << "ysip = "    << ysip            << std::endl;
     Rcpp::Rcout << "tlow = "    << tlow            << std::endl;
     Rcpp::Rcout << "thigh = "   << thigh           << std::endl;
     Rcpp::Rcout << "iltv(i) = " << iltv.at(i - 1)  << std::endl;
     Rcpp::Rcout << "iutv(i) = " << iutv.at(i - 1)  << std::endl;
     Rcpp::Rcout << "ptru = "    << p.at(iltvi - 1) << std::endl;
     Rcpp::Rcout << "qtru = "    << q.at(iutvi - 1) << std::endl;
     
  }

}

 return;

}
