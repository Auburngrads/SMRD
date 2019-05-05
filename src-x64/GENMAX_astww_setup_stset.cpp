#include <base/base.hpp>

using namespace genx00; ///nrownw,ncoly,ncolty
using namespace genx01; ///ipy,ipcode,ipweig,ipinow,ipty,iptc
using namespace genx03; ///ipkode,ipplab,ngame
using namespace genx05; ///ncolx,ipxbar,ipxbru,ipsd,ipiscd,ipx
using namespace genx07; ///kdist,kmod,kmccde,kpopu,llog,nparm

//' Set the model stuff into a global namespace 
//' In the original FORTRAN this would set the model stuff into 
//' the common block

void stset(int kdist,
           int llog,
           int kmod,
           Rcpp::IntegerVector ipkode,
           int nparm,
           int ngame,
           Rcpp::NumericVector ipxbar,
           Rcpp::NumericVector ipxbru,
           Rcpp::NumericVector ipsd,
           Rcpp::NumericMatrix ipy,
           int ncoly,
           int nrownw,
           Rcpp::NumericMatrix ipx,
           int ncolx,
           Rcpp::IntegerVector ipcode,
           Rcpp::IntegerVector ipweig,
           Rcpp::NumericMatrix ipty,
           int ncolty,
           Rcpp::IntegerVector ipinow,
           Rcpp::IntegerVector ipiscd,
           Rcpp::IntegerVector ipplab,
           Rcpp::IntegerVector iptc) {

genx00::g_ncolty = ncolty;
genx00::g_nrownw = nrownw;
genx00::g_ncoly = ncoly;
genx00::g_npoint = nrownw;

genx01::g_ipy = ipy;
genx01::g_ipcode = ipcode;
genx01::g_ipweig = ipweig;
genx01::g_ipinow = ipinow;
genx01::g_ipty = ipty;
genx01::g_iptc = iptc;

genx03::g_ipkode = ipkode;
genx03::g_ipplab = ipplab;
genx03::g_ngame = ngame;

genx05::g_ncolx = ncolx;
genx05::g_ipxbar = ipxbar;
genx05::g_ipxbru = ipxbru;
genx05::g_ipsd = ipsd;
genx05::g_ipiscd = ipiscd;
genx05::g_ipx = ipx;

genx07::g_kdist = kdist;
genx07::g_kmod = kmod;
genx07::g_llog = llog;
genx07::g_nparm = nparm;

      return;
}
