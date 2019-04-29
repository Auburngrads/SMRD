#include <base/base.hpp>
#include <genmax/flkt1.hpp>

using namespace genx00;
using namespace genx01;
using namespace genx03;
using namespace genx07;

//' Recover observation stuff data from stack and pass to likelihood routine
//' and the thetas vector (as seen by the user)

Rcpp::List flkt(Rcpp::List fargs){
   
int kpnow,nparm,weigi = 0;
Rcpp::NumericVector thetas;

kpnow  = Rcpp::as<int>(Rcpp::as<List>(fargs)["lk"]);
nparm  = Rcpp::as<int>(Rcpp::as<List>(fargs)["ln"]);
thetas = Rcpp::as<NumericVector>(Rcpp::as<List>(fargs)["lt"]);

double fl_kt;  
   
// Grab some space for the gamma vector that we will send to the
// likelihood routines
   Rcpp::NumericVector igamme = Rcpp::NumericVector(genx03::g_ngame);

// Go to get the likelihood value and its weight
   fl_kt = flkt1(kpnow,
                 genx07::g_kmod,
                 genx07::g_kdist,
                 weigi,
                 thetas,
                 nparm,
                 genx01::g_ipy,
                 genx00::g_ncoly,
                 genx00::g_nrownw,
                 genx01::g_ipcode,
                 genx01::g_ipweig,
                 genx00::g_ncolty,
                 genx01::g_ipty,
                 genx01::g_iptc,
                 igamme,
                 genx01::g_ipinow,
                 genx03::g_ngame);
   
   return Rcpp::List::create(Named("val") = fl_kt,
                             Named("weight") = genx01::g_ipweig.at(kpnow - 1));
   
}


#include <base/base.hpp>
#include <genmax/flkg.hpp>
#include <genmax/flkg0.hpp>
#include <genmax/flkg1.hpp>
#include <genmax/flkg2.hpp>
#include <genmax/flkg3.hpp>
#include <genmax/flkg4.hpp>
#include <genmax/flkg5.hpp>
#include <genmax/usrlik.hpp>
#include <utility/icheck.hpp>
#include <genmax/rgamme.hpp>

//' Compute the loglikelihood for a single observation for a single observation
//' as a function of the data and user-specified model
//'   kmod           model
//'    0        standard distribution
//'    1            lfp
//'    2            doa
//'    3            sts
//'    4            param. ph
//'    5            open
//'   >100          user specified

double flkt1(int kpnow,
             int kmod,
             int kdist,
             int weigi,
             Rcpp::NumericVector thetas,
             int nparm,
             Rcpp::NumericMatrix y,
             int ncoly,
             int nrownw,
             Rcpp::IntegerVector codes,
             Rcpp::IntegerVector weight,
             int ncolty,
             Rcpp::NumericMatrix ty,
             Rcpp::IntegerVector tcodes,
             Rcpp::NumericVector gamme,
             Rcpp::IntegerVector innow,
             int ngame){
   
double flkt_1 = zero,yl,yu;
double tryl = 0.0e00,tryu = 0.0e00;
int kccode,ktcode,ier = 0,kmodp;
   
// Pick up weight to send back to the accumulator
   weigi = weight.at(kpnow - 1);
   
// Check to see if the observation is in
   if(innow.at(kpnow - 1) == 0) return flkt_1;
   
// Get observation code and time (or limits)
   kccode = codes.at(kpnow - 1);
   yl = y.at(kpnow - 1,0);
   yu = yl;
   if(ncoly == 2) yu = y.at(kpnow - 1,1);
   
// Get truncation code and time (or limits)
   ktcode = 1;
   if(ncolty == 0) goto line50;
   tryl = ty.at(kpnow - 1,0);
   ktcode = tcodes.at(kpnow - 1);
   
// Get upper truncation limit for interval truncation
   tryu = tryl;
   if(ncolty == 2) tryu = ty.at(kpnow - 1,1);
   line50: if(kmod > 100) goto line200;

// Go to recover the gamma vector to be passed to the likelihood
   rgamme(kpnow,thetas,gamme);
   icheck(kmod,0,5,0,0,ier,-3100);
   kmodp = kmod + 1;

if(kmodp == 1) {
   
   flkt_1 = flkg(flkg0,kdist,yl,yu,kccode,ncolty,
                 tryl,tryu,ktcode,gamme,ngame);
   
   goto line199;
   
}

if(kmodp == 2) {
   
   flkt_1 = flkg(flkg1,kdist,yl,yu,kccode,ncolty,
                 tryl,tryu,ktcode,gamme,ngame);
   goto line199;
   
}

if(kmodp == 3) {
   
   flkt_1 = flkg(flkg2,kdist,yl,yu,kccode,ncolty,
                 tryl,tryu,ktcode,gamme,ngame);
   goto line199;
   
}

if(kmodp == 4) {
   
   flkt_1 = flkg(flkg3,kdist,yl,yu,kccode,ncolty,
                 tryl,tryu,ktcode,gamme,ngame);
   goto line199;
   
}

if(kmodp == 5) {
   
   flkt_1 = flkg(flkg4,kdist,yl,yu,kccode,ncolty,
                 tryl,tryu,ktcode,gamme,ngame);
   goto line199;
   
}

if(kmodp == 6) {
   
   flkt_1 = flkg(flkg5,kdist,yl,yu,kccode,ncolty,
                 tryl,tryu,ktcode,gamme,ngame);
   goto line199;
   
}

line200: flkt_1 = usrlik(thetas,nparm,kdist,kmod,yl,yu,
                         kccode,tryl,tryu,ktcode);

line199: if(debug::kprint >= 6){
   
            Rcpp::Rcout << "\nFLKT1**6**\n" << std::endl;
            Rcpp::Rcout << "flkt1 = " << flkt_1 << std::endl;
            Rcpp::Rcout << "kpnow = " << kpnow << std::endl;
            Rcpp::Rcout << "kmod = " << kmod << std::endl;
            Rcpp::Rcout << "kdist = " << kdist << std::endl;
            Rcpp::Rcout << "kccode = " << kccode << std::endl;
            Rcpp::Rcout << "ktcode = " << ktcode << std::endl;
            Rcpp::Rcout << "yl = " << yl << std::endl;
            Rcpp::Rcout << "yu = " << yu << std::endl;
            Rcpp::Rcout << "weigi = " << weigi << std::endl;

         }

return flkt_1;

}
