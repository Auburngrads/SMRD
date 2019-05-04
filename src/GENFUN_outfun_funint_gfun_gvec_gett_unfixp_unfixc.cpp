#include <base/base.hpp>
#include <genfun/tquan.hpp>
#include <genfun/tfprob.hpp>

using namespace genx07;
using namespace genx08;
using namespace genx09;

//' return constrained, shifted, value of thetas(1) for constrained optimization
//' now upest depends only on dist and quantile and if
//' fixed once and for all.  more generally, upest will
//' be determined here as a function of all of the parameters (e.g. shape)
//' for estimation only, we can set up upest above and use klmode=2 (??? 7-85)
//' this is a relatively high-level routine so we can afford to do
//' some extra computation here

double fixc(Rcpp::NumericVector thetas,
            int nparm){
  
double fix_c = thetas.at(0);
Rcpp::List fargs,TQUAN,TFPROB;
        
if(genx07::g_kmccde < 3) {
   
   fargs = Rcpp::List::create(Named("lt") = thetas,
                              Named("ln") = nparm);
   
   TQUAN  = tquan(fargs);
   TFPROB = tfprob(fargs);

   if(genx08::g_klmode == 1) fix_c = Rcpp::as<double>(Rcpp::as<List>(TQUAN)["val"]);
   
   if(genx08::g_klmode == 2) fix_c = Rcpp::as<double>(Rcpp::as<List>(TQUAN)["val"]);
   
   if(genx08::g_klmode == 3) fix_c = Rcpp::as<double>(Rcpp::as<List>(TQUAN)["val"]);
   
   if(genx08::g_klmode == 4) fix_c = Rcpp::as<double>(Rcpp::as<List>(TFPROB)["val"]);

}

if(debug::kprint >= 6){
  
   Rcpp::Rcout << "\nFIXC**6**\n" << std::endl;
   Rcpp::Rcout << "fixc = " << fix_c << std::endl;
   Rcpp::Rcout << "kpoint = " << genx08::g_kpoint << std::endl;
   Rcpp::Rcout << "kscloc = " << genx09::g_kscloc << std::endl;
   Rcpp::Rcout << "kmccde = " << genx07::g_kmccde << std::endl;
   Rcpp::Rcout << "klmode = " << genx08::g_klmode << std::endl;
   Rcpp::Rcout << "pest = " << genx08::g_pest << std::endl;
   Rcpp::Rcout << "upest = " << genx08::g_upest << std::endl;
   Rcpp::Rcout << "thetas = " << thetas << std::endl;
  
}

return fix_c;

}

#include <base/base.hpp>
#include <genfun/tb0p.hpp>
#include <genfun/tb0f.hpp>

using namespace genx07;
using namespace genx08;
using namespace genx09;

//' Find the location parameter at xbar, starting with the
//' fixed value of the artificial intercept in thetas(1)
//' #later we can use kmccde to speed up with upest

double unfixc(Rcpp::NumericVector thetas,
              int nparm){
  
double un_fixc = thetas.at(0);
Rcpp::List fargs,TB0P,TB0F;
      
if(genx07::g_kmccde < 3) {
   
   fargs = Rcpp::List::create(Named("lt") = thetas,
                              Named("ln") = nparm);
   
   TB0P = tb0p(fargs);
   TB0F = tb0f(fargs);
  
   if(genx08::g_klmode == 1) un_fixc = Rcpp::as<double>(Rcpp::as<List>(TB0P)["val"]);
   
   if(genx08::g_klmode == 2) un_fixc = Rcpp::as<double>(Rcpp::as<List>(TB0P)["val"]);
   
   if(genx08::g_klmode == 3) un_fixc = Rcpp::as<double>(Rcpp::as<List>(TB0P)["val"]);
   
   if(genx08::g_klmode == 4) un_fixc = Rcpp::as<double>(Rcpp::as<List>(TB0F)["val"]);
   
}

if(debug::kprint >= 6){
  
   Rcpp::Rcout << "\nUNFIXC**6**\n" << std::endl;
   Rcpp::Rcout << "unfixc = " << un_fixc << std::endl;
   Rcpp::Rcout << "kpoint = " << genx08::g_kpoint << std::endl;
   Rcpp::Rcout << "kscloc = " << genx09::g_kscloc << std::endl;
   Rcpp::Rcout << "kmccde = " << genx07::g_kmccde << std::endl;
   Rcpp::Rcout << "klmode = " << genx08::g_klmode << std::endl;
   Rcpp::Rcout << "pest = " << genx08::g_pest << std::endl;
   Rcpp::Rcout << "upest = " << genx08::g_upest << std::endl;
   Rcpp::Rcout << "thetas = " << thetas << std::endl;
  
}

return un_fixc;

}
