#include <base/base.hpp>
#include <utility/dexpc.hpp>

//' Return t incremented (decremented) by an amount s (on proper scale)
//' if ktrcde=0, we just use +-s
 
double fdup(double t,
            double s,
            int ktrcde,
            int kode){

double f_dup;
int kodep = kode + 1;
   
if(ktrcde == 0) kodep = 2;

if(kodep == 1){ f_dup = t; };

if(kodep == 2){ f_dup = t + s; }

if(kodep == 3){ f_dup = t * dexpc(s); }

if(kodep == 4){ f_dup = t / (t + (one - t) * dexpc(-s)); }

if(kodep == 5){ f_dup = t + s; }

if(kodep == 6){ f_dup = t + s; }

if(debug::kprint >= 2){
   
   Rcpp::Rcout << "\nFDUP**6**\n" << std::endl;
   Rcpp::Rcout << "ktrcde = " << ktrcde << std::endl;
   Rcpp::Rcout << "kode = " << kode << std::endl;
   Rcpp::Rcout << "t = " << t << std::endl;
   Rcpp::Rcout << "s = " << s << std::endl;
   Rcpp::Rcout << "f_dup = " << f_dup << std::endl;
   
}

return f_dup;
      
}
