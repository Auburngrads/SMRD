#include <base/base.hpp>
#include <genmax/flktt1.hpp>

using namespace genx00;
using namespace genx01;
using namespace genx03;
using namespace genx07;

//' compute the (negative) of the log likelihood by
//' as a function of the unconstrained thetat vector
//' npoint=0    single call to flkt(thetas,n)
//' npoint>0    npoint calls to flkt(i,thetas,n)
//' 
//' this routine assumes that the fixed parameters have not been moved
//' to the end of the vector

Rcpp::List flktt(Rcpp::List fargs){
   
Rcpp::NumericVector thetat;
double fl_ktt;   

thetat = Rcpp::as<NumericVector>(Rcpp::as<List>(fargs)["lt"]);   

// Grab space for the untransformed thetas values that we will compute
   Rcpp::NumericVector ithets = Rcpp::NumericVector(genx07::g_nparm);
   Rcpp::IntegerVector IPKODE = clone(genx03::g_ipkode);
   Rcpp::IntegerVector IPINOW = clone(genx01::g_ipinow);
   
   fl_ktt = flktt1(thetat,ithets,IPKODE,IPINOW,
                   genx07::g_nparm,genx00::g_npoint);
   
   genx03::g_ipkode = clone(IPKODE);
   genx01::g_ipinow = clone(IPINOW);
    
return Rcpp::List::create(Named("val") = fl_ktt);

}

#include <base/base.hpp>
#include <genfun/unfixp.hpp>
#include <genmax/flkt.hpp>
#include <genfun/exadd.hpp>
#include <genmax/ptgame.hpp>

//' Root called by flktt and flkttx to compute total loglikelihood from thetat

double flktt1(Rcpp::NumericVector thetat,
              Rcpp::NumericVector thetas,
              Rcpp::IntegerVector kodet,
              Rcpp::IntegerVector innow,
              int nparm,
              int npoint){
   
Rcpp::NumericVector accum = Rcpp::NumericVector(2);
double fact,fl_ktt1;
int kpnow = 1,weigi;
Rcpp::List fargs,flist;
   
if(debug::kprint >= 7){
   
   Rcpp::Rcout << "\nFLKTT1**7** AT START\n" << std::endl;
   
}

// Zero the extended precision accumulators
   accum.at(0) = zero;
   accum.at(1) = zero;
   
// Get the untransformed thetas vector
   unfixp(thetat,kodet,nparm,thetas);
   
// save the parameter vector in common so that constants do not have to
// be unnessarily computed inside the summation
   ptgame(thetas);
   
// if npoint=0, we will compute the entire likelihood at one time below
   if(npoint == 0) goto line23;
   
// Loop over the rows in the data matrix
   for(kpnow = 1; kpnow <= npoint; kpnow++){
      
       // Skip observatiuon that is not in
          if(innow.at(kpnow - 1) == 0) continue;
          
       // Compute the likelihood for the ith row of the data matrix and pick up the weight
          weigi = 0;
          fargs = Rcpp::List::create(Named("lk") = kpnow,
                                     Named("lt") = thetas,
                                     Named("ln") = nparm);
          flist = flkt(fargs);
          weigi = Rcpp::as<int>(Rcpp::as<List>(flist)["weight"]);
          fact  = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);

       // Add it to the accumulator  using the weight factor received from below
          exadd(weigi * fact,accum);
   
   }
   
// Recover the final value of the summation
   fl_ktt1 = accum.at(0) + accum.at(1);
   goto line24;
   
line23: fargs = Rcpp::List::create(Named("lk") = kpnow,
                                   Named("lt") = thetas,
                                   Named("ln") = nparm);
          flist = flkt(fargs);
          weigi = Rcpp::as<int>(Rcpp::as<List>(flist)["weight"]);
        fl_ktt1 = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);

line24: if(debug::kprint >= 5){
    
           Rcpp::Rcout << "\nFLKTT1**5**\n" << std::endl;
           Rcpp::Rcout << "flktt1 = " << fl_ktt1 << std::endl;
           Rcpp::Rcout << "thetas = " << thetas  << std::endl;
   
          }

// Send back the negative of the log likelihood for powell min routine
   fl_ktt1 = -1 * fl_ktt1;

return fl_ktt1;

}