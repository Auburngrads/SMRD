#include <base/base.hpp>
#include <genmax/usloc1.hpp>

using namespace genx03;
using namespace genx07;

//' Adjust vcv for the estimation upest location shift

void usloc(Rcpp::NumericVector &theta,
           Rcpp::NumericMatrix &vcv,
           int &idim){
   
// We really need only ipkode here
   Rcpp::NumericVector ivhold = Rcpp::NumericVector(genx07::g_nparm);
   Rcpp::NumericVector idel = Rcpp::NumericVector(genx07::g_nparm);
   Rcpp::NumericVector igrad = Rcpp::NumericVector(genx07::g_nparm);
   Rcpp::IntegerVector IPKODE = clone(genx03::g_ipkode);
   
   usloc1(theta,IPKODE,vcv,ivhold,
          idel,igrad,genx07::g_nparm,idim);
   
   genx03::g_ipkode = clone(IPKODE);
   
return;
   
}

#include <base/base.hpp>
#include <genfun/tb0p.hpp>
#include <genfun/fdel.hpp>
#include <genfun/gvec.hpp>
#include <utility/wqm_filld.hpp>

using namespace genx07;
using namespace genx08;

//' Root for usloc

void usloc1(Rcpp::NumericVector &theta,
            Rcpp::IntegerVector &kodet,
            Rcpp::NumericMatrix &vcv,
            Rcpp::NumericVector &vhold,
            Rcpp::NumericVector &delta,
            Rcpp::NumericVector &grad,
            int &nparm,
            int &idim){

double epsx,test,eps;
int ktrcde, npoint;
Rcpp::List fargs,flist;
   
// cxx#perhaps this should be a controlable constant
   epsx = 1.0e-8;

// Do nothing if there was no translation
      if(genx07::g_kmccde >= 3) return;
      if(genx08::g_pest <= zero) return;
      fargs = Rcpp::List::create(Named("lt") = theta,
                                 Named("ln") = nparm);
      flist = tb0p(fargs);
      test = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
      eps = std::abs(epsx * test);
      
// we need kodet here to hold constants constant
// ktrcde=1 here becauseetheta is in the untransformed scale
   ktrcde = 1;
   fdel(tb0p,theta,ktrcde,kodet,nparm,eps,delta);
   npoint = 0;
   gvec(tb0p,npoint,theta,delta,nparm,ktrcde,kodet,grad);
   
   if(debug::kprint >= 4){
      
      Rcpp::Rcout << "\nUSLOC1**4**\n" << std::endl;
      Rcpp::Rcout << "test = " << test << std::endl;
      Rcpp::Rcout << "theta = " << theta << std::endl;
      Rcpp::Rcout << "delta = " << delta << std::endl;
      Rcpp::Rcout << "grad = " << grad << std::endl;
      Rcpp::Rcout << "kodet = " << kodet << std::endl;
      
   } 
   
   wqm_filld(0.0e00,vhold,1,nparm);
   
// Get the variance first
   for(int i = 1; i <= nparm; i++){
      
       for(int j  =1; j <= nparm; j++){
          
           vhold.at(0) = vhold.at(0) + grad.at(i - 1) * grad.at(j - 1) * vcv.at(i - 1,j - 1);
          
       }
       
   }
   
   if(nparm != 1){
      
      // Now get the covariances
         for(int i = 2; i <= nparm; i++){
            
             for(int j = 1; j <= nparm; j++){
                
                 vhold.at(i - 1) = vhold.at(i - 1) + grad.at(j - 1) * vcv.at(i - 1,j - 1);
                
             }
             
         }
         
   }
   
   vcv.at(0,0) = vhold.at(0);
   
// If there is only one parameter, we are done
   if(nparm == 1) return;
   
   for(int j = 2; j <= nparm; j++){
      
       // Replace the first row and column
          vcv.at(0,j - 1) = vhold.at(j - 1);
          vcv.at(j - 1,0) = vhold.at(j - 1);
          
   }
   
return;
   
}
