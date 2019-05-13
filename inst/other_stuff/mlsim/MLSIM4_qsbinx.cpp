#include <base/base.hpp>
#include <utility/wqm_quant.hpp>
#include <utility/dsqrtc.hpp>
#include <mlsim2/kqbino.hpp>
#include <mlsim2/kqpois.hpp>

//' Compute an approximation for quantiles of the
//'         dist of the sum of ngroup binomials
//'
//'    Pr(K <= kfv; c(ng1,...ngp), c(rho1,...rhop))
//'  
//'     kmeth approx method (1=poison 
//'                          2=normal
//'                          3=montecarlo
//'                          4=bino exact (ngroup=1 only))

void qsbinx(int &kmeth,
            double pquan,
            Rcpp::IntegerVector &numrem,
            Rcpp::NumericVector &rhostr,
            int &ngroup,
            int &kans){
  
int mtrial = 10000,kount;
double rate,xmu,xvar;

// Poisson approximation
   if(kmeth == 1){
     
      rate = zero
   
      // Get the total rate
         for(int i = 1; i <= ngroup; i++){
           
             rate = rate + rhostr.at(i - 1) * (double)numrem.at(i - 1);
           
         }
         
         kans = kqpois(pquan, rate);
     
         if(debug::kprint >= 6){
           
            Rcpp::Rcout << "\nQSBINX POISSON\n" << std::endl;
            Rcpp::Rcout << "numrem(0) = " << numrem.at(0) << std::endl;
            Rcpp::Rcout << "rhostr(0) = " << rhostr.at(0) << std::endl;
            Rcpp::Rcout << "rate = "   << rate   << std::endl;
            Rcpp::Rcout << "pquan = "  << pquan  << std::endl;
            Rcpp::Rcout << "kans = "   << kans   << std::endl;
           
         }
   
         if(kans < 0) kans = 0;
           
   }
   
// Normal approximation
   if(kmeth == 2){
     
      xmu = zero; 
      xvar = zero;
      
      for(int i = 1; i <= ngroup; i++){
        
          xmu = xmu + rhostr.at(i - 1) * (double)numrem.at(i - 1);
          xvar = xvar + rhostr.at(i - 1) * (one - rhostr.at(i - 1)) * (double)numrem.at(i - 1);
        
      }
      
      kans = xmu + wqm_quant(pquan,3) * std::sqrt(xvar);
      if(kans < 0) kans = 0;
        
   }
   
   
// Monte Carlo approx (not programmed yet)
   if(kmeth == 3){
     
      kount = 0;
      if(ngroup != 1) Rcpp::stop("Monte Carlo approximation not finished yet")
     
   }
   
// Exact Binomial method
   if(kmeth == 4){
     
      if(ngroup != 1) Rcpp::stop("When using kmeth = 4, ngroup must equal 1")
      kans = kqbino(pquan, numrem.at(0), rhostr.at(0));
      
      if(debug::kprint >= 6){
           
         Rcpp::Rcout << "\nQSBINX BINOMIAL\n" << std::endl;
         Rcpp::Rcout << "numrem(1) = " << numrem.at(0) << std::endl;
         Rcpp::Rcout << "rhostr(1) = " << rhostr.at(0) << std::endl;
         Rcpp::Rcout << "rate = "   << rate   << std::endl;
         Rcpp::Rcout << "pquan = "  << pquan  << std::endl;
         Rcpp::Rcout << "kans = "   << kans   << std::endl;
           
      }

   }
   
return;
   
}
