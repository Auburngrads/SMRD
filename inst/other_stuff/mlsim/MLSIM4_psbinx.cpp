#include <base/base.hpp>
#include <mlsim2/ppois.hpp>
#include <mlsim2/dnldev.hpp>
#include <mlsim2/pbino.hpp>

//'  compute an approximation to the sum of ngroup binomials
//' 
//'     Pr(K <= kfv; c(ng1,...ngp), c(rho1,...rhop))
//' 
//'      #kmeth approx method (1=poison, 2=normal, 3=montecarlo
//'      #                          4 bino exact (ngroup=1 only))
//'      #
void psbinx(int &kmeth,
            int &kfv,
            Rcpp::IntegerVector &numrem,
            Rcpp::NumericVector &rhostr,
            int &ngroup,
            double &ans){
  
  
int mtrial = 10000,kount,icount;
double rate,xmu,xvar,z,itest;
  
// Poisson approximation
   if(kmeth == 1){
     
      rate = zero;
     
      for(int i = 1; i <= ngroup; i++){
        
          rate = rate + rhostr.at(i - 1) * (double)numrem.at(i - 1);
        
      }
      
      ppois(kfv,rate,ans,1);
        
   }
   
// Normal approximation
   if(kmeth == 2){
     
      xmu = zero;
      xvar = zero;
      
      for(int i = 1; i <= ngroup; i++){
        
         xmu = xmu + rhostr.at(i - 1) * (double)numrem.at(i - 1);
         xvar = xvar + rhostr.at(i - 1) * (one - rhostr.at(i - 1)) * (double)numrem.at(i - 1);
        
      }
      z = ((double)kfv + 0.5 - xmu) / std::sqrt(xvar);
      ans = R::pnorm(z,0,1,true,false);
        
   }
   
// Monte Carlo approx
   if(kmeth == 3){

      // Initialize the mc counter
         kount = 0;
     
         for(int j = 1; j <= mtrial; j++){
           
             // Do a single trial summing the binomials
                icount = 0;
                for(int i = 1; i <= ngroup; i++){
                  
                    icount = icount + dnldev(rhostr.at(i - 1),numrem.at(i - 1));
                  
                }
                
                itest = dnldev(0.5e00,10);
                
                if(debug::kprint >= 6){
                  
                   Rcpp::Rcout << "\nPSBINX MONTE CARLO\n" << std::endl;
                   Rcpp::Rcout << "icount = " << icount << std::endl;
                   Rcpp::Rcout << "kfv = "    << kfv    << std::endl;
                   Rcpp::Rcout << "kount = "  << kount  << std::endl;
                   Rcpp::Rcout << "itest = "  << itest  << std::endl;
                  
                }
                
             if(icount <= kfv) kount = kount + 1;
         
         }
         
      ans = (double)kount / (double)mtrial;
         
   }
   
// Exact Binomial method
   if(kmeth == 4){
     
      if(ngroup != 1) Rcpp::stop("When using kmeth = 4, ngroup must equal 1")
      pbino(kfv,numrem.at(0),rhostr.at(0),ans,1);
        
   }
   
return;
   
}