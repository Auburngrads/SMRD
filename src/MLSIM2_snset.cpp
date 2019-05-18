#include <base/base.hpp>

// Set up for generation of a multinomial random weight vector;
// #;
// #nonparametric bootstrap;
// #simulate the data by sampling with replacement from the data;
// #we will do this by generating a new col of weights;
// #;
// #inputs:;
// #wtold(nrow) old weight vector;
// #;
// #nrow length of old weight vector;
// #;
// #outputs:;
// #kount sum of the weight vector;
// #;
// #iarray iarray(marray) scratch vector;
// #;
// #marray length of the scratch vector should be at > nrow;
// # more efficient if > kount;
// #;
// #ier;

void snset(Rcpp::IntegerVector &wtold,
           int &nrow,
           int &kount,
           int &method,
           Rcpp::IntegerVector &iarray,
           int &marray,
           int &ier){
  
int jndex = 0, number = 0;
ier = 0;
kount = 0;

for(int i = 0; i < nrow; i++){
  
    kount = kount + wtold.at(i);
  
}

if(kount <= marray) { 

   // method 1 - Here the number of observations is small enough to make a table look-up
   // wtold: 3 2 1 3 1 1 5
   // iarray: 1 1 1 2 2 3 4 4 4 5 6 7 7 7 7 7
      jndex = 0;
      method = 1;
      
      for(int i = 1; i <= nrow; i++) {
        
          number = wtold.at(i - 1);
          if(number <= 0) continue;
          
          for(int j = 1; j <= number; j++) {
            
              jndex = jndex + 1;
              iarray.at(jndex - 1) = i;
          //xx write(6,433)i,nrow,number,jndex,iarray(jndex);
          //xx 433 format(' setup method-1-',5i5);
          }
          
      }
      
if(debug::kprint >= 3){
   
   Rcpp::Rcout << "\nSNSET METHOD 1\n" << std::endl;
   Rcpp::Rcout << "iarray = \n" << iarray << std::endl;
   
}
   
return;

}

// method 2 - here the total number of observations is large, and we will search thru each time
// wtold: 3 2 1 3 1 1 5
// iarray: 3 5 6 9 10 11 16
   method = 2;
   
   if(nrow > marray) ier = 1; return;
            
   iarray.at(0) = wtold.at(0);
            
   for(int i = 2; i <= nrow; i++){
              
       iarray.at(i - 1) = iarray.at(i - 2) + wtold.at(i - 1);
              
   }

if(debug::kprint >= 3){
   
   Rcpp::Rcout << "\nSNSET METHOD 1\n" << std::endl;
   Rcpp::Rcout << "iarray = \n" << iarray << std::endl;
   
}

return;

}
