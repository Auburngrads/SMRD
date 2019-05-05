#include <base/base.hpp>

// #nonparametric (resampling) bootstrap;
// #simulate the data by sampling with replacement from the data;
// #we will do this by generating a new col of weights;

void simnp(Rcpp::IntegerVector &iarray,
           int &marray,
           int &method,
           int &kount,
           Rcpp::IntegerVector &wtnew,
           int &nrow){
  
double xran, mark;
int jndex, index, itemp = 0;
wtnew = Rcpp::IntegerVector(nrow);
  
if(method != 2) {

   // here the total number of observations is large, 
   // and we will search thru each time
      for(int i = 1; i <= kount; i++){
        
          xran = R::unif_rand();
          index = xran * kount + 1;
          jndex = iarray.at(index - 1);
          wtnew.at(jndex - 1) = wtnew.at(jndex - 1) + 1;
        
          if(debug::kprint > 0) {
             
              Rcpp::Rcout << "\nSIMNP method 1\n" << std::endl;
              Rcpp::Rcout << "i = " << i << std::endl;
              Rcpp::Rcout << "kount = " << kount << std::endl;
              Rcpp::Rcout << "jndex = " << jndex << std::endl;
              Rcpp::Rcout << "index = " << index << std::endl;
              Rcpp::Rcout << "iarray(index) = " << iarray.at(index - 1) << std::endl;
              Rcpp::Rcout << "xran = " << xran << std::endl;
              Rcpp::Rcout << "wtnew(jndex) = " << wtnew.at(jndex - 1) << std::endl;
             
           }

      }
   
   return;
  
}

// 25apr2009 Do we ever use the following?;
for(int i = 1; i <= kount; i++){
  
    xran = R::unif_rand();
  
    // mark will be between 1 and kount;
       mark = xran * kount + 1;
       
    for(jndex = 1; jndex <= nrow; jndex++){
      
        itemp = iarray.at(jndex - 1);
      
        if(itemp >= mark) goto line45;
          
        }

        jndex = 1;
             
        if(debug::kprint > 0) {
             
           Rcpp::Rcout << "\nSIMNP method 2\n" << std::endl;
           Rcpp::Rcout << "i = " << i << std::endl;
           Rcpp::Rcout << "kount = " << kount << std::endl;
           Rcpp::Rcout << "jndex = " << jndex << std::endl;
           Rcpp::Rcout << "mark = " << mark << std::endl;
           Rcpp::Rcout << "itemp = " << itemp << std::endl;
           Rcpp::Rcout << "iarray(jndex) = " << iarray.at(jndex - 1) << std::endl;
           Rcpp::Rcout << "xran = " << xran << std::endl;
           Rcpp::Rcout << "wtnew(jndex) = " << wtnew.at(jndex - 1) << std::endl;
             
        }

    
line45: wtnew.at(jndex - 1) = wtnew.at(jndex - 1) + 1;

}

return;

}
