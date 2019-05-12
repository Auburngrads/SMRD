#include <base/base.hpp>
#include <utility/dlogc.hpp>

//' Find the incomplete data gkm log likelihood

void cdfxlk(Rcpp::IntegerVector &ilcv,
            Rcpp::IntegerVector &iucv,
            Rcpp::IntegerVector &iltv,
            Rcpp::IntegerVector &iutv,
            Rcpp::IntegerVector &weight,
            int &nty,
            int &n,
            Rcpp::NumericVector &probd,
            int &m,
            double &xllgkm){
  
double sumc,sumt,add;
int ilc = 0,iuc = 0,ilt = 0,iut = 0;
  
xllgkm = zero;

// Sum over data rows
   for(int i = 1; i <= n; i++){
     
       sumc = zero;
       sumt = one;
       add = zero;
       ilc = ilcv.at(i - 1);
       if(ilc <= 0) goto line32;
       iuc = iucv.at(i - 1);
       
       // Sum up probs in the covering interval
          for(int j = ilc; j <= iuc; j++){
            
              sumc = sumc + probd.at(j - 1);
            
          }
          
          if(nty == 0) goto line56;
          sumt = zero;
          ilt = iltv.at(i - 1);
          iut = iutv.at(i - 1);
       
      // Sum up probs in the observation interval
         for(int j = ilt; j <= iut; j++){
           
             sumt = sumt + probd.at(j - 1);
           
         }
         
line56: if((sumc > zero) & (sumt > zero)){
  
            add = dlogc(sumc / sumt);
  
        }

        xllgkm = xllgkm + weight.at(i - 1) * add;

line32: if(debug::kprint >= 6){
  
           Rcpp::Rcout << "\nCDFXLK**6**\n"                  << std::endl;
           Rcpp::Rcout << "i = "         << i - 1            << std::endl;
           Rcpp::Rcout << "ilc = "       << ilc              << std::endl;
           Rcpp::Rcout << "iuc = "       << iuc              << std::endl;
           Rcpp::Rcout << "weight(i) = " << weight.at(i - 1) << std::endl;
           Rcpp::Rcout << "sumc = "      << sumc             << std::endl;
           Rcpp::Rcout << "add = "       << add              << std::endl;
           Rcpp::Rcout << "xllgkm = "    << xllgkm           << std::endl;

        }

   }
   
return;
   
}