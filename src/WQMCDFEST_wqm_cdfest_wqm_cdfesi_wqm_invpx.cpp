#include <base/base.hpp>

//' @description Invert positive definite matrix
//'              stored in lower triangular form.
//'
//' @details     f(1,1),f(1,2),f(2,2),f(1,3),f(2,3),
//'              f(3,3), Invert in place.
//'              \code{ier=1} indicates non positive
//'              definite matrix

void wqm_invpx(Rcpp::NumericVector &f,
               int &n,
               int &ier){

double tol = 0.0625e00;
double x,test,sum,relz = tol / (float)n;
int ipndex = 1;
int ipind1,nm1,index;

ier = 0;

for(int i = 1; i <= n; i++){

    int iphold = ipndex;
    int kount = 1;
   
    for(int j = 1; j <= i; j++){

        x = f.at(ipndex - 1);

        if(j != 1) {

           for(int k = iphold; k <= ipind1; k++) {

               x = x - f.at(k - 1) * f.at(kount - 1);
               kount = kount + 1;

           }
          
        }
       
        if(i == j) {

           test = f.at(ipndex - 1) + x * relz;

           if(test <= f.at(ipndex - 1)) { ier = 1; return; }

           f.at(ipndex - 1) = one / std::sqrt(x);

         } else {

           f.at(ipndex - 1) = x * f.at(kount - 1);

         }
        
         ipind1 = ipndex;
         ipndex = ipndex + 1;
         kount  = kount + 1;
   
   }
   
   if(debug::kprint >= 5) {
       
      Rcpp::Rcout << "\nWQM_INVPX AFTER LOOP1\n" << std::endl;
      Rcpp::Rcout << "f = " << f << std::endl;
      Rcpp::Rcout << "n = " << n << std::endl;
       
   }

}

nm1 = n - 1;

if(n == 1) goto line55;

index = 1;

for(int i = 1; i <= nm1; i++){
    
    ipind1 = i + 1;
    int jm1 = i;
    int jndex = index;
    
    for(int j = ipind1; j <= n; j++){
        
        sum = zero;
        int indlow = index;
        int jindex = jndex + i;
        int jl = jindex;
     
        for(int l = i; l <= jm1; l++){
     
            sum = sum + f.at(indlow - 1) * f.at(jl - 1);
            jl = jl + 1;
            indlow = indlow + l;
      
        }
     
        jndex = jndex + j;
        f.at(jindex - 1) = -1 * f.at(jndex - 1) * sum;
        jm1 = j;
     
    }
    
    index = index + ipind1;
    
}

line55: if(debug::kprint >= 5) {
    
           Rcpp::Rcout << "\nWQM_INVPX AFTER LOOP2\n" << std::endl;
           Rcpp::Rcout << "f = " << f << std::endl;
           Rcpp::Rcout << "n = " << n << std::endl;
            
        }

index = 0;

for(int i = 1; i <= n; i++){

    int jndex = index;

    for(int j = i; j <= n; j++){

        sum = zero;
        int jindex = jndex + i;
        int indlow = jindex;
        jndex = jndex + j;
        int jndlow = jndex;

        for(int l = j; l <= n; l++){
           
            sum = sum + f.at(indlow - 1) * f.at(jndlow - 1);
            indlow = indlow + l;
            jndlow = jndlow + l;
            
        }

        f.at(jindex - 1) = sum;
    }

    index = index + i;
    
}

if(debug::kprint >= 5) {
    
   Rcpp::Rcout << "\nWQM_INVPX AFTER LOOP3\n" << std::endl;
   Rcpp::Rcout << "f = " << f << std::endl;
    
}


return;

}