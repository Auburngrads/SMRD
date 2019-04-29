#include <base/base.hpp>

//' Function to compute a relationship between beta and x
//' 26 Aug 94 changed irelat to irelag below

double grelat(Rcpp::NumericVector beta,
              int ipthta,
              Rcpp::NumericVector x,
              int n,
              int irelag){
   
double g_relat = zero;
   
// Call user specified function
if(irelag > 100) {
   
   // g_relat = usrelt(beta,x,n,irelag);
   
   
}

if(irelag == 1){
   
   for(int i = 1; i <= n; i++){
      
       g_relat = g_relat + beta.at((ipthta - 1) + i - 1) * x.at(i - 1);
      
   }

}

if(irelag == 2){
   
   //xerr(8976);
   
   // Dummy for now
      g_relat = beta.at(0);

}

if(irelag == 3){
   
   //xerr(8976);
   
   // Dummy for now
      g_relat = beta.at(0);

}

return g_relat;
      
}
