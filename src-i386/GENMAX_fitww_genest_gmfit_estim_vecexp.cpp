#include <base/base.hpp>

//' Move fixed parameters from the end of the vector according to kodet

void vecexp(Rcpp::NumericVector &thetam,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            Rcpp::NumericVector &thetat,
            int &nparmm){
   
int nfix,npos;   
   
nparmm = 0;
nfix = 0;

for(int i = 1; i <= nparm; i++){
   
    if(kodet.at(i - 1) != 0) {
       
      nparmm = nparmm + 1;
      thetat.at(i - 1) = thetam.at(nparmm - 1);
      continue;
       
    }
    
    nfix = nfix + 1;
    npos = nparm - nfix + 1;
    thetat.at(i - 1) = thetam.at(npos - 1);
         
}

return;
      
}
