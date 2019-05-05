#include <base/base.hpp>

//' Move fixed parameters to the end of the vector according to kodet

void vecsqu(Rcpp::NumericVector &thetat,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            Rcpp::NumericVector &thetam,
            int &nparmm){

int nfix,npos;
   
nparmm = 0;
nfix = 0;

for(int i = 1; i <= nparm; i++){
   
    if(kodet.at(i - 1) != 0){
       
       nparmm = nparmm + 1;
       thetam.at(nparmm - 1) = thetat.at(i - 1);
       continue;
       
    }
    
    nfix = nfix + 1;
    npos = nparm - nfix + 1;
    thetam.at(npos - 1) = thetat.at(i - 1);

}

return;
      
}
