#include <base/base.hpp>

void wqm_cdfegr(Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                Rcpp::IntegerVector &weight,
                int &nty,
                int &n,
                Rcpp::NumericVector &probd,
                int &m,
                Rcpp::NumericVector &pgrad){

double sumc,sumt,add,start;
int mm1 = m - 1;
int ilt = 0, iut = 0, ilc = 0, iuc = 0;

for(int j = 1; j <= m; j++) { pgrad.at(j - 1) = 0.0;}

for(int i = 1; i <= n; i++){

    ilc = ilcv.at(i - 1);
    if(ilc <= 0) continue;
    iuc = iucv.at(i - 1);
    sumc = 0.0;

    for(int j = ilc; j <= iuc; j++) {
    
        sumc = sumc + probd.at(j - 1);
    
    }

    sumt = one;

    if(nty != 0) {
    
       sumt = 0.0;
       ilt  = iltv.at(i - 1);
       iut  = iutv.at(i - 1);
    
       for(int j = ilt; j <= iut; j++) {
       
           sumt = sumt + probd.at(j - 1);
       
       }
    
    }

    start = 0.0;

    if(iuc == m) start = -1 * one / sumc;

    if(nty != 0) {
    
       if(iut == m) start = start + one / sumt;
    
    }

    for(int j = 1; j <= mm1; j++){
    
        if(probd.at(j - 1) <= 0.0) continue;
        
         add = start;
         
         if((j >= ilc) and (j <= iuc)) add = add + one / sumc;
        
         if(nty != 0) {
        
            if((j >= ilt) and (j <= iut)) add = add - one / sumt;
           
         }
         
           pgrad.at(j - 1) = pgrad.at(j - 1) + weight.at(i - 1) * add;
           
        }
}

return;

}
