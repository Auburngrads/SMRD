#include <base/base.hpp>
#include <genmax/unfxv1.hpp>

//' Find the hessian in terms of the untransformed parameters using chain rules

void unfixv(Rcpp::NumericMatrix &v,
            Rcpp::NumericVector &g,
            Rcpp::NumericVector &thetas,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            int &idim){
   
Rcpp::NumericVector igs = Rcpp::NumericVector(idim);
Rcpp::NumericVector igd = Rcpp::NumericVector(idim);

unfxv1(v,g,thetas,kodet,nparm,idim,igs,igd);

return;
   
}

#include <base/base.hpp>

//'   #find the hessian in terms of the untransformed parameters
//'   #using chain rules
//'   #we do not worry about the percentile translation problem here
//' xx#because we can expect g (the grad vector) to be 0, we
//'   #may want to eliminate it here

void unfxv1(Rcpp::NumericMatrix &v,
            Rcpp::NumericVector &g,
            Rcpp::NumericVector &thetas,
            Rcpp::IntegerVector &kodet,
            int &nparm,
            int &idim,
            Rcpp::NumericVector &gs,
            Rcpp::NumericVector &gd){
   
int kodep,jm1;
double p;
   
for(int j = 1; j <= nparm; j++){
   
    kodep = kodet.at(j - 1) + 1;
   
    if(kodep == 1) {
       
       gs.at(j - 1) = one;
       gd.at(j - 1) = zero;
       
    }
    
    if(kodep == 2) {
       
       gs.at(j - 1) = one;
       gd.at(j - 1) = zero;
       
    }
    
    if(kodep == 3) {
       
       gs.at(j - 1) = one / thetas.at(j - 1);
       gd.at(j - 1) = -1 * gs.at(j - 1) / thetas.at(j - 1);
       
    }
    
    if(kodep == 4) {
       
       p = thetas.at(j - 1);
       gs.at(j - 1) = one / (p * (one - p));
       gd.at(j - 1) = gs.at(j - 1) * gs.at(j - 1) * (one - two * p);
       
    }
    
    if(kodep == 5) {
       
       gs.at(j - 1) = one;
       gd.at(j - 1) = zero;
       
    }
    
    if(kodep == 6) {
       
       gs.at(j - 1) = one;
       gd.at(j - 1) = zero;
       
    }
    
    jm1 = j - 1;
    
    if(jm1 != 0) {
       
       for(int i = 1; i <= jm1; i++){
          
           v.at(i - 1,j - 1) = v.at(i - 1,j - 1) * gs.at(i - 1) * gs.at(j - 1);
           v.at(j - 1,i - 1) = v.at(i - 1,j - 1);
          
       }
         
    }
    
    v.at(j - 1,j - 1) = std::pow(gs.at(j - 1),2) * v.at(j - 1,j - 1) + g.at(j - 1) * gd.at(j - 1);
         
}

return;
      
}
