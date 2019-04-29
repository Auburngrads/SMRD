#include <base/base.hpp>

//' Set powell convergence parameters (max step size is 1.00)

void gpowp(Rcpp::NumericVector &e,
           double &escale,
           double &stepmx,
           double &tol,
           int &nparm){
   
for(int i = 1; i <= nparm; i++){
   
    e.at(i - 1) = tol;
   
}

escale = stepmx / tol;
   
return;
   
}