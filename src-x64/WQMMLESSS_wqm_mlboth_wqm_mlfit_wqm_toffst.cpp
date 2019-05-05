#include <base/base.hpp>

//' Subtract out the offset when not taking logs
//' y,nrow,ny as in wqm_mlboth.
//' 
//' y - gamthr is returned in y, where gamthr is the
//' input vector threshhold parameter

void wqm_toffst(Rcpp::NumericVector &gamthr,
                Rcpp::NumericMatrix &y,
                int &nrow,
                int &ny){

double ynew = 0.0e00;
  
for(int i = 0; i < nrow; i++){

    // Go over columns of y (if more than one)

    for(int j = 0; j < ny; j++){

        // Subtract out gamthr and take logs
        ynew = y.at(i,j) - gamthr.at(i);
      
        // Compute new y(i,j)
        y.at(i,j) = ynew;
    }
}

    return;

}