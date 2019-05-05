#include <base/base.hpp>

//' Compute weighted ybar and ysd

void wqm_rdesc(Rcpp::NumericVector &y,
               Rcpp::IntegerVector &wt,
               int &nrow,
               double &ybar,
               double &ysd){
  
int xnrow = 0;

for(int i = 0; i < nrow; i++){
  
    xnrow = xnrow + wt.at(i);
    ybar  = ybar + y.at(i) * wt.at(i);
  
}

ybar = ybar / xnrow;

for(int i = 0; i < nrow; i++){
  
    ysd = ysd + wt.at(i) * std::pow(y.at(i) - ybar,2);
  
}

ysd = std::sqrt( ysd / (xnrow - 1));

return;

}
