#include <base/base.hpp>
#include <utility/wqm_quant.hpp>

// Use simple unweighted least squares to get graphical 
// estimates of the location and scale parameters

void lspar(Rcpp::NumericVector &pplot,
           Rcpp::NumericVector &yplot,
           int &mplot,
           int &kdists,
           int &lscale,
           double &xloc,
           double &scale){

double x,xbar,ybar,sxx,sxy;
  
xbar = zero; 
ybar = zero; 
sxx = zero; 
sxy = zero; 

for(int i = 1; i <= mplot; i++){
  
    x = wqm_quant(pplot.at(i - 1),kdists);
    xbar = xbar + x;
    ybar = ybar + yplot.at(i - 1);
  
}

xbar = xbar / double(mplot);
ybar = ybar / double(mplot);

if((lscale == 0) or (mplot == 1)) goto line50;

for(int i = 1; i <= mplot; i++){
  
    x = wqm_quant(pplot.at(i - 1),kdists);
    sxx = sxx + std::pow((x - xbar),2);
    sxy = sxy + (x - xbar) * (yplot.at(i - 1) - ybar);
  
}

// Check to see if we have a line with positive slope
   if((sxy <= zero) or (sxx <= zero)) goto line50;
   
   scale = sxy / sxx;
   xloc = ybar - scale * xbar;
   return;
   
// Fixup for problems without scale or when there is a data problem
   line50: scale = one;
           xloc = ybar - xbar;
           
return;
      
}
