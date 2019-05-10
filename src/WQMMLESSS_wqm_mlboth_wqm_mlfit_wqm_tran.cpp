#include <base/base.hpp>

//' Subtract out the threshhold parameter 
//' (for log distribution only), take logs of 
//' observations and do data checks for consistency
//'
//'  y,cen,wt,nrow,ny    as in wqm_mlboth
//'                      log of (y-gamthr) is returned in y
//'
//'  gamthr          input vector threshhold parameter
//'
//'  ltrunc          input =.true. if doing tuncation times
//'
//'  ltr3            return =.true. if we change an interval
//'
//'  ier             return error code
//'                          4             input time less than threshhold
//'                          8             threshhold less than zero

void wqm_tran(Rcpp::NumericMatrix &y,
              Rcpp::IntegerVector &cen,
              Rcpp::IntegerVector &wt,
              int &nrow,
              int &ny,
              Rcpp::NumericVector &gamthr,
              bool ltrunc,
              bool &ltr3,
              int &ier){

ier = 0;
int itype;
double ynew;
  
for(int i = 1; i <= nrow; i++){
  
    itype = cen.at(i - 1);

    // Don't do anything if a dummy observation

    if((itype == 0) or (wt.at(i - 1) <= 0)) continue;

    // Don't signal error or do anything else if we are
    // doing truncation but there is no truncation for 
    // this case

    if((ltrunc) and ((itype == 1) or (itype == 0))) continue;

    // Go over columns of y (if more than one)

    for(int j = 1; j <= ny; j++){
      
        if((itype != 4) or (y.at(i - 1,0) > gamthr.at(i - 1)) or (j == 2)) goto line50;

        // Here lower limit of an observation or a truncation
        // interval is 0; make it into a left censored
        // observation and mark the action by setting ltr3 = true

        ltr3 = true;
        cen.at(i - 1) = 3;
        y.at(i - 1,0) = y.at(i - 1,ny - 1);
        line50: ynew = y.at(i - 1,j - 1);
                y.at(i - 1,j - 1) = -1.0e25;

        // If a censored observation is less than gamthr, 
        // ignore and return log(0+)

        if((ynew <= zero) and (itype == 2)) continue;
        
           if(ynew <= zero) { ier = 4; return; }
           
           ynew = std::log(ynew);
   
           // Subtract out gamthr and take logs
   
           y.at(i - 1,j - 1) = ynew - gamthr.at(i - 1);
        
    }
}

return;
        
}
