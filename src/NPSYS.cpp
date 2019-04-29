#include <base/base.hpp>

//'  Compute the nonparametric estimate of the
//'  mean cumulative occurence rate of failures
//'  and corresponding estimate of the var if the estimator
//' 
//' 
//'   inputs:
//'         itime(nobs)       time indices   (from 1,2,..., nfailt)
//'                           gives index pointing to the vector of unique times
//'         isys(nobs)         system indices (from 1,2,..., nsys)
//'                           gives index pointing to the vector of unique sys ids
//'         icodes(nobs)       censor/failure indicator (1 fail, 2 censor)
//'         weight(nobs)       weight (multiplicity) or cost of failure
//'         nobs               number of observations in data set
//'         nfailt             number of unique failure times
//'         nsys               number of unique system ids
//'         utime(nfailt)     list of unique times
//'         iusys(nsys)        list of unique system ids
//' 
//' 
//' 
//'         ctime(nsys),varsum(nsys) scratch arrays
//'         dead(nfailt,nsys),idelta(nfailt,nsys) scratch arrays
//'         dsum(nfailt),idlsum(nfailt),dbar(nfailt)   scratch arrays
//' 
//' 
//'   outputs:
//'         xmuhat(nfailt)   estimate of mu at each failure time
//'         varxmu(nfailt)   estimate of var(muhat) at each failure time
// [[Rcpp::export]]
Rcpp::List npsys(Rcpp::IntegerVector itime,
                 Rcpp::IntegerVector isys,
                 Rcpp::IntegerVector icodes,
                 Rcpp::IntegerVector weight,
                 int nobs,
                 int nfailt,
                 int nsys,
                 Rcpp::NumericVector utime,
                 Rcpp::IntegerVector iusys,
                 Rcpp::NumericVector ctime,
                 Rcpp::NumericMatrix dead,
                 Rcpp::IntegerMatrix idelta,
                 Rcpp::NumericVector dsum,
                 Rcpp::IntegerVector idlsum,
                 Rcpp::NumericVector dbar,
                 Rcpp::NumericVector varsum,
                 Rcpp::NumericVector xmuhat,
                 Rcpp::NumericVector varxmu,
                 int kprint){

debug::kprint = kprint;
  
double big = 1.0e35,small = 1.e-6,smdbar,ratio;
Rcpp::List ints,doubs,intvec,numvec;
int it,js;

// Zero the matrices and varsum accumulator
   for(int js = 1; js <= nsys; js++){
     
       varsum.at(js - 1) = zero;
       ctime.at(js - 1) = -one;
       
       for(int it = 1; it <= nfailt; it++){
         
           dead.at(it - 1,js - 1) = zero;
           idelta.at(it - 1,js - 1) = 0;
         
       }
           
   }

// Set up the matrices
   for(int iobs = 1; iobs <= nobs; iobs++){
     
       it = itime.at(iobs - 1);
       js = isys.at(iobs - 1);

       // Check if failure or censor indicator
          if(icodes.at(iobs - 1) == 1) {
            
             // Accumulate reported cost/failures
                dead.at(it - 1,js - 1) = dead.at(it - 1,js - 1) + weight.at(iobs - 1);
                continue;
            
          }
          
          if(icodes.at(iobs) != 2) continue;
       
       // Record the censoring time
          ctime.at(js - 1) = utime.at(it - 1);
   
   }

// Fix delta matrix
   for(int js = 1; js <= nsys; js++){
     
       // If no censoring time was reported for a system, set to big
       // ***this probably needs to be changed to something 
       // ***more robust or error return
          if(ctime.at(js - 1) <= zero) ctime.at(js - 1) = big;
          
          for(int it = 1; it <= nfailt; it++){
            
              idelta.at(it - 1,js - 1) = 0;
          
              // Set delta=1 if we are still observing system js at time it
                 if(utime.at(it - 1) <= ctime.at(js - 1) + small){
                   
                    idelta.at(it - 1,js - 1) = 1;
                   
                 }
          }
   }

// Accumulate
   for(int it = 1; it <= nfailt; it++){
     
       dsum.at(it - 1) = zero;
       idlsum.at(it - 1) = 0;
       
       for(int js = 1; js <= nsys; js++){
         
           dsum.at(it - 1) = dsum.at(it - 1) + dead.at(it - 1,js - 1) * (double)idelta.at(it - 1,js - 1);
           idlsum.at(it - 1) = idlsum.at(it - 1) + idelta.at(it - 1,js - 1);
         
       }
           
   }

// Average and accumulate variance terms
   smdbar = zero;
   for(int it = 1; it <= nfailt; it++){
     
       varxmu.at(it - 1) = zero;
       dbar.at(it - 1) = zero;
       
       if(debug::kprint >= 3){
         
          Rcpp::Rcout << "\nNPSYS: xx\n" << std::endl;
          Rcpp::Rcout << "it = "         << it - 1 << std::endl;
          Rcpp::Rcout << "idlsum(it) = " << idlsum.at(it - 1) << std::endl;
          Rcpp::Rcout << "dsum(it) = "   << dsum.at(it - 1) << std::endl;
          Rcpp::Rcout << "dbar(it) = "   << dbar.at(it - 1) << std::endl;
          Rcpp::Rcout << "smdbar = "     << smdbar << std::endl;
         
       }

       if(idlsum.at(it - 1) > 0) dbar.at(it) = dsum.at(it) / (double)(idlsum.at(it - 1));
       smdbar = smdbar + dbar.at(it - 1);
       xmuhat.at(it - 1) = smdbar;
         
       if(debug::kprint >= 3){
         
          Rcpp::Rcout << "\nNPSYS: yy\n" << std::endl;
          Rcpp::Rcout << "it = "         << it - 1 << std::endl;
          Rcpp::Rcout << "idlsum(it) = " << idlsum.at(it - 1) << std::endl;
          Rcpp::Rcout << "dsum(it) = "   << dsum.at(it - 1) << std::endl;
          Rcpp::Rcout << "dbar(it) = "   << dbar.at(it - 1) << std::endl;
          Rcpp::Rcout << "smdbar = "     << smdbar << std::endl;
         
       }
       
       for(int js = 1; js <= nsys; js++){
         
           ratio = zero;
         
           if(idlsum.at(it - 1) > 0){
              
              ratio = (double)idelta.at(it - 1,js - 1) / (double)idlsum.at(it - 1);
             
           }
           
           varsum.at(js - 1) = varsum.at(js - 1) + ratio * (dead.at(it - 1,js - 1) - dbar.at(it - 1));
           varxmu.at(it - 1) = varxmu.at(it - 1) + std::pow(varsum.at(js - 1), 2);

           // For return use idelta to make event points
              idelta.at(it - 1,js - 1) = 0;
              if(dead.at(it - 1,js - 1) > zero) idelta.at(it - 1,js - 1) = 1;
           
           // For return accumulate cost/failures
              if(it > 1) dead.at(it - 1,js - 1) = dead.at(it - 1,js - 1) + dead.at(it - 2,js - 1);
              
       }
              
   }

 ints = Rcpp::List::create(Named("nobs") = nobs,
                           Named("nfailt") = nfailt,
                           Named("nsys") = nsys);
   
 doubs = Rcpp::List::create(Named("smdbar") = smdbar,
                            Named("ratio") = ratio);

 intvec = Rcpp::List::create(Named("idlsum") = idlsum,
                             Named("itime") = itime,
                             Named("isys") = isys,
                             Named("icodes") = icodes,
                             Named("weight") = weight,
                             Named("iusys") = iusys,
                             Named("idelta") = idelta);

 numvec = Rcpp::List::create(Named("dead") = dead,
                             Named("dsum") = dsum,
                             Named("dbar") = dbar,
                             Named("varsum") = varsum,
                             Named("xmuhat") = xmuhat,
                             Named("varxmu") = varxmu,
                             Named("utime") = utime,
                             Named("ctime") = ctime);
  
return Rcpp::List::create(Named("ints") = ints,
                          Named("doubs") = doubs,
                          Named("intvec") = intvec,
                          Named("numvec") = numvec);
  
}