#include <base/base.hpp>
#include <utility/dmachine.hpp>

//**refer to dqage,dqagie,dqagpe,dqawse;
//**routines called (none);
//**revision date 810101 (yymmdd);
//**keywords sequential sorting;
//**author piessens,robert,appl. math. & progr. div. - k.u.leuven;
// de doncker,elise,appl. math. & progr. div. - k.u.leuven;
//**purpose this routine maintains the descending ordering in the;
// list of the local error estimated resulting from the;
// interval subdivision process. at each call two error;
// estimates are inserted using the sequential search;
// method, top-down for the largest error estimate and;
// bottom-up for the smallest error estimate.;
//**description;
// ordering routine;
// standard fortran subroutine;
// double precision version;
// parameters (meaning at output);
// limit - integer;
// maximum number of error estimates the list;
// can contain;
// Last - integer;
// number of error estimates currently in the list;
// maxerr - integer;
// maxerr points to the nrmax-th largest error;
// estimate currently in the list;
// ermax - double precision;
// nrmax-th largest error estimate;
// ermax = elist(maxerr);
// elist - double precision;
// vector of dimension Last containing;
// the error estimates;
// iord - integer;
// vector of dimension Last, the first k elements;
// of which contain pointers to the error;
// estimates, such that;
// elist(iord(1)),..., elist(iord(k));
// form a decreasing sequence, with;
// k = Last if Last <= (limit/2+2), and;
// k = limit+1-Last otherwise;
// nrmax - integer;
// maxerr = iord(nrmax);

void dqpsrt(int &limit,
            int &Last,
            int &maxerr,
            double &ermax,
            Rcpp::NumericVector &work,
            int &l3,
            Rcpp::IntegerVector &iord,
            int &nrmax){

double errmax,errmin;
int i = 1,ibeg,ido,isucc,jbnd,jupbn,k;
// Check whether the list contains more than two error estimates
   if(Last > 2) goto line10;
   iord.at(0) = 1;
   iord.at(1) = 2;
   goto line90;
   
// This part of the routine is only executed if, due to a
// difficult integrand, subdivision increased the error estimate. 
//
// In the normal case the insert procedure should start after the nrmax-th largest error estimate.;
   line10: errmax = work.at((l3 - 1) + maxerr - 1);
           if(nrmax == 1) goto line30;
           ido = nrmax - 1;

for(i = 1; i <= ido; i++){
    
    isucc = iord.at(nrmax - 2);
    // Jump out of do-loop***
    if(errmax <= work.at((l3 - 1) + isucc - 1)) goto line30;
    iord.at(nrmax - 1) = isucc;
    nrmax = nrmax - 1;
    
}

// Compute the number of elements in the list to be maintained in descending order. 
// This number depends on the number of subdivisions still allowed
   line30: jupbn = Last;
           if(Last > (std::floor(limit / 2) + 2)) jupbn = limit + 3 - Last;
           errmin = work.at((l3 - 1) + Last - 1);
           
// Insert errmax by traversing the list top-down,
// starting comparison from the element elist(iord(nrmax+1))
   jbnd = jupbn - 1;
   ibeg = nrmax + 1;
   if(ibeg <= jbnd) {
   
      for(i = ibeg; i <= jbnd; i++){
          
          isucc = iord.at(i - 1);
          // Jump out of do-loop***
          if(errmax >= work.at((l3 - 1) + isucc - 1)) goto line60;
          iord.at(i - 2) = isucc;
          
      }
      
   }
   
   iord.at(jbnd - 1) = maxerr;
   iord.at(jupbn - 1) = Last;
   goto line90;
   
// Insert errmin by traversing the list bottom-up
   line60: iord.at(i - 2) = maxerr;
           k = jbnd;
           
   for(int j = i; j <= jbnd; j++){
       
       isucc = iord.at(k - 1);
       // Jump out of do-loop***
       if(errmin < work.at((l3 - 1) + isucc - 1)) goto line80;
       iord.at(k) = isucc;
       k = k - 1;
       
   }
   
   iord.at(i - 1) = Last;
   goto line90;
   line80: iord.at(k) = Last;
   
// Set maxerr and ermax
   line90: maxerr = iord.at(nrmax - 1);
           ermax = work.at((l3 - 1) + maxerr - 1);

return;

}