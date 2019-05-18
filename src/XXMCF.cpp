#include <base/base.hpp>
#include <xxmcf/setup_winmcfdata.hpp>
#include <xxmcf/compute_winmcf.hpp>

//' Computes the sample MCF and its robust sample 
//' variance. Created by Huaiqing Wu & William Q. 
//' Meeker, Iowa State University
// [[Rcpp::export]]
Rcpp::List XXMCF(int numrecurr, 
                 Rcpp::NumericVector timeofrecurr, 
                 Rcpp::IntegerVector krecurrid,
                 Rcpp::NumericVector dcost, 
                 int muniqrecurr, 
                 Rcpp::NumericVector tuniq, 
                 Rcpp::IntegerVector apoint,
                 int lnumrecurr, 
                 Rcpp::IntegerVector delta, 
                 int nunitsgroups, 
                 Rcpp::IntegerVector wpoint,
                 int nwindows, 
                 Rcpp::NumericVector twindowsl, 
                 Rcpp::NumericVector twindowsu, 
                 Rcpp::IntegerVector wcounts,
                 Rcpp::IntegerVector inwindowj, 
                 Rcpp::NumericVector muhat, 
                 Rcpp::NumericVector varhat, 
                 Rcpp::NumericVector dbar,
                 Rcpp::IntegerVector iordl, 
                 Rcpp::IntegerVector iordu, 
                 Rcpp::IntegerVector iorder, 
                 Rcpp::IntegerVector iscrat,
                 int kdebug){

debug::kprint = kdebug;
  
// Set up the mcf input data structure
   setup_winmcfdata(numrecurr, timeofrecurr, krecurrid,
                    dcost, muniqrecurr, tuniq, lnumrecurr, 
                    apoint, iorder, iscrat);

// Compute the mcf
   compute_winmcf(muniqrecurr, tuniq, delta, apoint,
                  lnumrecurr, krecurrid, dcost, nunitsgroups,
                  wpoint, nwindows, twindowsl, twindowsu,
                  wcounts, iordl, iordu, inwindowj, muhat,
                  varhat, dbar, iscrat, iorder);

  return Rcpp::List::create(Named("varhat")      = varhat,
                            Named("krecurrid")   = krecurrid,
                            Named("dcost")       = dcost,
                            Named("muhat")       = muhat,
                            Named("dbar")        = dbar,
                            Named("tuniq")       = tuniq,
                            Named("muniqrecurr") = muniqrecurr,
                            Named("apoint")      = apoint,
                            Named("lnumrecurr")  = lnumrecurr,
                            Named("delta")       = delta,
                            Named("inwindowj")   = inwindowj,
                            Named("iordl")       = iordl,
                            Named("iordu")       = iordu,
                            Named("iorder")      = iorder,
                            Named("RecurrTimes") = timeofrecurr,
                            Named("twindowsl")   = twindowsl,
                            Named("twindowsu")   = twindowsu,
                            Named("nwindows")    = nwindows,
                            Named("wcounts")     = wcounts);
}


#include <base/base.hpp>
#include <riskset/wqm_riskset.hpp>
#include <xxmcf/icheckwin.hpp>
#include <xxmcf/pairrisk.hpp>

// subroutine to compute the sample mcf and its robust sample variance;
// based on methodology given in nelson (1988, 1995),;
// and lawless and nadeau (1995) and extended by wu and meeker (2004);
// this algorithm is designed specifically for large populations;
// involving a large number of observed units;
// (up tp 100s of thousands or more), and with a large number;
// of reports (thousands to 100s of thousands or more);
// huaiqing wu;
// william q. meeker;
// iowa state university;
// inputs;
// muniqrecurr number of unique recurrence times;
// tuniq(muniqrecurr) unique recurrence times (in increasing order);
// # j=1,...,muniqrecurr;
// apoint(muniqrecurr) pointers to the jth recurrence time;
// # group in krecurrid and dcost;
// # apoint(j-1)+1 to apoint(j) are indices for;
// # the recurrences at unique time j;
// lnumrecurr total number of recurrences (including ties);
// krecurrid(lnumrecurr) ids of the recurrences at time tuniq(j);
// # these records must be ordered by time,;
// # and unit within time;
// dcost(lnumrecurr) dcost(j) is the costs of the recurrence;
// # corresponding to id at krecurrid(j);
// nunitsgroups the number of unique units and groups;
// # in the data set;
// wpoint(nunitsgroups) pointers into twindowsl and twindowsu;
// # wpoint(i-1)+1 to wpoint(i) are indices for;
// # the windows for unit/group i;
// # these pointers allow us to identify windows;
// # with units/groups;
// inwindowj(nunitsgroups) element i indicates whether the current;
// # recurrence time is in an observation window;
// # for unit i;
// nwindows the length of twindowsl and twindowsu;
// twindowsl(nwindows) vector giving observation window start points;
// # for individual windows or groups;
// twindowsu(nwindows) end points corresponding to twindowsl;
// wcounts(nwindows) counts for the windows mapped and replicated;
// # from units/groups;
// outputs;
// muhat(muniqrecurr) sample mcf at time tuniq(j);
// varhat(muniqrecurr) robust estimate of variance of the mcf estimate;
// delta(muniqrecurr) size of the risk set just before time tuniq(j);
// scratch space;
// dbar(muniqrecurr) sample mean (mcf jump-height) at tuniq(j);
// iordl(nwindows) windows ordering vector;
// iordu(nwindows) windows ordering vector;
// iscrat(nwindows) scratch needed for sorting;
// important local variables;
// ij, istartj, iendj indices taken from pointer for time tuniq(j);
// ik, istartk, iendk indices taken from pointer for time tuniq(k);
// vardbar estimate of the variance of dbar(j);
// dbarjk the average value of all units at time t_j,;
// # computed over only those units that are alive;
// # at both times tj and tk;
// covdjdk estimate of the covariance;
// # between dbar(j) and dbar(k);
// covterms sum of column j of the covdjdk upper-diagonal;
// # covariance matrix;
void compute_winmcf(int &muniqrecurr, 
                    Rcpp::NumericVector &tuniq, 
                    Rcpp::IntegerVector &delta, 
                    Rcpp::IntegerVector &apoint,
                    int &lnumrecurr, 
                    Rcpp::IntegerVector &krecurrid, 
                    Rcpp::NumericVector &dcost, 
                    int &nunitsgroups, 
                    Rcpp::IntegerVector &wpoint,
                    int &nwindows, 
                    Rcpp::NumericVector &twindowsl, 
                    Rcpp::NumericVector &twindowsu, 
                    Rcpp::IntegerVector &wcounts, 
                    Rcpp::IntegerVector &iordl, 
                    Rcpp::IntegerVector &iordu,
                    Rcpp::IntegerVector &inwindowj, 
                    Rcpp::NumericVector &muhat, 
                    Rcpp::NumericVector &varhat, 
                    Rcpp::NumericVector &dbar, 
                    Rcpp::IntegerVector &iscrat, 
                    Rcpp::IntegerVector &iorder){

double vardbar, covterms, covdjdk, sunit;
double dbarjk;
int jm1, istartj, iendj;
int istartk, iendk, deltajk;
int inwindowk; //kount00

if(debug::kprint > 0) {
  
   Rcpp::Rcout << "\nCOMPUTE_WINMCF***1***\n" << std::endl;

   Rcpp::Rcout << "\nmuniqrecurr\n" << std::endl;
   Rcpp::Rcout << "delta      = " << delta << std::endl;
   Rcpp::Rcout << "apoint     = " << apoint << std::endl;
   Rcpp::Rcout << "tuniq      = " << tuniq << std::endl;

   Rcpp::Rcout << "\nlnumrecurr\n" << std::endl;
   Rcpp::Rcout << "krecurrid  = " << krecurrid << std::endl;
   Rcpp::Rcout << "dcost      = " << dcost << std::endl;

   Rcpp::Rcout << "\nnunitsgroups\n" << std::endl;
   Rcpp::Rcout << "wpoint     = " << wpoint << std::endl;

   Rcpp::Rcout << "\nnwindows\n" << std::endl;
   Rcpp::Rcout << "twindowsl  = " << twindowsl << std::endl;
   Rcpp::Rcout << "twindowsu  = " << twindowsu << std::endl;

}

// compute the number in the riskset;
   wqm_riskset(muniqrecurr, tuniq, nwindows,
               twindowsl, twindowsu, wcounts, 
               iordl, iordu, delta, iorder);

if(debug::kprint > 0) {
  
   Rcpp::Rcout << "\nCOMPUTE_WINMCF***2***\n" << std::endl;

   Rcpp::Rcout << "\nmuniqrecurr\n" << std::endl;
   Rcpp::Rcout << "delta      = " << delta << std::endl;
   Rcpp::Rcout << "apoint     = " << apoint << std::endl;
   Rcpp::Rcout << "tuniq      = " << tuniq << std::endl;

   Rcpp::Rcout << "\nlnumrecurr\n" << std::endl;
   Rcpp::Rcout << "krecurrid  = " << krecurrid << std::endl;
   Rcpp::Rcout << "dcost      = " << dcost << std::endl;

   Rcpp::Rcout << "\nnunitsgroups\n" << std::endl;
   Rcpp::Rcout << "wpoint     = " << wpoint << std::endl;

   Rcpp::Rcout << "\nnwindows\n" << std::endl;
   Rcpp::Rcout << "twindowsl  = " << twindowsl << std::endl;
   Rcpp::Rcout << "twindowsu  = " << twindowsu << std::endl;

}


// begin computation of the sample mcf and its sample variance;
// loop over the unique recurrence times;
   istartj = 1;

// unique_times:
for(int j = 1; j <= muniqrecurr; j++){

// get indices for units with an recurrence at time t_j;
   if(j > 1) istartj = apoint.at(j - 2) + 1;
   iendj = apoint.at(j - 1);

// loop over the individual units/groups to;
// determine whether the current recurrence
// time tuniq(j) is in at least one of the
// observation windows corresponding to
// unit/group index;
   for(int index = 1; index <= nunitsgroups; index++){
   
       inwindowj.at(index - 1) = icheckwin(tuniq.at(j - 1),index,nunitsgroups, 
                                           wpoint,nwindows,twindowsl,twindowsu);
     
       if(debug::kprint > 6){
         
          Rcpp::Rcout << "\nj,tj,index,inwindowj(index)\n" << std::endl;
          Rcpp::Rcout << "j                = " << j - 1 << std::endl;
          Rcpp::Rcout << "tuniq(j)         = " << tuniq.at(j - 1) << std::endl;
          Rcpp::Rcout << "index            = " << index << std::endl;
          Rcpp::Rcout << "inwindowj(index) = " << inwindowj.at(index - 1) << std::endl;
         
       }
   
   }
   
// compute dbar(j), the sample mean jump size at time tuniq(j);
   dbar.at(j - 1) = zero;

// sum of increments
   for(int ij = istartj; ij <= iendj; ij++) {
   
       dbar.at(j - 1) = dbar.at(j - 1) + dcost.at(ij - 1);
   
   }

   dbar.at(j - 1) = dbar.at(j - 1) / (float)delta.at(j - 1);
   
// compute the sample variance of dbar(j),;
// looping over units with recurrences at time t_j;
   vardbar = zero;

// sum of squares
   for(int ij = istartj; ij <= iendj; ij++){
   
       vardbar = vardbar + pow((dcost.at(ij - 1) - dbar.at(j - 1)),2);
   
   }

// add in stuff for units with no recurrences;
   vardbar = vardbar + (float)(delta.at(j - 1) - iendj + istartj - 1) * pow(dbar.at(j - 1),2);
   int kount00 = delta.at(j - 1) - iendj + istartj - 1;
   if(debug::kprint > 2){
     
      Rcpp::Rcout << "\nCOMPUTE_WINMCF: vardbar 1\n" << std::endl;
      Rcpp::Rcout << "j        = " << j - 1 << std::endl;
      Rcpp::Rcout << "istartj  = " << istartj << std::endl;
      Rcpp::Rcout << "iendj    = " << iendj << std::endl;
      Rcpp::Rcout << "delta(j) = " << delta.at(j - 1) << std::endl;
      Rcpp::Rcout << "kount00  = " << kount00 << std::endl;
      Rcpp::Rcout << "dbar(j)  = " << dbar.at(j - 1) << std::endl;
      Rcpp::Rcout << "vardbar  = " << vardbar << std::endl;
     
   }
 
// kount00 = (float)delta.at(j) - iendj + istartj - 1;
   vardbar = vardbar / ((float)delta.at(j - 1) * (float)delta.at(j - 1));
   if(debug::kprint > 1){
     
      Rcpp::Rcout << "\nCOMPUTE_WINMCF: vardbar 2\n" << std::endl;
      Rcpp::Rcout << "j        = " << j - 1 << std::endl;
      Rcpp::Rcout << "istartj  = " << istartj << std::endl;
      Rcpp::Rcout << "iendj    = " << iendj << std::endl;
      Rcpp::Rcout << "delta(j) = " << delta.at(j - 1) << std::endl;
      Rcpp::Rcout << "dbar(j)  = " << dbar.at(j - 1) << std::endl;
      Rcpp::Rcout << "vardbar  = " << vardbar << std::endl;
     
   }

// do the initilization or accumulations for the sample mcf;
// and its sample variance;
if(j == 1) {

   muhat.at(0) = dbar.at(0);
   varhat.at(0) = vardbar;

 } else {

   muhat.at(j - 1) = muhat.at(j - 2) + dbar.at(j - 1);
   covterms = zero;
   istartk = 1;
   jm1 = j - 1;

// if j > 1, loop over the unique times that are less than the current t_j;
// to compute the sample covariance between dbar(j) and dbar(k);
for(int k = 1; k <= jm1; k++){

// get indices for units with an recurrence at time t_k;
if(k > 1) istartk = apoint.at(k - 2) + 1;

iendk = apoint.at(k - 1);

// compute the size of the risk set just before;
// the recurrence at t_j counting among only the units;
// that were also under observation at time t_k < t_j;
   deltajk = pairrisk(k, inwindowj, tuniq, muniqrecurr,
                      nunitsgroups, wpoint, nwindows,
                      twindowsl, twindowsu, wcounts);

   if(debug::kprint > 3){
     
      Rcpp::Rcout << "\nCOMPUTE_WINMCF: after pairrisk\n" << std::endl;
      Rcpp::Rcout << "j       = " << j << std::endl; 
      Rcpp::Rcout << "istartj = " << istartj << std::endl; 
      Rcpp::Rcout << "iendj   = " << iendj << std::endl; 
      Rcpp::Rcout << "k       = " << k << std::endl; 
      Rcpp::Rcout << "istartk = " << istartk << std::endl; 
      Rcpp::Rcout << "iendk   = " << iendk << std::endl; 
      Rcpp::Rcout << "deltajk = " << deltajk << std::endl; 
     
   }

// if there were some units at risk at both t_j and t_k,;
// the we compute a covariance term for the pair;
// otherwise, assume it is zero;
   covdjdk = zero;

if(deltajk > 0) {

// compute the sample mean of all units that had a recurrence at t_j;
// and that were also under observation at time t_k;
   dbarjk = zero;

// sum_of_increments_tj_tk:
   for(int ij = istartj; ij <= iendj; ij++){
   
   // check to see if tuniq(k) in one of the windows of krecurrid(ij);
      inwindowk = icheckwin(tuniq.at(k - 1),krecurrid.at(ij - 1),
                            nunitsgroups, wpoint, nwindows,
                            twindowsl, twindowsu);
   
       if(inwindowk > 0) dbarjk = dbarjk + dcost.at(ij - 1);
       
       if(debug::kprint > 2){
         
          Rcpp::Rcout << "\nCOMPUTE_WINMCF: after icheckwin\n" << std::endl;
          Rcpp::Rcout << "j             = " << j - 1 << std::endl;
          Rcpp::Rcout << "k             = " << k - 1 << std::endl;
          Rcpp::Rcout << "ij            = " << ij - 1 << std::endl;
          Rcpp::Rcout << "krecurrid(ij) = " << krecurrid.at(ij - 1) << std::endl;
          Rcpp::Rcout << "inwindowk     = " << inwindowk << std::endl;
          Rcpp::Rcout << "deltajk       = " << deltajk << std::endl;
          Rcpp::Rcout << "tuniq(k)      = " << tuniq.at(k - 1) << std::endl;
          Rcpp::Rcout << "dcost(ij)     = " << dcost.at(ij - 1) << std::endl;
          Rcpp::Rcout << "dbarjk        = " << dbarjk << std::endl;
         
       }
   
   }

dbarjk = dbarjk / (float)deltajk;

if(debug::kprint > 1){
  
   Rcpp::Rcout << "\nj,istj, iej, k,istk,iek, delj,dbarjk\n" << std::endl;
   Rcpp::Rcout << "j       = " << j - 1 << std::endl;
   Rcpp::Rcout << "istartj = " << istartj << std::endl;
   Rcpp::Rcout << "iendj   = " << iendj << std::endl;
   Rcpp::Rcout << "k       = " << k - 1 << std::endl;
   Rcpp::Rcout << "istartk = " << istartk << std::endl;
   Rcpp::Rcout << "iendk   = " << iendk << std::endl;
   Rcpp::Rcout << "deltajk = " << deltajk << std::endl;
   Rcpp::Rcout << "dbarjk  = " << dbarjk << std::endl;

}
   
// loop over the units that had an recurrence at time t_k;
for(int ik = istartk; ik <= iendk; ik++){

    sunit = zero;
    // add this part only if t_j is in one of the windows;
    // corresponding to recurrence ik;
    if(inwindowj.at(krecurrid.at(ik - 1) - 1) > 0) {

    // loop over the units that had an recurrence at t_j;
    for(int ij = istartj; ij <= iendj; ij++){

        // accumulate extra terms for recurrences at t_j and t_k;
        // that are within the same unit;
        if(krecurrid.at(ik - 1) == krecurrid.at(ij - 1)) {

           sunit = sunit + dcost.at(ij - 1);

        }

    }

    covdjdk = covdjdk + dcost.at(ik - 1) * (sunit - dbarjk);

    }
    
    if(debug::kprint > 1){
  
       Rcpp::Rcout << "\nj,tuniq(j),k,ik,krecurrid(ik),dc,cdjdk,covt\n" << std::endl;
       Rcpp::Rcout << "j             = " << j - 1 << std::endl;
       Rcpp::Rcout << "tuniq(j)      = " << tuniq(j - 1) << std::endl;
       Rcpp::Rcout << "k             = " << k - 1 << std::endl;
       Rcpp::Rcout << "ik            = " << ik - 1 << std::endl;
       Rcpp::Rcout << "krecurrid(ik) = " << krecurrid.at(ik - 1) << std::endl;
       Rcpp::Rcout << "dcost(ik)     = " << dcost.at(ik - 1) << std::endl;
       Rcpp::Rcout << "covdjdk       = " << covdjdk << std::endl;
       Rcpp::Rcout << "covterms      = " << covterms << std::endl;  
  
    }

}

covdjdk = covdjdk / ((float)delta.at(j - 1) * (float)delta.at(k - 1));

}

covterms = covterms + covdjdk;

}

// add in the variance and covariance terms for t_j;
   varhat.at(j - 1) = varhat.at(j - 2) + vardbar + two * covterms;

 }

if(debug::kprint > 0){
  
   Rcpp::Rcout << "\n*ans*j,tj,delj,muj,dbarj,vdb,covt,varj\n" << std::endl;
   Rcpp::Rcout << "j         = " << j - 1 << std::endl;
   Rcpp::Rcout << "tuniq(j)  = " << tuniq.at(j - 1) << std::endl;
   Rcpp::Rcout << "delta(j)  = " << delta.at(j - 1) << std::endl;
   Rcpp::Rcout << "muhat(j)  = " << muhat.at(j - 1) << std::endl;
   Rcpp::Rcout << "dbar(j)   = " << dbar.at(j - 1) << std::endl;
   Rcpp::Rcout << "vardbar   = " << vardbar << std::endl;
   Rcpp::Rcout << "covterms  = " << covterms << std::endl;
   Rcpp::Rcout << "varhat(j) = " << varhat.at(j - 1) << std::endl;
  
}
 
}

return;

}



#include <base/base.hpp>

// Check to see if tuniq is in one of the 
// windows corresponding to the unit/group index
int icheckwin(double tuniq, 
              int index, 
              int nunitsgroups,
              Rcpp::IntegerVector wpoint, 
              int nwindows, 
              Rcpp::NumericVector twindowsl, 
              Rcpp::NumericVector twindowsu){

int icheck_win = 0, istart, iend;

// get the start and end window pointers for the krecurrid id;
istart = wpoint.at(index - 1);

if(index == nunitsgroups) {
  
   iend = nwindows;

 } else {
  
   iend = wpoint.at(index + 1 - 1) - 1;

}
 
// loop across the windows for the unit with id krecurrid;
for(int i = istart; i <= iend; i++){
  
    // return with 1 (true) as soon as we find tuniq in a window;
    if((tuniq >= twindowsl.at(i - 1)) and (twindowsu.at(i - 1) >= tuniq)) {
      
        icheck_win = 1;
        return icheck_win;
        
    }
    
}

return icheck_win;
 
}



#include <base/base.hpp>
#include <xxmcf/icheckwin.hpp>

// Compute the size of the pairwise risk set for a
// recurrence process with window observation.
// 
// output;
// pairrisk the number of units under observation;
// at both just before time tuniq(j);
// and just before time tuniq(k), k = 1, ..., j;
int pairrisk(int k, 
             Rcpp::IntegerVector inwindowj, 
             Rcpp::NumericVector tuniq, 
             int muniqrecurr,
             int nunitsgroups,
             Rcpp::IntegerVector wpoint, 
             int nwindows, 
             Rcpp::NumericVector twindowsl, 
             Rcpp::NumericVector twindowsu, 
             Rcpp::IntegerVector wcounts){
  
int inwindowk, wcounti, pair_risk = 0, iwinindex;
  
// loop over the individual units/groups to;
// determine whether the current recurrence times;
// tuniq(j) and tuniq(k) are both in;
// observation windows corresponding to unit/group krecurrid(i);

for(int i = 1; i <= nunitsgroups; i++){
  
    if(inwindowj.at(i - 1) > 0) {
      
       inwindowk = icheckwin(tuniq.at(k - 1),i, nunitsgroups, 
                             wpoint, nwindows, twindowsl, 
                             twindowsu);
       
       if(inwindowk > 0) {
         
          iwinindex = wpoint.at(i - 1);
          wcounti   = wcounts.at(iwinindex - 1);
          pair_risk  = pair_risk + inwindowj.at(i - 1) * inwindowk * wcounti;
       
       }
    
    }

}

return pair_risk;

}


#include <base/base.hpp>
#include <utility/merge_sortdd.hpp>
#include <utility/merge_sortii.hpp>
#include <xxmcf/reorderd.hpp>
#include <xxmcf/reorderi.hpp>
#include <xxmcf/wqm_uniqued.hpp>

//       subroutine to setup for mcf computation:
//
//     1. Order the recurrences by time and id within time
//     2. Find the unique, ordered recurrence times
//     3. Collapse ties (same unit, same time)
//     #        into one recurrence with total cost
//     4. Make the apoint pointers
//
//  inputs:
//
//     numrecurr               number of recurrences, including ties
//     timeofrecurr(numrecurr) recurrence times
//     krecurrid(numrecurr)    unit id of the recurrence
//     dcost(numrecurr)        cost of the recurrence
//
//        the above must be ordered by time, with id within time
//
// outputs:
//
//     muniqrecurr             number of unique recurrence times
//     tuniq(numrecurr)      unique recurrence times (in increasing order)
//     lnumrecurr              number of recurrences after within-unit
//                                 ties (if any) are combined
//
//     apoint(numrecurr)     pointers to the jth recurrence time-alike
//                               group in krecurrid and dcost
//                               apoint(j-1)+1 to apoint(j) are indices for
//                               the reports at time tuniq(j)
//
// scratch:
//     iscrat(numrecurr)
//     iorder(numrecurr)    vector to indicate the order of the recurrences
void setup_winmcfdata(int &numrecurr, 
                      Rcpp::NumericVector &timeofrecurr,
                      Rcpp::IntegerVector &krecurrid, 
                      Rcpp::NumericVector &dcost, 
                      int &muniqrecurr, 
                      Rcpp::NumericVector &tuniq,
                      int &lnumrecurr, 
                      Rcpp::IntegerVector &apoint, 
                      Rcpp::IntegerVector &iorder, 
                      Rcpp::IntegerVector &iscrat){
  
double lasttime;
int j, i, lastid;

// find the needed ordering indices for the 
// recurrences first order with respect to unit id
   Rcpp::IntegerVector ISCRAT(numrecurr);
   merge_sortii(krecurrid, numrecurr, iorder, ISCRAT);

   if(debug::kprint > 6){
     
      Rcpp::Rcout << "\nSETUP_WINMCFDATA: iorder check 1\n" << std::endl;
      Rcpp::Rcout << "krecurrid = " << krecurrid << std::endl;
      Rcpp::Rcout << "iorder    = " << iorder << std::endl;
   }
// reorder order vector based on the times ordered by units
// signal with negative length
   Rcpp::IntegerVector ISCRAT2(numrecurr);
   merge_sortdd(timeofrecurr, -numrecurr, iorder, ISCRAT2);
   if(debug::kprint > 0){
     
      Rcpp::Rcout << "\ninitial setup_winmcfdata ordered\n" << std::endl;
      Rcpp::Rcout << "iorder       = " << iorder << std::endl;
      Rcpp::Rcout << "krecurrid    = " << krecurrid << std::endl;
      Rcpp::Rcout << "timeofrecurr = " << timeofrecurr << std::endl;
      Rcpp::Rcout << "dcost        = " << dcost   << std::endl;
     
   } 

//  reorder krecurrid, timeofrecurr, dcost according
//  to iorder using scratch in iscrat and tuniq
    reorderi(krecurrid, numrecurr, iorder, iscrat);

    reorderd(timeofrecurr, numrecurr, iorder, tuniq);

    reorderd(dcost, numrecurr, iorder, tuniq);

//	find the unique recurrence times; reuse iscrat as scratch
    wqm_uniqued(timeofrecurr, numrecurr, tuniq,
                muniqrecurr, iorder, iscrat);

    if(debug::kprint > 1){
      
       Rcpp::Rcout << "\nSETUP_WINMCF: after wqm_uniqed\n" << std::endl;
       Rcpp::Rcout << "timeofrecurr = " << timeofrecurr << std::endl;
       Rcpp::Rcout << "tuniq        = " << tuniq << std::endl; 
       Rcpp::Rcout << "krecurrid    = " << krecurrid << std::endl;
       Rcpp::Rcout << "dcost        = " << dcost << std::endl;
      
    } 

// combine the tied recurrences within the same unit
   if(debug::kprint > 0){
     
      Rcpp::Rcout << "\nbefore collapse setup_winmcfdata\n" << std::endl;
      Rcpp::Rcout << "krecurrid    = " << krecurrid << std::endl;
      Rcpp::Rcout << "timeofrecurr = " << timeofrecurr << std::endl;
      Rcpp::Rcout << "dcost        = " << dcost << std::endl;

   } 

   lasttime = timeofrecurr.at(0);
   lastid = krecurrid.at(0);
   lnumrecurr = 1;

for(i = 2; i <= numrecurr; i++){

//	if the id and the time agree,
//  combine into one observation with double cost

	   if((krecurrid.at(i - 1) == lastid) and	(timeofrecurr.at(i - 1) == lasttime)) {

	       dcost.at(lnumrecurr - 1) = dcost.at(lnumrecurr - 1) + dcost.at(i - 1);

	    } else {

	       lnumrecurr = lnumrecurr + 1;
	       dcost.at(lnumrecurr - 1) = dcost.at(i - 1);
	      

	   }

	   krecurrid.at(lnumrecurr - 1)    = krecurrid.at(i - 1);
	   timeofrecurr.at(lnumrecurr - 1) = timeofrecurr.at(i - 1);
	   lastid   = krecurrid.at(i - 1);
	   lasttime = timeofrecurr.at(i - 1);

	   if(debug::kprint > 2){
	     
	      Rcpp::Rcout << "\ncollapse setup_winmcfdata\n" << std::endl;
        Rcpp::Rcout << "i               = " << i - 1 << std::endl;
        Rcpp::Rcout << "lnumrecurr      = " << lnumrecurr << std::endl;
        Rcpp::Rcout << "krecurrid(i)    = " << krecurrid.at(i - 1) << std::endl;
        Rcpp::Rcout << "muniqrecurr     = " << muniqrecurr << std::endl;
        Rcpp::Rcout << "timeofrecurr(i) = " << timeofrecurr.at(i - 1) << std::endl;
        Rcpp::Rcout << "dcost(i)        = " << dcost.at(i - 1) << std::endl;

	   } 

	}

// set up pointers from the unique times to the individual recurrences

	j = 1;

	for(i = 1; i <= muniqrecurr; i++){

	    while((j <= lnumrecurr) and (tuniq.at(i - 1) > timeofrecurr.at(j - 1))) {

	           j = j + 1;

	    }

	    if(i > 1) apoint(i - 2) = j - 1;
	    
	    if((i > 1) and (debug::kprint > 1)){
	      
	       Rcpp::Rcout << "\nAPOINT\n" << std::endl;
	       Rcpp::Rcout << "i             = " << i - 1 << std::endl;
	       Rcpp::Rcout << "apoint(i - 1) = " << apoint.at(i - 2) << std::endl;
	       Rcpp::Rcout << "tuniq(i)      = " << tuniq.at(i - 1) << std::endl;
	      
	    } 

	}

	apoint.at(muniqrecurr - 1) = lnumrecurr;
	
return;
	
}



#include <base/base.hpp>
#include <utility/wqm_copyd.hpp>

void reorderd(Rcpp::NumericVector &dinvec,
              int &n,
              Rcpp::IntegerVector &iorder,
              Rcpp::NumericVector &dscrat){
  
if(n > 0) {
  
   for(int i = 1; i <= n; i++){
     
   dscrat.at(i - 1) = dinvec.at(iorder.at(i - 1) - 1);
   
   }
   
}

wqm_copyd(dscrat, dinvec, n);

return;

}



#include <base/base.hpp>
#include <utility/wqm_copyi.hpp>

void reorderi(Rcpp::IntegerVector &invec,
              int &n,
              Rcpp::IntegerVector &iorder,
              Rcpp::IntegerVector &iscrat){
  
if(n > 0) {
  
   for(int i = 1; i <= n; i++) {
   
       iscrat.at(i - 1) = invec.at(iorder.at(i - 1) - 1);
   
   }
   
}

wqm_copyi(iscrat, invec, n);
 
return;
   
}



#include <base/base.hpp>
#include <utility/merge_sortdd.hpp>

// @usage uniqd(dvec, n, duniq, nuniq, iorder, iscrat)
// @param y A double precision vector to be uniqed
// 
// @return \code{yuniq} Vector of the unique values
//         \code{nuniq} Number of unique values found
// 
// @details \code{iorder} is a scratch vector that 
//          should be the same length as the vector 
//          to be uniqued.
//          
//          \code{iscrat} is a scratch vector that 
//          should be the same length as the vector 
//          to be uniqued
//
void wqm_uniqued(Rcpp::NumericVector &y,
                 int &n,
                 Rcpp::NumericVector &yuniq,
                 int &nuniq,
                 Rcpp::IntegerVector &iorder,
                 Rcpp::IntegerVector &iscrat){
  
int index;
nuniq = 0;

if(n == 0) return;
     
// trivial is n = 1
   nuniq = 1;
   yuniq.at(0) = y.at(0);
   if(n == 1) return; 
     
// first get the ordering vector;
   Rcpp::IntegerVector ISCRAT(n);
   merge_sortdd(y, n, iorder, ISCRAT);
      
   for(int i = 1; i <= n; i++){
        
       index = iorder.at(i - 1);
     
       // save and increment if the new y is bigger;
          if(y.at(i - 1) <= yuniq.at(nuniq - 1)) continue;
          nuniq = nuniq + 1;
          yuniq.at(nuniq - 1) = y.at(index - 1);
          
   }

return;

}