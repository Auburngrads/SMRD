#include <base/base.hpp>
#include <utility/icompd.hpp>
#include <utility/wqm_copyi.hpp>

// uses xkey to make an order-index array iorder, using merge-sort;
// generalized to allow sorting based on an arbitrary comparison function;
// and adapted from michel olagnon's mrgref in orderpac;
// merge sort is a stable sorting algorithm;

void merge_sortD(Rcpp::NumericVector &xkey, 
                 int &nvalp, 
                 Rcpp::IntegerVector &iorder, 
                 Rcpp::IntegerVector &iscrat){

int lmtna, lmtnc;
int nval, iind, iwrkd, iwrk, iwrkf, jinda, iinda, iindb;
int kdump = 1;

nval = std::abs(nvalp);

if (nval == 0) return;

// fill-in the dummy index array if nvalp > 0;
// otherwise move around the iorder sent down;
// # (useful for ordering more than one vector);
   for(int i = 1; i <= nval; i++){
     
       if(nvalp > 0) {
         
          iscrat.at(i - 1) = i;
         
       } else {
         
          iscrat.at(i - 1) = iorder.at(i - 1);
       
       }

   }

// create ordered couples;
   for(int iind = 2; iind <= nval; iind = iind + 2){
     
       if (icompd(iscrat.at(iind - 2), iscrat.at(iind - 1), xkey) <= 0) {
         
           iorder.at(iind - 2) = iscrat.at(iind - 2);
           iorder.at(iind - 1) = iscrat.at(iind - 1);
       
         } else {
         
           iorder.at(iind - 2) = iscrat.at(iind - 1);
           iorder.at(iind - 1) = iscrat.at(iind - 2);
       
       }

   }

if ((nval % 2) != 0) iorder.at(nval - 1) = iscrat.at(nval - 1);

// we will now have ordered subsets a - b - a - b - ...;
// and merge a and b couples into c - c - ...;
   lmtnc = 2;
   lmtna = 2;

// iteration. each time, the length of the ordered subsets is doubled.;

while(true){
  
  if (lmtna >= nval) break;
  iwrkf = 0;
  lmtnc = 2 * lmtnc;
  iwrk = 0;

// loop on merges of a and b into c;
   while(true){
     
     iinda = iwrkf;
     iwrkd = iwrkf + 1;
     iwrkf = iinda + lmtnc;
     jinda = iinda + lmtna;
     if(iwrkf >= nval) {
       
        if(jinda >= nval) break;
        iwrkf = nval;
        
     }
     iindb = jinda;

// shortcut for the case when the max of a is smaller;
// than the min of b (no need to do anything);
   if(icompd(iorder.at(jinda - 1),iorder.at(jinda),xkey) <= 0) {
  
      iwrk = iwrkf;
      continue;
      
}

// one steps in the c subset, that we create in the final rank array;
   while(true){
  
   if(iwrk >= iwrkf) {
   
   // make a copy of the rank array for next iteration;
   // iorder (iwrkd:iwrkf) = iscrat (iwrkd:iwrkf);
      wqm_copyi(iscrat.at(iwrkd - 1), iorder.at(iwrkd - 1), iwrkf - iwrkd + 1);
      break;
   
   }

iwrk = iwrk + 1;

// we still have unprocessed values in both a and b;
if(iinda < jinda) {

   if(iindb < iwrkf) {

      if(icompd(iorder.at(iinda),iorder.at(iindb),xkey) > 0) {
        
         iindb = iindb + 1;
         iscrat.at(iwrk - 1) = iorder.at(iindb - 1);
         
      } else {
        
         iinda = iinda + 1;
         iscrat.at(iwrk - 1) = iorder.at(iinda - 1);
      
      }
    
   } else {

      // only a still has unprocessed values;
         iinda = iinda + 1;
         iscrat.at(iwrk - 1) = iorder.at(iinda - 1);
      
   }

} else {

  // only b still has unprocessed values;
  // iorder (iwrkd:iindb) = iscrat (iwrkd:iindb);
     wqm_copyi(iscrat.at(iwrkd - 1), iorder.at(iwrkd - 1), iindb - iwrkd + 1);
     iwrk = iwrkf;
     break;

}

}

}

// the cs become as and bs;
   lmtna = 2 * lmtna;

}

return;

}


#include <base/base.hpp>
#include <utility/icompi.hpp>
#include <utility/wqm_copyi.hpp>

// uses xkey to make an order-index array iorder, using merge-sort;
// generalized to allow sorting based on an arbitrary comparison function;
// and adapted from michel olagnon's mrgref in orderpac;
// merge sort is a stable sorting algorithm;

void merge_sortI(Rcpp::IntegerVector &xkey, 
                 int &nvalp, 
                 Rcpp::IntegerVector &iorder, 
                 Rcpp::IntegerVector &iscrat){

int lmtna, lmtnc;
int nval, iind, iwrkd, iwrk, iwrkf, jinda, iinda, iindb;
int kdump = 1;

nval = std::abs(nvalp);

if (nval == 0) return;

// fill-in the dummy index array if nvalp > 0;
// otherwise move around the iorder sent down;
// # (useful for ordering more than one vector);
   for(int i = 1; i <= nval; i++){
     
       if(nvalp > 0) {
         
          iscrat.at(i - 1) = i;
         
       } else {
         
          iscrat.at(i - 1) = iorder.at(i - 1);
       
       }

   }

// create ordered couples;
   for(int iind = 2; iind <= nval; iind = iind + 2){
     
       if (icompi(iscrat.at(iind - 2), iscrat.at(iind - 1), xkey) <= 0) {
         
           iorder.at(iind - 2) = iscrat.at(iind - 2);
           iorder.at(iind - 1) = iscrat.at(iind - 1);
       
         } else {
         
           iorder.at(iind - 2) = iscrat.at(iind - 1);
           iorder.at(iind - 1) = iscrat.at(iind - 2);
       
       }

   }

if ((nval % 2) != 0) iorder.at(nval - 1) = iscrat.at(nval - 1);

// we will now have ordered subsets a - b - a - b - ...;
// and merge a and b couples into c - c - ...;
   lmtnc = 2;
   lmtna = 2;

// iteration. each time, the length of the ordered subsets is doubled.;

while(true){
  
  if (lmtna >= nval) break;
  iwrkf = 0;
  lmtnc = 2 * lmtnc;
  iwrk = 0;

// loop on merges of a and b into c;
   while(true){
     
     iinda = iwrkf;
     iwrkd = iwrkf + 1;
     iwrkf = iinda + lmtnc;
     jinda = iinda + lmtna;
     if(iwrkf >= nval) {
       
        if(jinda >= nval) break;
        iwrkf = nval;
        
     }
     iindb = jinda;

// shortcut for the case when the max of a is smaller;
// than the min of b (no need to do anything);
   if(icompi(iorder.at(jinda - 1),iorder.at(jinda),xkey) <= 0) {
  
      iwrk = iwrkf;
      continue;
      
}

// one steps in the c subset, that we create in the final rank array;
   while(true){
  
   if(iwrk >= iwrkf) {
   
   // make a copy of the rank array for next iteration;
   // iorder (iwrkd:iwrkf) = iscrat (iwrkd:iwrkf);
      wqm_copyi(iscrat.at(iwrkd - 1), iorder.at(iwrkd - 1), iwrkf - iwrkd + 1);
      break;
   
   }

iwrk = iwrk + 1;

// we still have unprocessed values in both a and b;
if(iinda < jinda) {

   if(iindb < iwrkf) {

      if(icompi(iorder.at(iinda),iorder.at(iindb),xkey) > 0) {
        
         iindb = iindb + 1;
         iscrat.at(iwrk - 1) = iorder.at(iindb - 1);
         
      } else {
        
         iinda = iinda + 1;
         iscrat.at(iwrk - 1) = iorder.at(iinda - 1);
      
      }
    
   } else {

      // only a still has unprocessed values;
         iinda = iinda + 1;
         iscrat.at(iwrk - 1) = iorder.at(iinda - 1);
      
   }

} else {

  // only b still has unprocessed values;
  // iorder (iwrkd:iindb) = iscrat (iwrkd:iindb);
     wqm_copyi(iscrat.at(iwrkd - 1), iorder.at(iwrkd - 1), iindb - iwrkd + 1);
     iwrk = iwrkf;
     break;

}

}

}

// the cs become as and bs;
   lmtna = 2 * lmtna;

}

return;

}