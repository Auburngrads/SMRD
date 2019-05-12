#include <base/base.hpp>

int kindex(int k1,
           int l1){

  return l1 + (k1 - 1) * k1 / 2;
}


//' @description Compute factor, adjust \code{il}
//'              and \code{iu} for zero step
//'              probabilities, and add factor to
//'              information matrix

void wqm_cdffac(int &il,
                int &iu,
                Rcpp::NumericVector &f,
                Rcpp::NumericVector &probd,
                int &wt,
                bool &ltrunc,
                double &small,
                int &nnzs,
                int &mnzs,
                int &m,
                int &nty){

double ss, factr, pdl;
int nx,n1,nn1,ilx,iux,index;

if((ltrunc) and (nty == 1)) return;

// Compute factor
   ss = zero;

for(int j = il; j <= iu; j++){ ss = ss + probd.at(j - 1); }

if(ss == zero) return;

factr = wt / std::pow(ss,2);

// When computing the censoring (truncation) factor, the factor is
// added to (subtracted from) the information matrix
   if(ltrunc) factr = zero - factr;

// Correct il and iu for zero step probabilities
   nn1 = nnzs + 1;
   n1  = il - 1;
   nx  = 0;

if(il != 1) {

   // Adjust il for zero probability values by counting
   // the number of zero probability values before il
      for(int l = 1; l <= n1; l++){
      
          pdl = probd.at(l - 1);
          if(pdl > small)  nx = nx + 1;
      
      }
}

ilx = nx + 1;

// Adjust iu for zero probability values by counting the
// number of zero probability values between il and iu
   nx = 0;

   for(int l = il; l <= iu; l++) {
   
       pdl = probd.at(l - 1);
       if(pdl > small) nx = nx + 1;
   
   }

iux = ilx + nx - 1;

if(iux < ilx) return;

// Add factor to matrix
for(int k = ilx; k <= iux; k++){

    for(int l = ilx; l <= k; l++){

        if(k != nn1) {

           index = kindex(k,l);
           f.at(index - 1) = f.at(index - 1) + factr;
           continue;

        } 

        if(l == nn1) { 
      
           for(int kk = 1; kk <= mnzs; kk++){
  
               f.at(kk - 1) = f.at(kk - 1) + factr;
  
           }
       
           continue;
      
        }
    
        for(int ii = 1; ii <= l; ii++){
      
            index = kindex(l,ii);
            f.at(index - 1) = f.at(index - 1) - factr;
        
        }

        for(int jj = l; jj <= nnzs; jj++){
      
            index = kindex(jj,l);
            f.at(index - 1) = f.at(index - 1) - factr;
        
        }
    }
}

return;

}
