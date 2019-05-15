#include <base/base.hpp>

int kindex(int k1,
           int l1,
           std::string chk){

  int OUT = l1 + (k1 - 1) * std::floor(k1 / 2);
  
  if(debug::kprint >= 10){
    
     Rcpp::Rcout << "\nkindex check\n" << std::endl;
     Rcpp::Rcout << "where = " << chk << std::endl;
     Rcpp::Rcout << "k1 = " << k1 << std::endl;
     Rcpp::Rcout << "l1 = " << l1 << std::endl;
     Rcpp::Rcout << "index = " << OUT << std::endl;
    
  }
  return OUT;
  
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

if((ltrunc) and (nty == 0)) return;

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
          if(pdl <= small) continue;
          nx = nx + 1;
      
      }
}

ilx = nx + 1;

// Adjust iu for zero probability values by counting the
// number of zero probability values between il and iu
   nx = 0;

   for(int l = il; l <= iu; l++) {
   
       pdl = probd.at(l - 1);
       if(pdl <= small) continue;
       nx = nx + 1;
   
   }

iux = ilx + nx - 1;

if(iux < ilx) return;

// Add factor to matrix
for(int k = ilx; k <= iux; k++){

    for(int l = ilx; l <= k; l++){

        if(k == nn1) goto line50;

           index = kindex(k,l,"In loop k-l");
           f.at(index - 1) = f.at(index - 1) + factr;
           continue;

        line50: if(l == nn1) goto line60; 
      
        for(int ii = 1; ii <= l; ii++){
      
            index = kindex(l,ii,"In loop ii");
            f.at(index - 1) = f.at(index - 1) - factr;
        
        }

        for(int jj = l; jj <= nnzs; jj++){
      
            index = kindex(jj,l,"In loop jj");
            f.at(index - 1) = f.at(index - 1) - factr;
        
        }
        
        continue;
        
        line60: for(int kk = 1; kk <= mnzs; kk++){
          
                    f.at(kk - 1) = f.at(kk - 1) + factr;
          
                }
    }
}

return;

}
