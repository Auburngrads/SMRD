#include <base/base.hpp>

//' Eigenvalue routine with the same arguement 
//' list as ge eig1 double precision version
//'
//'   b     input: symmetric matrix in full storage mode
//'
//'         output:  eigenvalues on diagonal
//'
//'   d     output:  eigenvectors
//'
//'   n     input:   dimension of matrix
//'   xtol  input: machine zero
//'
//'   idim  dimension of matrix storage area

void wqm_deign(Rcpp::NumericMatrix &b,
               Rcpp::NumericMatrix &d,
               int &n,
               double &xtol,
               int &idim){

int nitt = 30, niter = 0, nn, mo,ip1,io;
double by,sig,cot,s,c,bki,bmi;
double bik,bim,dik,dim;
double bmax,bm;
  
for(int i = 0; i < n; i++){
  
    for(int j = 0; j < n; j++){
      
        if(i == j) {
          
           d.at(i,i) = one;
           
         } else {
        
           d.at(i,j) = zero;
          
        }
        
    }
}

nn = n - 1;

if(nn <= 0) return;

line30:
  
for(int k = 1; k <= nn; k++){
  
    mo = k + 1;
  
    for(int m = mo; m <= n; m++){
      
      if(std::abs(b.at(k - 1,m - 1)) <= xtol) continue;
      
      by = (b.at(k - 1,k - 1) - b.at(m - 1,m - 1)) / (two * b.at(k - 1,m - 1));
      
      if(by < zero) { sig = -1.0e00; } else { sig = 1.0e00; }
         
      cot = by + (sig * std::sqrt(by * by + 1.0e00));
      s = sig / std::sqrt(cot * cot + 1.0e00);
      c = s * cot;
      
      for(int i = 1; i <= n; i++){
        
          bki = b.at(k - 1,i - 1);
          bmi = b.at(m - 1,i - 1);
          b.at(k - 1,i - 1) =     c * bki + s * bmi;
          b.at(m - 1,i - 1) = -1* s * bki + c * bmi;
          
      }
      
      for(int i = 1; i <= n; i++){
        
          bik = b.at(i - 1,k - 1);
          bim = b.at(i - 1,m - 1);
          dik = d.at(i - 1,k - 1);
          dim = d.at(i - 1,m - 1);
          b.at(i - 1,k - 1) =      c * bik + s * bim;
          b.at(i - 1,m - 1) = -1 * s * bik + c * bim;
          d.at(i - 1,k - 1) =      c * dik + s * dim;
          d.at(i - 1,m - 1) = -1 * s * dik + c * dim;
        
      }
        
    }
}

bmax = zero;
  
for(int i = 1; i <= nn; i++){
  
    io = i + 1;
  
    for(int j = io; j <= n; j++){
      
        bm = std::abs(b.at(i - 1,j - 1));
        if(bmax < bm) bmax = bm;
        
    }
}

if(bmax > xtol) {
  
   niter = niter + 1;
   if(niter < nitt) goto line30;

}

for(int i = 1; i <= n; i++){
  
    ip1 = i + 1;
    if(ip1 > n) continue;
    
    for(int j = ip1; j <= n; j++){
      
        if(b.at(i - 1,i - 1) > b.at(j - 1,j - 1)) continue;
        bm = b.at(i - 1,i - 1);
        b.at(i - 1,i - 1) = b.at(j - 1,j - 1);
        b.at(j - 1,j - 1) = bm;
        
        for(int k = 1; k <= n; k++){
          
            bm = d.at(k - 1,i - 1);
            d.at(k - 1,i - 1) = d.at(k - 1,j - 1);
            d.at(k - 1,j - 1) = bm;
            
        }
      
    }
}

      return;

}