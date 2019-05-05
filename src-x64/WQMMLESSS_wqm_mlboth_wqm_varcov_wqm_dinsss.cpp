#include <base/base.hpp>
#include <wqmmlesss/wqm_dpivot.hpp>

//' This routine will invert an ntc x ntc matrix (ntc.le.idim)
//' the method of double pivoting is used. ir and jc are scratch
//' arrays of size idim.   irank returns the rank of the matrix
//' which will equal ntc if the matrix is nonsingular.   xtol is
//' a tolerance level and should be set to 1.e-8 for 32 or 36
//' bit words (and smaller for larger words).

void wqm_dinsss(Rcpp::NumericMatrix &a,
                int &ntc,
                double &xtol,
                int &irank,
                Rcpp::IntegerVector &jc,
                Rcpp::IntegerVector &ir,
                int &idim){

int nr, ix, is, ntp1,imax = 0,jmax = 0,it,jx;
double temp = 0,tol;
double tx,ty,t;
  
nr = ntc;
ix = 0;

is = 0;
ntp1 = ntc + 1;
irank = 0;
        
for(int i = 1; i <= nr; i++){
  
    jc.at(i - 1) = i;
    ir.at(i - 1) = i;
    
}

line20: temp = zero;

if(is == nr) goto line500;

is = is + 1;

for(int i = is; i <= nr; i++){
  
    for(int j = is; j <= nr; j++){
      
        if(std::abs(a.at(i - 1,j - 1)) > temp) {
          
           imax = i;
           jmax = j;
           temp = std::abs(a.at(i - 1,j - 1));
        
        }
    }
}

if(ix == 0) tol = xtol * temp;
ix = 1;
if(temp <= tol) goto line610;

for(int j = 1; j <= ntc; j++){
  
    tx = a.at(is - 1,j - 1);
    a.at(is - 1,j - 1) = a.at(imax - 1,j - 1);
    a.at(imax - 1,j - 1) = tx;
  
}

for(int i = 1; i <= nr; i++){
  
    ty = a.at(i - 1,is - 1);
    a.at(i - 1,is - 1) = a.at(i - 1,jmax - 1);
    a.at(i - 1,jmax - 1) = ty;
  
}

it = ir.at(imax - 1);
ir.at(imax - 1) = ir.at(is - 1);
ir.at(is - 1) = it;
it = jc.at(jmax - 1);
jc.at(jmax - 1) = jc.at(is - 1);
jc.at(is - 1) = it;

for(int i = 1; i <= nr; i++){
  
    a.at(i - 1,ntp1 - 1) = zero;
  
}

a.at(is - 1,ntp1 - 1) = one;

wqm_dpivot(a,is,nr,ntp1,idim);
          
for(int i = 1; i <= nr; i++){
  
    a.at(i - 1,is - 1) = a.at(i - 1,ntp1 - 1);
  
}

goto line20;
        
line500: if(is == nr) irank = nr;

line600: if(irank == 0) return;

goto line700;

line610: irank = is - 1;

goto line600;

line700: if(irank == nr) goto line710;

return;

line710: for(int i = 1; i <= nr; i++){
  
             for(int iq = 1; iq <= nr; iq++){
               
                 if(jc.at(iq - 1) == i) {
                   
                    ix = jc.at(iq - 1);
                    jc.at(iq - 1) = jc.at(i - 1);
                    jc.at(i - 1) = ix;
                    
                    for(int k = 1; k <= ntc; k++){
                      
                        t = a.at(iq - 1,k - 1);
                        a.at(iq - 1,k - 1) = a.at(i - 1,k - 1);
                        a.at(i - 1,k - 1) = t;
                      
                     }
                 }
             }
         }

for(int j = 1; j <= nr; j++){
  
    for(int jq = 1; jq <= nr; jq++){
      
        if(ir.at(jq - 1) == j) {
          
           jx = ir.at(jq - 1);
           ir.at(jq - 1) = ir.at(j - 1);
           ir.at(j - 1) = jx;
           
           for(int k = 1; k <= nr; k++){
             
               t = a.at(k - 1,jq - 1);
               a.at(k - 1,jq - 1) = a.at(k - 1,j - 1);
               a.at(k - 1,j - 1) = t;
             
            }
        }
    }
}
        
 return;
        
}
