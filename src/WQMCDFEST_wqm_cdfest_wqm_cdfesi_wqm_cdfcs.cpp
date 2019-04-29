#include <base/base.hpp>

//' @description Get std errors from covariance
//'              matrix.

void wqm_cdfcs(Rcpp::NumericVector &sd,
               Rcpp::NumericVector &f,
               int &m,
               int &m1,
               int &mm1,
               Rcpp::NumericVector &probd,
               double &small,
               int &mnzs,
               int &nnzs){

int iy,iz,ix = 1,ir;
sd.at(0) = std::sqrt(f.at(0));
double vars = f.at(0),pdk;

for(int i = 2; i <= nnzs; i++){

    ix = ix + i; // move this line
    vars = vars + f.at(ix - 1);
    iy = ix - 1;
    iz = ix - i + 1;

    for(int j = iz; j <= iy; j++) {
    
        vars = vars + two * f.at(j - 1);
    
    }

    sd.at(i - 1) = std::sqrt(vars);
    
}

// Insert variances for zero probability values

int mx = nnzs + 1;

for(int i = mx; i <= m; i++){ sd.at(i - 1) = zero; }

for(int k = 1; k <= mm1; k++){

    pdk = probd.at(k - 1);
    if(pdk > small) continue;

    for(int i = k; i <= mm1; i++){
    
        ir = mm1 - i + k;
        sd.at(ir - 1) = sd.at(ir - 2);
    
    }
    
}

return;

}
