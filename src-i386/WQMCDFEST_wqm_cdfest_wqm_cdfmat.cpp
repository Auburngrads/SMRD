#include <base/base.hpp>

//' Print a lower triangle matrix

void wqm_cdfmat(Rcpp::NumericVector &f,
                int &mm1,
                double &small,
                Rcpp::NumericVector &probd,
                int &mnzs,
                int &m,
                int &nnzs){

int ixl = 0, ixu = 0,ic = 0;
Rcpp::NumericVector out;
double pdi;
  
if(debug::kprint <= 1) return;
if(nnzs == 0) return;

for(int i = 1; i <= mm1; i++){
  
    pdi = probd.at(i - 1);
    if(pdi <= small) continue;
    ic = ic + 1;
    ixl = ixu + 1;
    ixu = ixu + ic;
    
    Rcpp::Rcout << "\nWQM_CDFMAT\n" << std::endl;
    Rcpp::Rcout << "i = " << i << std::endl;
    
    out = Rcpp::NumericVector(ic);
    for(int k = ixl; k <= ixu; k++){
      
        out.at(k - 1) = f.at(k - 1);
      
    }
    
    Rcpp::Rcout << "f = " << out << std::endl;

}
 return;
}
