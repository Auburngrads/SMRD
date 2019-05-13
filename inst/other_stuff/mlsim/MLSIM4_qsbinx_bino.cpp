#include <base/base.hpp>
#include <mlsim2/binodf.hpp>

//' Binomial cdf -- vectorized
void pbino(Rcpp::IntegerVector &ix,
           Rcpp::IntegerVector &n,
           Rcpp::NumericVector &p,
           Rcpp::NumericVector &ans,
           int &number){
  
for(int i = 1; i <= number; i++){
  
    ans.at(i - 1) = binodf(ix.at(i - 1),n.at(i - 1),p.at(i - 1));
  
}

return;
        
}

#include <base/base.hpp>
#include <mlsim2/dbetai.hpp>

//' Binomial cdf, following kennedy and gentle
double binodf(int ix,
              int n,
              double p){

double bino_df = zero;
  
if(ix < 0) return bino_df;

bino_df = one;

if(ix >= n) return bino_df;

bino_df = dbetai(one - p, (double)(n - ix), (double)(ix + 1));

return bino_df;

}


#include <base/base.hpp>
#include <mlsim2/bnldev.hpp>

using namespace rantab;

//' Interface to vectorize binomial random numbers  for R function rbino
void rbino(int &numsim,
           double &pp,
           int &n,
           Rcpp::NumericVector &tspass,
           bool &lrand,
           Rcpp::IntegerVector &ivec){
  
double xxx = 1.0e00;
  
if(lrand) xxx = R::unif_rand();

for(int k = 1; k <= 32; k++){
  
    rantab::g_t.at(k - 1) = tspass.at(k - 1);
  
}

if(lrand)xxx = R::unif_rand();

for(int i = 1; i <= numsim; i++){
  
    ivec.at(i - 1) = bnldev(pp,n);
  
}
        
return;
      
}


#include <base/base.hpp>
#include <mlsim2/kqbino.hpp>

//' Binomial quantile, vectorized
void qbino(Rcpp::NumericVector &pquan, 
           Rcpp::IntegerVector &n, 
           Rcpp::NumericVector &pbin, 
           int &number, 
           Rcpp::IntegerVector &kqb){
  
for(int i = 1; i <= number; i++){
  
    kqb.at(i - 1) = kqbino(pquan.at(i - 1), n.at(i - 1), pbin.at(i - 1));
  
}

return;
  
}


#include <base/base.hpp>
#include <sbq/zeroin.hpp>
#include <mlsim2/fqbin.hpp>

using namespace fdb001;

//' Binomial quantile

int kqbino(double pquan,
           int n, 
           double pbin){

double small = 1.0e-10, tol = 1.0e-12;
double x1,x2,qbinom;
int kq_bino;

fdb001::g_pbinp = pbin;
fdb001::g_np = n;
fdb001::g_pquanp = pquan;

x1 = -one + small;
x2 = (double)n - small;
qbinom = zeroin(x1,x2,fqbin,tol);
kq_bino = qbinom + one;

return kq_bino;

}

#include <base/base.hpp>
#include <mlsim2/dbeta1.hpp>

using namespace fdb001;

double fqbin(double x){
  
double fq_bin = 0.0e00;
  
fq_bin = dbetai(one - fdb001::g_pbinp, (double)fdb001::g_np - x, x + one) - fdb001::g_pquanp;
           
if(debug::kprint >= 7) {
  
   Rcpp::Rcout << "\nin fqbin\n" << std::endl
   Rcpp::Rcout << "x = " << x << std::endl;
   Rcpp::Rcout << "pbinp = " << fdb001::g_pbinp << std::endl;
   Rcpp::Rcout << "pquanp = " << fdb001::g_pquanp << std::endl;
   Rcpp::Rcout << "np = " << fdb001::g_np << std::endl;
   Rcpp::Rcout << "fq_bin = " << fq_bin << std::endl;
  
}     
  
return fq_bin;

}
