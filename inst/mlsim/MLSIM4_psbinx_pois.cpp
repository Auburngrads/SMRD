#include <base/base.hpp>
#include <mlsim2/pchisq.hpp>

//' Poisson cdf Pr(X <= ix;lambda), vectorized
void ppois(Rcpp::IntegerVector &ix,
           Rcpp::NumericVector &xlamb,
           Rcpp::NumericVector &ans,
           int& number){
  
double dof,x;
  
for(int i = 1; i <= number; i++){
    
    ans.at(i - 1) = zero;
    if(ix.at(i - 1) < 0) continue;
    dof = (double)(2 * (ix.at(i - 1) + 1));
    x = 2.0e00 * xlamb.at(i - 1);
    ans.at(i - 1) = one - pchisq(x,dof);
    
}

return;
  
}

#include <base/base.hpp>
#include <mlsim2/kqpois.hpp>

//' Poisson quantile, vectorized
void qpois(Rcpp::NumericVector &pquan, 
           Rcpp::NumericVector &xlam, 
           int &number, 
           Rcpp::IntegerVector &kqp){
  
for(int i = 1; i <= number; i++){
  
    kqp.at(i - 1) = kqpois(pquan.at(i - 1), xlam.at(i - 1));
  
}
        
return;
        
}

#include <base/base.hpp>
#include <mlsim2/fdqgam.hpp>
#include <utility/wqm_quant.hpp>
#include <sbq/zeroin.hpp>

using namespace fdb001;

//' Poisson dist quantiles
int kqpois(double pquan, 
           double xlam){
  
double tol = 1.0e-12,xmu,xsig,x2,x1,xinit;
double qpoisx;
int kq_pois;


fdb001::g_xlamp = xlam;
fdb001::g_pquanp = pquan;

xmu = xlam;
xsig = std::sqrt(xlam);
xinit = xmu + xsig * wqm_quant(pquan,3); 
x2 = std::max(xinit * 20.0e00,50e00);
x1 = -0.99999999999;
qpoisx = zeroin(x1,x2,fdqgam,tol);
kqpois = qpoisx + one;

return kq_pois;

}


#include <base/base.hpp>
#include <mlsim2/dgami.hpp>

using namespace fdb001;

double fdqgam(double x){
  
double dgamix,fdq_gam;
int ier = 0;
  
dgamix = dgami(fdb001::g_xlamp, x + one,ier);

fdqgam = one - dgamix - fdb001::g_pquanp;

if(debug::kprint >= 7){
  
   Rcpp::Rcout << "\nIn fdqgam\n"                  << std::endl;
   Rcpp::Rcout << "x = "       << x                << std::endl;
   Rcpp::Rcout << "xlamp = "   << fdb001::g_xlamp  << std::endl;
   Rcpp::Rcout << "pquanp = "  << fdb001::g_pquanp << std::endl;
   Rcpp::Rcout << "dgamix = "  << dgamix           << std::endl;
   Rcpp::Rcout << "fdq_gam = " << fdq_gam          << std::endl;

}
      
return fdq_gam;
  
}


#include <base/base.hpp>
#include <spgeng/gaminc.hpp>

double dgami(double x, 
             double qinp,
             int ier){
  
double d_gami = gaminc(x,qinp,ier);
  
return d_gami;
  
}