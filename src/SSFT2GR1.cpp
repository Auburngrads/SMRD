#include <base/base.hpp>
#include <ssft2gr1/sft2gr1.hpp>

//' Vector version of urlike for R calling
// [[Rcpp::export]]
Rcpp::List SSFT2GR1(Rcpp::NumericVector t,
                    int nt,
                    double r1log,
                    double mut2,
                    double sigmat2,
                    double mur1,
                    double sigmar1,
                    double mur2,
                    double sigmar2,
                    double rho,
                    Rcpp::NumericVector answer,
                    int kprint,
                    int ier){

debug::kprint = kprint;
double tlog;

for(int i = 0; i < nt; i++){
  
    tlog = std::log(t.at(i));
    ier = 0;
    answer.at(i) = sft2gr1(tlog, r1log, mut2, sigmat2, mur1,
                           sigmar1, mur2, sigmar2, rho, ier);
  
}

return Rcpp::List::create(Named("ier") = ier,
                          Named("t") = t,
                          Named("nt") = nt,
                          Named("r1log") = r1log,
                          Named("mut2") = mut2,
                          Named("sigmat2") = sigmat2,
                          Named("mur1") = mur1,
                          Named("sigmar1") = sigmar1,
                          Named("mur2") = mur2,
                          Named("sigmar2") = sigmar2,
                          Named("rho") = rho,
                          Named("answer") = answer);

}

#include <base/base.hpp>
#include <ssft2gr1/wqm_dqags.hpp>
#include <ssft2gr1/sft2gr1int.hpp>

using namespace passersft2gr1;

//' Subroutine to compute the sf of t2 given r1 (rate 1)
//' obtained by averaging over the conditional distribution of r2
//'
//' @param tlog        log of point at which sf is computed
//' @param r1          given value of r1
//' @param mut2        mean of log(t2) when r2=1
//' @param sigmat2     standard deviation of log(t2) when r2=1
//' @param mur1        mean of log(r1)
//' @param sigmar1     standard deviation of log(r1)
//' @param mur2        mean of log(r2)
//' @param sigmar2     standard deviation of log(r2)
//' @param rho         correlation between log(r1) and log(r2)
//' @param ier         return condition indicator (ier = 0 if no errors were detected in dqags. See dqags documentation for meaning of values of ier>0.)

double sft2gr1(double tlog, 
               double r1log, 
               double mut2, 
               double sigmat2, 
               double mur1,
               double sigmar1, 
               double mur2, 
               double sigmar2, 
               double rho, 
               int ier){
  
double eps,xfactor;
double r2lower,r2upper,abserr,sft2_gr1,answer;
int limit,limit4,last,neval;
Rcpp::NumericVector work  = Rcpp::NumericVector(400);
Rcpp::IntegerVector iwork = Rcpp::IntegerVector(100);

// constants that one might want to change to
// achieve a higher degree of accuracy from the algorithm
   eps = 1.0e-14;
   xfactor = 4.5e00;
   limit = 100;
   ier = 0;

// Get the conditional mean and variance of log(r2) given r1
   passersft2gr1::g_mur2gr1p = mur2 + rho * sigmar2 * ((r1log - mur1) / sigmar1);
   passersft2gr1::g_sigmar2gr1p = sigmar2 * std::sqrt(1 - std::pow(rho, 2));

// Define other constants needed to compute the integrand
   passersft2gr1::g_tlogp = tlog;
   passersft2gr1::g_mut2p = mut2;
   passersft2gr1::g_sigmat2p = sigmat2;

// Do the integration from r2lower to r2upper
   r2lower = passersft2gr1::g_mur2gr1p - xfactor * passersft2gr1::g_sigmar2gr1p;
   r2upper = passersft2gr1::g_mur2gr1p + xfactor * passersft2gr1::g_sigmar2gr1p;
   
   limit4 = 4 * limit;
   wqm_dqags(sft2gr1int,r2lower,passersft2gr1::g_mur2gr1p,
             eps,eps,answer,abserr,neval,ier,limit,limit4,
             last,iwork,work);
   
   limit4 = 4 * limit;
   wqm_dqags(sft2gr1int,passersft2gr1::g_mur2gr1p,r2upper,
             eps,eps,answer,abserr,neval,ier,limit,limit4,
             last,iwork,work);
   
   sft2_gr1 = answer;
   
   if(ier > 0) {
     
      Rcpp::warning("ier > 0 in sft2gr1");
      Rcpp::Rcout << "\nier = \n" << ier      << std::endl;
      Rcpp::Rcout << "sft2gr1 = " << sft2_gr1 << std::endl;
     
   }
   
return sft2_gr1;
     
}
