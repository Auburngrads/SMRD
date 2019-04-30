#include <base/base.hpp>
#include <surlike/urlike.hpp>

//' Vector version of urlike for Splus calling
// [[Rcpp::export]]
Rcpp::List SURLIKE(Rcpp::NumericVector t, 
                   int nt,
                   double mut1,
                   double sigmat1,
                   double mut2,
                   double sigmat2,
                   double mur1,
                   double sigmar1,
                   double mur2,
                   double sigmar2,
                   double rho,
                   Rcpp::NumericVector answer,
                   int kprint){

debug::kprint = kprint;

int ier = 0;

for(int i = 0; i < nt; i++){
  
    answer.at(i) = urlike(t.at(i), mut1, sigmat1, mut2, sigmat2,
                          mur1, sigmar1, mur2, sigmar2, rho, ier);
  
}

return Rcpp::List::create(Named("t")       = t,
                          Named("nt")      = nt,
                          Named("mut1")    = mut1,
                          Named("sigmat1") = sigmat1,
                          Named("mut2")    = mut2,
                          Named("sigmat2") = sigmat2,
                          Named("mur1")    = mur1,
                          Named("sigmar1") = sigmar1,
                          Named("mur2")    = mur2,
                          Named("sigmar2") = sigmar2,
                          Named("rho")     = rho,
                          Named("answer")  = answer,
                          Named("ier")     = ier);
  
}

#include <base/base.hpp>
#include <ssft2gr1/wqm_dqags.hpp>
#include <surlike/urlikeint.hpp>

using namespace passerurlike;

//' Function to compute log likelihood for an exact failure of mode 1
//'
//' @param t           point at which sf is computed
//' @param mut1        mean of log(t1) when r1=1
//' @param sigmat1     standard deviation of log(t1) when r1=1
//' @param mut2        mean of log(t2) when r2=1
//' @param sigmat2     standard deviation of log(t2) when r2=1
//' @param mur1        mean of log(r1)
//' @param sigmar1     standard deviation of log(r1)
//' @param mur2        mean of log(r2)
//' @param sigmar2     standard deviation of log(r2)
//' @param rho         correlation between log(r1) and log(r2)
//' @param ier         Return condition indicator (ier = 0 if no errors were detected in dqags. 
//'                    See dqags documentation for meaning of values of ier > 0)

double urlike(double t,
              double mut1,
              double sigmat1,
              double mut2,
              double sigmat2,
              double mur1,
              double sigmar1,
              double mur2,
              double sigmar2,
              double rho,
              int ier){
  
Rcpp::IntegerVector iwork = Rcpp::IntegerVector(200);
Rcpp::NumericVector work  = Rcpp::NumericVector(800);
double eps,xfactor,ur_like,r1lower,r1upper;
double abserr,answer1,answer2;
int limit,limit4,neval,last;

// Constants that one might want to change to achieve a higher degree of accuracy from the algorithm
   eps = 1.0e-14;
   xfactor = 4.5e00;
   limit = 200;
   ier = 0;

// Define other constants needed to compute the integrand
   passerurlike::g_tlogp = std::log(t);
   passerurlike::g_mut1p = mut1;
   passerurlike::g_sigmat1p = sigmat1;
   passerurlike::g_mut2p = mut2;
   passerurlike::g_sigmat2p = sigmat2;
   passerurlike::g_mur1p = mur1;
   passerurlike::g_sigmar1p = sigmar1;
   passerurlike::g_mur2p = mur2;
   passerurlike::g_sigmar2p = sigmar2;
   passerurlike::g_rhop = rho;

// Do the integration from r1lower to r1upper
   r1lower = mur1 - xfactor * sigmar1;
   r1upper = mur1 + xfactor * sigmar1;
   
   limit4 = limit * 4;
   wqm_dqags(urlikeint,r1lower,mur1,eps,eps,answer1,
             abserr,neval,ier,limit,limit4,last,
             iwork,work);
   
   limit4 = limit * 4;
   wqm_dqags(urlikeint,mur1,r1upper,eps,eps,answer2,
             abserr,neval,ier,limit,limit4,last,
             iwork,work);
   
   ur_like = std::log(answer1 + answer2);
   
if(debug::kprint > 0){
  
   Rcpp::Rcout << "\nEnd of urlike\n"        << std::endl;
   Rcpp::Rcout  << "r1lower = "  << r1lower  << std::endl;
   Rcpp::Rcout  << "r1upper = "  << r1upper  << std::endl;
   Rcpp::Rcout  << "answer1 = "  << answer1  << std::endl;
   Rcpp::Rcout  << "answer2 = "  << answer2  << std::endl;
   Rcpp::Rcout  << "urlike = "   << ur_like  << std::endl;
   Rcpp::Rcout  << "ier = "      << ier      << std::endl;

}

return ur_like;

}

#include <base/base.hpp>
#include <ssft2gr1/sft2gr1.hpp>

using namespace passerurlike;

//' Function to compute integrand for computing  the sf of t2 given r1
//'
//' @details The following variables are communicated through common
//'
//' \document{
//'   \item{tlogp}{log of time point for evaluation of the conditional sf}
//'   \item{mut1p}{mean of log(t1) when r1=1}
//'   \item{sigmat1p}{standard deviation of log(t1) when r1=1}
//'   \item{mut2p}{mean of log(t2) when r2=1}
//'   \item{sigmat2p}{standard deviation of log(t2) when r2=1}
//'   \item{mur1p}{mean of log(r1)}
//'   \item{sigmar1p}{standard deviation of log(r1)}
//'   \item{mur2p}{mean of log(r2)}
//'   \item{sigmar2p}{standard deviation of log(r2)}
//'   \item{rhop}{correlation between log(r1) and log(r2)}
//' }

double urlikeint(double r1log){

double mut1gr1,ztigr1,zr1,tmp1,tmp2,tmp3;
double ur_likeint;
int ier = 0;
Rcpp::NumericVector xtest = Rcpp::NumericVector(5);

// Get the conditional mean of log(t1) given r1
   mut1gr1 = passerurlike::g_mut1p + r1log;
   ztigr1 = (passerurlike::g_tlogp - mut1gr1) / passerurlike::g_sigmat1p;
   zr1 = (r1log - passerurlike::g_mur1p) / passerurlike::g_sigmar1p;
   
   tmp1 = sft2gr1(passerurlike::g_tlogp, r1log, passerurlike::g_mut2p, 
                  passerurlike::g_sigmat2p, passerurlike::g_mur1p, 
                  passerurlike::g_sigmar1p,passerurlike::g_mur2p, 
                  passerurlike::g_sigmar2p,passerurlike::g_rhop, ier);
   
   tmp2 = R::dnorm(ztigr1, 0, 1, false) / passerurlike::g_sigmat1p;
   tmp3 = R::dnorm(zr1, 0, 1, false) / passerurlike::g_sigmar1p;
   
   ur_likeint = tmp1 * tmp2 * tmp3;
   
   xtest.at(0) = r1log;
   xtest.at(1) = tmp1;
   xtest.at(2) = tmp2;
   xtest.at(3) = tmp3;
   xtest.at(4) = ur_likeint;
   
   if(debug::kprint > 0) {
     
      Rcpp::Rcout << "\nxtest = \n" << xtest << std::endl; 
     
   }
   
return ur_likeint;
  
}
