#include <base/base.hpp>
#include <genfun/gfun1.hpp>

//' compute an estimate of a function and its variance
//' 
//'     func           name of a fortran function of thetas to be computed
//'                    must be of form func(thetas,nparm)
//'                    must be declared external in the calling program
//' 
//'     thetas         standardized parameter vector
//' 
//'     vcvs            variance-covariance matrix of theta
//' 
//'     kodet          parameter range vector
//' 
//'     nparm          length of parameter vector
//' 
//'     igsd           1 if standard error is needed, 0 otherwise
//' 
//'     fest           output estimate of the function
//' 
//'     vest           variance of the estimate of the function
//' 
//' 
//'     epsx           epsilon for finite differences

void gfun(Rcpp::List (*func)(Rcpp::List),
          Rcpp::NumericVector &thetas,
          Rcpp::NumericMatrix &vcvs,
          Rcpp::IntegerVector &kodet,
          int &nparm,
          int &igsd,
          double &fest,
          double &vest,
          double &epsx){
  
Rcpp::NumericVector igrad = Rcpp::NumericVector(nparm);
Rcpp::NumericVector idelta = Rcpp::NumericVector(nparm);

gfun1(func,thetas,vcvs,kodet,nparm,igsd,
      fest,vest,igrad,idelta,epsx);

return;

}

#include <base/base.hpp>
#include <genfun/gvec.hpp>
#include <genfun/fdel.hpp>

using namespace genx04;

//' Compute an estimate of a function and its standard error

void gfun1(Rcpp::List (*func)(Rcpp::List),
           Rcpp::NumericVector &thetas,
           Rcpp::NumericMatrix &vcvs,
           Rcpp::IntegerVector &kodet,
           int &nparm,
           int &igsd,
           double &fest,
           double &vest,
           Rcpp::NumericVector &grad,
           Rcpp::NumericVector &delta,
           double &epsx){
  
double eps,fout;
int ktrcde,npoint,im1;
Rcpp::List fargs,flist;
 
// Turn of ltp so that we do not use zgtran on the gamma vector
   genx04::g_ltp = 0;
   
   if(debug::kprint >= 4){
      
      Rcpp::Rcout << "\nGFUN1 check \n" << std::endl;
      Rcpp::Rcout << "   ltp = " << genx04::g_ltp << std::endl;
      Rcpp::Rcout << " ngame = " << genx03::g_ngame << std::endl;
      Rcpp::Rcout << " nparm = " << nparm << std::endl;
      Rcpp::Rcout << "thetas = " << thetas << std::endl;
      
   }
   
   fargs = Rcpp::List::create(Named("lt") = thetas);
   flist = func(fargs);
   fout = Rcpp::as<double>(Rcpp::as<Rcpp::List>(flist)["val"]);
   
   fest = fout;
   
   Rcpp::Rcout << "fest = " << fest << std::endl;
// If we do not need a standatrd error, we are done
   if(igsd == 0) return;
   
// Find a reasonable delta vector
   eps = std::abs(epsx * fest);
   ktrcde = 1;
   fdel(func,thetas,ktrcde,kodet,nparm,eps,delta);
   npoint = 0;
   gvec(func,npoint,thetas,delta,nparm,ktrcde,kodet,grad);
   
// Compute quadratic form for the variance of func at thetas
   vest = zero;
   for(int i = 1; i <= nparm; i++){
     
       vest = vest + grad.at(i - 1) * grad.at(i - 1) * vcvs.at(i - 1,i - 1);
       if(i == 1) continue;
       im1 = i - 1;
       
       for(int j = 1; j <= im1; j++){
         
           vest = vest + two * grad.at(i - 1) * grad.at(j - 1) * vcvs.at(i - 1,j - 1);
         
       }
        
   }
   
return;
      
}