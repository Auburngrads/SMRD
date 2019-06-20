#include <base/base.hpp>
#include <genfun/func0.hpp>
#include <genfun/func1.hpp>
#include <genfun/func2.hpp>
#include <genfun/func3.hpp>
#include <genfun/func4.hpp>
#include <genfun/usrfun.hpp>
#include <utility/icheck.hpp>

//' compute a function of the model parameters
//' if kfuncf>0, the parameter vector is in the user's scale
//' if kfuncf<0, the first element of gamme contains the function value and
//' the estimate of the usual first parameter will be returned
//' this first parameter is usually a location parameter or an intercept
//' in the first regression relationship
//' 
//' llog determines whether exponential trans is used or not
//' on quantiles in the individual func routines.

double funcg(Rcpp::NumericVector gamme,
             int ngame,
             int kmod,
             int kdist,
             int kfuncf,
             int llog,
             double funarg){

int kmodp, ier = 0;
double fun_cg = 0.0e00;
  
icheck(kmod,0,5,0,0,ier,5001);
kmodp = kmod + 1;

if(kmodp > 100) fun_cg = usrfun(kmodp,gamme,kdist,kfuncf,llog,funarg);

if(kmodp == 1)  fun_cg = func0(gamme,ngame,kdist,kfuncf,llog,funarg);

if(kmodp == 2)  fun_cg = func1(gamme,ngame,kdist,kfuncf,llog,funarg);

if(kmodp == 3)  fun_cg = func2(gamme,ngame,kdist,kfuncf,llog,funarg);

if(kmodp == 4)  fun_cg = func3(gamme,ngame,kdist,kfuncf,llog,funarg);

if(kmodp == 5)  fun_cg = func4(gamme,ngame,kdist,kfuncf,llog,funarg);

if(kmodp == 6)  fun_cg = func0(gamme,ngame,kdist,kfuncf,llog,funarg);

if(debug::kprint >= 7) {
  
   Rcpp::Rcout << "\nEND OF FUNCG\n" << std::endl;
   Rcpp::Rcout << " gamme = " << gamme << std::endl;
   Rcpp::Rcout << " ngame = " << ngame << std::endl;
   Rcpp::Rcout << "  kmod = " << kmod << std::endl;
   Rcpp::Rcout << " kdist = " << kdist << std::endl;
   Rcpp::Rcout << "kfuncf = " << kfuncf << std::endl;
   Rcpp::Rcout << "  llog = " << llog << std::endl;
   Rcpp::Rcout << "funarg = " << funarg << std::endl;
   Rcpp::Rcout << "fun_cg = " << fun_cg << std::endl;
  
} 

return fun_cg;

}