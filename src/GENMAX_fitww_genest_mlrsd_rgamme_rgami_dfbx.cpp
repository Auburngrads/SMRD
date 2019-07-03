#include <base/base.hpp>
#include <genmax/dfbx1.hpp>

using namespace genx00;
using namespace genx05;

//' function to compute the estimated real parameter at kpnow
//' starting with the standardized (xbar,sd) intercept in thetas(1)
//' the parameter is defined by the explanatory variables pointed to
//' by ixcolp and the relationship specified by irelag
//' 
//' kpnow
//'   -1             parameter at x=0
//'    0             parameter at xbar
//'   >0             parameter at x(kpnow)

double dfbx(int kpnow,
            Rcpp::NumericVector thetas,
            int ipthta,
            Rcpp::IntegerVector ipxcg,
            int nterg,
            int intg,
            int irelag){
   
double df_bx;
// Grab some space for print x scratch
   Rcpp::NumericVector ixsave = Rcpp::NumericVector(nterg);
   
   if(debug::kprint >= 4){
      
      Rcpp::Rcout << "\nDFBX\n" << std::endl;
      Rcpp::Rcout << "ixsave = " << ixsave << std::endl;
      
   }
   
   df_bx = dfbx1(kpnow,
                 genx05::g_ipx,
                 genx05::g_ipxbru,
                 genx05::g_ipsd,
                 ipxcg,
                 genx00::g_nrownw,
                 irelag,nterg,intg,thetas,ipthta,ixsave);
   
return df_bx;
      
}

#include <base/base.hpp>
#include <genmax/grelat.hpp>
#include <utility/wqm_filld.hpp>

//' Root called by dfbx

double dfbx1(int kpnow,
             Rcpp::NumericMatrix x,
             Rcpp::NumericVector xbaru,
             Rcpp::NumericVector sd,
             Rcpp::IntegerVector ipcolx,
             int nrownw,
             int irelag,
             int nterg,
             int intg,
             Rcpp::NumericVector thetas,
             int ipthta,
             Rcpp::NumericVector xsave){
   
int jj,j = 1;
double df_bx1 = 0.0e00;

if(debug::kprint >= 4){
   
   Rcpp::Rcout << "\nDFBX1\n" << std::endl;
   Rcpp::Rcout << " kpnow = " << kpnow << std::endl;
   Rcpp::Rcout << "  intg = " << intg << std::endl;
   Rcpp::Rcout << "irelag = " << irelag << std::endl;
   Rcpp::Rcout << "ipcolx = " << ipcolx << std::endl;
   Rcpp::Rcout << "ipthta = " << ipthta << std::endl;
   Rcpp::Rcout << "     x = " << x << std::endl;
   Rcpp::Rcout << " xbaru = " << xbaru << std::endl;
   Rcpp::Rcout << "    sd = " << sd << std::endl;
   Rcpp::Rcout << "thetas = " << thetas << std::endl;
   
}

if(kpnow < 0) {
   
   for(int j = 1; j <= nterg; j++){

       jj = ipcolx.at(j - 1) + 1;
      
       // Save the values of the scaled and centered x values at true x=0
          xsave.at(j - 1) = -1 * xbaru.at(jj - 1) / sd.at(jj - 1);
          
   }
   
   // If there is an intercept in the model, we need to fix up col of ones
      xsave.at(0) = one;
      df_bx1 = grelat(thetas,ipthta,xsave,nterg,irelag);
   
}

if(kpnow == 0){
   
   df_bx1 = zero;
   
   for(int i = 1; i <= nterg; i++){
      
       xsave.at(i - 1) = zero;
      
   }
   
   if(intg != 0){
      
      df_bx1 = thetas.at(ipthta - 1);
      xsave.at(0) = one;
   
   }
   
}
   
if(kpnow > 0){
   
   for(int j = 1; j <= nterg; j++){
      
       // Pick out x columns used in the current relationship, put in xsave
       // icolpx(1)=0 if there is an intercept
       // up one because x0 is always a column of ones
          jj = ipcolx.at(j - 1) + 1;
      
       // Save the scaled and possibly centered x values
          xsave.at(j - 1) = x.at(kpnow - 1,jj - 1);
   
   }
   
   // If irelag=1, dfbx1=theta(i1)x(i1)+theta(i2)*x(i2)+...theta(interg)*x(interg)
      df_bx1 = grelat(thetas,ipthta,xsave,nterg,irelag);
   
}

if(debug::kprint >= 9){
   
   Rcpp::Rcout << "\nDFBX**9**\n" << std::endl;
   Rcpp::Rcout << " kpnow = " << kpnow - 1 << std::endl;
   Rcpp::Rcout << "nrownw = " << nrownw << std::endl;
   Rcpp::Rcout << "irelag = " << irelag << std::endl;
   Rcpp::Rcout << " nterg = " << nterg << std::endl;
   Rcpp::Rcout << "  intg = " << intg << std::endl;
   Rcpp::Rcout << " dfbx1 = " << df_bx1 << std::endl;
   Rcpp::Rcout << " xsave = " << xsave << std::endl;
   Rcpp::Rcout << "thetas = " << thetas << std::endl;
   
}
   
return df_bx1;

}