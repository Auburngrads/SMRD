#include <base/base.hpp>
#include <genmax/wlsq1.hpp>

//' Weighted least squates with option for fixed parameter values
//' nparm = nter + 1 is the total number of parameters (including 
//' sigma). nter is the number ogf terms in linear model (col in x)
//' ntermm is the number of unconstrained regression parameters

void wlsq(Rcpp::NumericMatrix &y,
          Rcpp::IntegerVector &weight,
          Rcpp::NumericMatrix &x,
          int &nter,
          int &npoint,
          Rcpp::NumericVector &theta,
          Rcpp::IntegerVector &kodet,
          Rcpp::IntegerVector &iplab,
          Rcpp::NumericMatrix &vcv,
          int &idim,
          Rcpp::NumericMatrix &resid,
          Rcpp::NumericMatrix &yhat,
          double &sd,
          int &ier){
  
Rcpp::NumericVector iyfix = Rcpp::NumericVector(npoint);
Rcpp::IntegerVector iic   = Rcpp::IntegerVector(idim);
Rcpp::IntegerVector ijc   = Rcpp::IntegerVector(idim);
Rcpp::NumericVector ibeta = Rcpp::NumericVector(nter);
int nparm = nter + 1;

wlsq1(y,weight,x,nter,npoint,theta,kodet,
      iplab,nparm,ibeta,vcv,idim,resid,
      yhat,sd,iic,ijc,iyfix,ier);

return;

}

#include <base/base.hpp>
#include <genmax/dinvx.hpp>
#include <genmax/wlsrsd.hpp>
#include <genmax/matexp.hpp>

//' Root called by \code{wlsq}

void wlsq1(Rcpp::NumericMatrix &y,
           Rcpp::IntegerVector &weight,
           Rcpp::NumericMatrix &x,
           int &nter,
           int &npoint,
           Rcpp::NumericVector &theta,
           Rcpp::IntegerVector &kodet,
           Rcpp::IntegerVector &iplab,
           int &nparm,
           Rcpp::NumericVector &beta,
           Rcpp::NumericMatrix &vcv,
           int &idim,
           Rcpp::NumericMatrix &resid,
           Rcpp::NumericMatrix &yhat,
           double &sd,
           Rcpp::IntegerVector &ic,
           Rcpp::IntegerVector &jc,
           Rcpp::NumericVector &yfix,
           int &ier){

int indwg,jj = 0,kk = 0,ntermm;

for(int i = 1; i <= idim; i++){
  
    for(int j = 1; j <= idim; j++){
      
        vcv.at(i - 1,j - 1) = zero;
      
    }
  
}
for(int i = 1; i <= nter; i++){
    
    beta.at(i - 1) = zero;
  
}

// Compute x'x

if(debug::kprint >= 4) Rcpp::Rcout << "\nWLSQ1**4**\n" << std::endl;

for(int i = 1; i <= npoint; i++){
  
    indwg = weight.at(i - 1);
    yfix.at(i - 1) = zero;
    
    for(int j = 1; j <= nter; j++){
      
        if(kodet.at(j - 1) > 0) continue;
        yfix.at(i - 1) = yfix.at(i - 1) + x.at(i - 1,j - 1) * indwg * theta.at(j - 1);
    
    }
    
    jj = 0;
    
    for(int j = 1; j <= nter; j++){
      
        if(kodet.at(j - 1) == 0) continue;
        kk = 0;
        jj = jj + 1;
        
        for(int k = 1; k <= nter; k++){
          
            if(kodet.at(k - 1) == 0) continue;
            kk = kk + 1;
            vcv.at(jj - 1,kk - 1) = vcv.at(jj - 1,kk - 1) + x.at(i - 1,j - 1) * x.at(i - 1,k - 1) * indwg;
        
        }
        
    }
    
    if(debug::kprint >= 4) {
    
       Rcpp::NumericVector yrowi = y.row(i - 1);
       Rcpp::Rcout << "\ni = "       << i - 1        << std::endl;
       Rcpp::Rcout << "y(i,) = "     << yrowi        << std::endl;
       Rcpp::Rcout << "weight(i) = " << weight.at(i - 1) << std::endl;
       Rcpp::Rcout << "indwg = "     << indwg        << std::endl;
       Rcpp::Rcout << "yfix(i) = "   << yfix.at(i - 1)   << std::endl;
      
    }

}

ntermm = jj;

 if(debug::kprint >= 4) {
   
    Rcpp::Rcout << "\nBEFORE DINVX\n"      << std::endl;
    Rcpp::Rcout << "vcv = \n"    << vcv    << std::endl;
    Rcpp::Rcout << "ntermm = "   << ntermm << std::endl;
    Rcpp::Rcout << "idim = "     << idim   << std::endl;
   
 }

int irank;
double tol = 1.0e-20;
dinvx(vcv,ntermm,tol,ic,jc,irank,idim);

 if(debug::kprint >= 4) {
   
    Rcpp::Rcout << "\nAFTER DINVX\n"       << std::endl;
    Rcpp::Rcout << "vcv = \n"    << vcv    << std::endl;
    Rcpp::Rcout << "ntermm = "   << ntermm << std::endl;
    Rcpp::Rcout << "idim = "     << idim   << std::endl;
   
 }


if(irank != ntermm) {

   Rcpp::Rcout << "ic = "     << ic     << std::endl;
   Rcpp::Rcout << "jc = "     << jc     << std::endl;
   Rcpp::stop("\nwlsq1: irank (%i) != ntermm (%i)", irank,ntermm);

}

// Compute beta = inv(x'x)x'y
   jj = 0;

for(int j = 1; j <= nter; j++) {
  
    if(kodet.at(j - 1) <= 0) {
      
       beta.at(j - 1) = theta.at(j - 1);
       continue;
    
    }
    
    beta.at(j - 1) = zero;
    
    kk = 0;
    jj = jj + 1;
    
    for(int k = 1; k <= nter; k++){
      
        if(kodet.at(k - 1) == 0) continue;
        kk = kk + 1;

        for(int i = 1; i <= npoint; i++){
          
            indwg = weight.at(i - 1);
            beta.at(j - 1) = beta.at(j - 1) + vcv.at(jj - 1,kk - 1) * indwg * x.at(i - 1,k - 1) * y.at(i - 1,0) - yfix.at(i - 1);
        
        }
        
    }
    
    theta.at(j - 1) = beta.at(j - 1);

}

 if(debug::kprint >= 4) {
   
    Rcpp::Rcout << "\nBEFORE WLRSRD\n"      << std::endl;
    Rcpp::Rcout << "vcv = \n"    << vcv    << std::endl;
    Rcpp::Rcout << "resid = \n"  << resid  << std::endl;
    Rcpp::Rcout << "idim = "     << idim   << std::endl;
    Rcpp::Rcout << "beta = "     << beta   << std::endl;
    Rcpp::Rcout << "sd = "       << sd     << std::endl;
   
 }

// Compute the residuals
   int ny = 0;
   wlsrsd(y,x,weight,ny,nter,npoint,beta,resid,yhat,sd,ier);
   
 if(debug::kprint >= 4) {
   
    Rcpp::Rcout << "\nAFTER WLRSRD\n"      << std::endl;
    Rcpp::Rcout << "resid = \n"  << resid  << std::endl;
    Rcpp::Rcout << "beta = "     << beta   << std::endl;
    Rcpp::Rcout << "sd = "       << sd     << std::endl;
   
 }

// Expand the vcv by putting zeros where parameters were fixed
   matexp(vcv,kodet,nter,idim);
 
 if(debug::kprint >= 4) {
   
    Rcpp::Rcout << "\nAFTER MATEXP\n"      << std::endl;
    Rcpp::Rcout << "vcv = \n"      << vcv    << std::endl;
    Rcpp::Rcout << "ntermm = "   << ntermm << std::endl;
    Rcpp::Rcout << "idim = "     << idim   << std::endl;
   
 }

for(int i = 1; i <= nter; i++){
  
    for(int j = 1; j <= nter; j++){
      
        vcv.at(i - 1,j - 1) = sd * sd * vcv.at(i - 1,j - 1);
      
    }

}

 if(debug::kprint >= 4) {
   
    Rcpp::Rcout << "\nEND OF WLSQ1\n"      << std::endl;
    Rcpp::Rcout << "vcv = \n"    << vcv    << std::endl;
    Rcpp::Rcout << "nter = "     << ntermm << std::endl;
    Rcpp::Rcout << "ntermm = "   << ntermm << std::endl;
    Rcpp::Rcout << "idim = "     << idim   << std::endl;
   
 }

theta.at(nparm - 1) = sd;
 
return;

}

