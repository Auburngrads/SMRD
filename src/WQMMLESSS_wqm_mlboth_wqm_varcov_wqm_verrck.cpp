#include <base/base.hpp>
#include <wqmmlesss/wqm_deign.hpp>

//' Error checking routine for mlfit
//'  
//' Check:  o non full rank information matrix in invert (iernfr)
//'         o information matrix that is not positive definite
//'              by the log(max/min eigen value) check (iernpd)
//'         o first derivatives that are not close to zero (iernzd)
//'
//' iernzd     iernpd       iernfr     ier
//'  0         0            0          0
//'  1         0            0          1
//'  0         1            0          2
//'  1         1            0          3
//'  0         0            1          4
//'  1         0            1          5
//'  0         1            1          6
//'  1         1            1          7

void wqm_verrck(Rcpp::NumericMatrix &vcvd,
                Rcpp::NumericMatrix &vcvdd,
                Rcpp::NumericVector &fsderd,
                Rcpp::LogicalVector &lfix,
                double &xtol,
                int &nparm,
                int &nparmm,
                int &ier){
  
//  Define tolerances for:
//
//  - first derivative,
//  - smallest eigenvalue
//  - log of max/min eigenvalue ratio

double xdtol = 1.0e-01;
double xztol = 1.0e-12;
double xlrtol = 15.0e00;
int jj, kk = 0;
double evmin,evmax,xdmax;
int iernzd,iernpd,iernfr;
double xlmin,xlmax,xlr;

// First condense vcvd to vcdvdd again
jj = 0;

for(int j = 1; j <= nparm; j++){
  
    if(lfix.at(j - 1)) continue;
    jj = jj + 1;
    kk = 0;
    
    for(int k = 1; k <= j; k++){
      
        if(!lfix.at(k - 1)) {
          
           kk = kk + 1;
           vcvdd.at(kk - 1,jj - 1) = vcvd.at(k - 1,j - 1);
           vcvd.at(j - 1,k - 1) = vcvd.at(k - 1,j - 1);
           vcvdd.at(jj - 1,kk - 1) = vcvdd.at(kk - 1,jj - 1);
        
        }
      
    }
}

nparmm = jj;

if(debug::kprint >= 3) {
  
   Rcpp::Rcout <<"\nVERRCK**3**\n" << std::endl;
   Rcpp::Rcout << "vcvd = \n" << vcvd   << std::endl;
   
}

// Go to find the eigen values and eigen vectors
   wqm_deign(vcvdd,vcvd,nparmm,xtol,nparm);

// Initilize
   evmin =  1.0e20;
   evmax = -1.0e20;
   xdmax = 0.0e00;
   iernzd = 0;
   iernpd = 0;
   iernfr = 0;

if(ier > 0) iernfr = 1;

jj = 0;

// Go over parameters not fixed and look for problems
for(int j = 1; j <= nparm; j++){
  
    if(lfix.at(j - 1)) continue;
    jj = jj + 1;
    evmin = std::min(evmin,vcvdd.at(jj - 1,jj - 1));
    evmax = std::max(evmax,vcvdd.at(jj - 1,jj - 1));
    xdmax = std::max(std::abs(fsderd.at(j - 1)),xdmax);
    
    if(debug::kprint >= 3){
      
       Rcpp::Rcout << "\nVERRCK**3**\n" << std::endl;
       Rcpp::Rcout << "j = " << j << std::endl;
       Rcpp::Rcout << "jj = " << jj << std::endl;
       Rcpp::Rcout << "lfix(j) = "      << lfix.at(j - 1) << std::endl;
       Rcpp::Rcout << "fsderd(j) = "    << fsderd.at(j - 1) << std::endl;
       Rcpp::Rcout << "vcvdd(jj,jj) = " << vcvdd.at(jj - 1,jj - 1) << std::endl;
       Rcpp::Rcout << "vcvd(kk,jj) = "  << vcvd.at(kk - 1, jj - 1) << std::endl;
       Rcpp::Rcout << "nparmm = "       << nparmm << std::endl;
      
    }
  
}

if(evmin >= xztol) {
  
   xlmin = std::log(evmin);
   xlmax = std::log(evmax);
   xlr   = xlmax - xlmin;
   if(xlr < xlrtol) goto line50;
      
}

iernpd = 1;

line50: if (xdmax > xdtol) iernzd = 1;
        ier = iernfr * 4 + iernpd * 2 + iernzd;
        
if(debug::kprint >= 2){
  
   Rcpp::Rcout << "\nVERRCK**2**\n" << std::endl;
   Rcpp::Rcout << "evmin = "  << evmin << std::endl;
   Rcpp::Rcout << "evmax = "  << evmax << std::endl;
   Rcpp::Rcout << "xdmax = "  << xdmax << std::endl;
   Rcpp::Rcout << "xlr = "    << xlr << std::endl;
   Rcpp::Rcout << "iernfr = " << iernfr << std::endl;
   Rcpp::Rcout << "iernpd = " << iernpd << std::endl;
   Rcpp::Rcout << "iernzd = " << iernzd << std::endl;
   Rcpp::Rcout << "ier = "    << ier << std::endl;
  
  
}

  return;

}