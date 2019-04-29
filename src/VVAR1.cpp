#include <base/base.hpp>
#include <vvar1/var1.hpp>

//' Vectorized version ov svar1
//' Calculate variance of percentile at some stress level
//' zivar is the variable defining that level
//' (usually set to zero for design stress)
//' this a wrap for a call from splus
// [[Rcpp::export]]
Rcpp::List vvar1(Rcpp::NumericVector param,
                 Rcpp::NumericVector z,
                 Rcpp::NumericVector pi,
                 double zivar,
                 int npar,
                 int nplan,
                 int nlev,
                 Rcpp::NumericVector perc,
                 int idist,
                 int knownt,
                 Rcpp::NumericMatrix fret,
                 Rcpp::NumericVector varret,
                 int iprint) {

debug::kprint = iprint;
  
Rcpp::NumericMatrix fisher(5,5);
Rcpp::NumericVector fp(20),pq(20);
double a, b1,b2,thet1;
int index;

for(int iplan = 1; iplan <= nplan; iplan++){
  
// Pick up scalars to send down
   a  = param.at(iplan - 1);
   b1 = param.at(nplan + iplan - 1);
   b2 = param.at(2 * nplan + iplan - 1);
   thet1 = param.at(3 * nplan + iplan - 1);
   
if(debug::kprint > 1){
  
   Rcpp::Rcout << "\nVVAR1**1\n" << std::endl;
   Rcpp::Rcout << "iplan = "       << iplan << std::endl;
   Rcpp::Rcout << "nlev = "        << nlev << std::endl;
   Rcpp::Rcout << "a = "           << a << std::endl;
   Rcpp::Rcout << "b1 = "          << b1 << std::endl;
   Rcpp::Rcout << "b2 = "          << b2 << std::endl;
   Rcpp::Rcout << "thet1 = "       << thet1 << std::endl;
   Rcpp::Rcout << "zivar = "       << zivar << std::endl;
   Rcpp::Rcout << "perc(iplan) = " << perc.at(iplan - 1) << std::endl;
  
}

// Pick up index for the vectors
   index = (iplan - 1) * nlev + 1;

   if(debug::kprint > 1) {
     
      Rcpp::Rcout << "\npi = \n" << pi << std::endl;
      Rcpp::Rcout << "z = \n"    << z  << std::endl;
      Rcpp::Rcout << "index = "  << index << std::endl;
     
   }
   
   varret.at(iplan - 1) = var1(a,b1,b2,thet1,z,pi,zivar,nlev,
                               perc.at(iplan - 1),idist,knownt,
                               fp,pq,fisher,index - 1);
   
   if(debug::kprint > 1) {
     
      Rcpp::Rcout << "\niplan = "         << iplan - 1            << std::endl;
      Rcpp::Rcout << "varret(iplan) = " << varret.at(iplan - 1) << std::endl;
     
   }      
           
}

// Return only if single-plan input
   if(nplan > 1) goto exit;
   
   for(int i = 1; i <= npar; i++){
     
       for(int j = 1; j <= npar; j++){
         
           fret.at(i - 1,j - 1) = fisher.at(i - 1,j - 1);
         
       }
           
   }

exit: return Rcpp::List::create(Named("a") = a,
                                Named("b1") = b1,
                                Named("b2") = b2,
                                Named("thet1") = thet1,
                                Named("param") = param,
                                Named("z") = z,
                                Named("pi") = pi,
                                Named("zivar") = zivar,
                                Named("npar") = npar,
                                Named("nplan") = nplan,
                                Named("nlev") = nlev,
                                Named("perc") = perc,
                                Named("idist") = idist,
                                Named("knownt") = knownt,
                                Named("fret") = fret,
                                Named("varret") = varret);
   
}

#include <base/base.hpp>
#include <slsinf/lsinf.hpp>
#include <genmax/dinvx.hpp>
#include <utility/wqm_quant.hpp>
#include <vvar1/zfcdf.hpp>
#include <vvar1/fvrdes.hpp>

bool Close(double x1,
           double x2){
  
  double check_val = 1.0e-5;
  double mmax = std::max(std::abs(x1),std::abs(x2));
  
  bool test = ((std::abs(x1 - x2) / std::max(mmax,check_val)) < 1.0e-7);
  
  return test;
  
}

// Calculate variance of percentile at some stress level
// zivar is the variable defining that level
// (usually set to zero for design stress)

double var1(double a,
            double b1,
            double b2,
            double thet1,
            Rcpp::NumericVector z,
            Rcpp::NumericVector pi,
            double zivar,
            int nlev,
            double perc,
            int idist,
            int knownt,
            Rcpp::NumericVector fp,
            Rcpp::NumericVector pq,
            Rcpp::NumericMatrix fisher,
            int index){

Rcpp::NumericMatrix f(5,5);
Rcpp::NumericVector vec(4);
Rcpp::IntegerVector ir(5),jc(5);
double rho,zz;
double f11 = 0.0e00,f12 = 0.0e00,f22 = 0.0e00;
double pin,pif11,pif12,pif22;
double VAR1, zdummy = 0.0e00,uperc;
Rcpp::List LSINF;
int ifault = 0,ctn,dim,irank;
double tol;

// Zero the fisher information matrix
   for(int i = 1; i <= 4; i++){
     
       for(int j = 1; j <= 4; j++){
         
           f.at(i - 1,j - 1) = zero;
         
       }
   }

// Accumulate the fisher information matrix
   for(int i = 1; i <= nlev; i++){
     
       rho = std::pow(thet1,z.at(index + i - 1));
       zz  = (a - b1 * z.at(index + i - 1) - b2 * (std::pow(z.at(index + i - 1),2))) / rho;
       
       LSINF = lsinf(idist,2,zdummy,zz,f11,f12,f22,ifault);
       
       ifault = Rcpp::as<int>(Rcpp::as<Rcpp::List>(LSINF)["ifault"]);
       f11    = Rcpp::as<double>(Rcpp::as<Rcpp::List>(LSINF)["f11"]);
       f12    = Rcpp::as<double>(Rcpp::as<Rcpp::List>(LSINF)["f12"]);
       f22    = Rcpp::as<double>(Rcpp::as<Rcpp::List>(LSINF)["f22"]);
       
       if((Close(zz,a)) and (Close(pi.at(index + 0),one))) goto line75;
       
       fp.at(i - 1) = zfcdf(zz,idist);
       pq.at(i - 1) = fp.at(i - 1) * pi.at(index + i - 1);
       pin = pi.at(index + i - 1);
       pif11 = pin * f11;
       pif12 = pin * f12;
       pif22 = pin * f22;
       f.at(0,0) = f.at(0,0) + pif11 / (std::pow(rho,2));
       f.at(0,1) = f.at(0,1) + z.at(index + i - 1) * pif11 / (std::pow(rho, 2));
       f.at(1,1) = f.at(1,1) + (std::pow(z.at(index + i - 1),2)) * pif11 / (std::pow(rho, 2));
       
// Complete different fisher info matrices for 2 cases

// theta unknown:
   if(knownt == 1) {
     
      f.at(0,2) = f.at(0,2) + (1 - z.at(index + i - 1)) * pif12 / rho;
      f.at(0,3) = f.at(0,3) + z.at(index + i - 1) * pif12 / (rho * thet1);
      f.at(1,2) = f.at(1,2) + (1 - z.at(index + i - 1)) * z.at(index + i - 1) * pif12 / rho;
      f.at(1,3) = f.at(1,3) + (std::pow(z.at(index + i - 1),2)) * pif12 / (rho * thet1);
      f.at(2,2) = f.at(2,2) + std::pow((1 - z.at(index + i - 1)), 2) * pif22;
      f.at(2,3) = f.at(2,3) + (1 - z.at(index + i - 1)) * z.at(index + i - 1) * pif22 / thet1;
      f.at(3,3) = f.at(3,3) + (std::pow(z.at(index + i - 1), 2)) * pif22 / (std::pow(thet1, 2));
   
   }
   
// theta known:
   if(knownt == 2){
     
      f.at(0,2) = f.at(0,2) + pif12 / rho;
      f.at(1,2) = f.at(1,2) + z.at(index + i - 1) * pif12 / rho;
      f.at(2,2) = f.at(2,2) + pif22;
   
   }
   
if(knownt == 3){
  
   f.at(0,2) = f.at(0,2) + (std::pow(z.at(index + i - 1), 2)) * pif11;
   f.at(0,3) = f.at(0,3) + pif12;
   f.at(1,2) = f.at(1,2) + (std::pow(z.at(index + i - 1), 3)) * pif11;
   f.at(1,3) = f.at(1,3) + (z.at(index + i - 1)) * pif12;
   f.at(2,2) = f.at(2,2) + std::pow((z.at(index + i - 1)), 4) * pif11;
   f.at(2,3) = f.at(2,3) + std::pow((z.at(index + i - 1)), 2) * pif12;
   f.at(3,3) = f.at(3,3) + pif22;

}

if(debug::kprint > 1){
  
   Rcpp::Rcout << "\nVAR1**1\n" << std::endl;
   Rcpp::Rcout << "i = "      << i - 1 << std::endl;
   Rcpp::Rcout << "idist = "  << idist << std::endl;
   Rcpp::Rcout << "zz = "     << zz << std::endl;
   Rcpp::Rcout << "f11 = "    << f11 << std::endl;
   Rcpp::Rcout << "f12 = "    << f12 << std::endl;
   Rcpp::Rcout << "f22 = "    << f22 << std::endl;
   Rcpp::Rcout << "thet1 = "  << thet1 << std::endl;
   Rcpp::Rcout << "rho = "    << rho << std::endl;
   Rcpp::Rcout << "z(i) = "   << z.at(index + i - 1) << std::endl;
   Rcpp::Rcout << "pi(i) = "  << pi.at(index + i - 1) << std::endl;
   Rcpp::Rcout << "f(2,3) = " << f.at(1,2) << std::endl;
   Rcpp::Rcout << "f(3,3) = " << f.at(2,2) << std::endl;
  
}

   }

for(int i = 1; i <= 4; i++){
 
   for(int j = 1; j <= 4; j++){
     
       f.at(j - 1,i - 1) = f.at(i - 1, j - 1);
     
   }
}

// ******for unknown theta1:******;
// compute the variance-covariance matrix;
if(knownt == 1) {
  
   for(int i = 1; i <= 4; i++){
     
       for(int j = 1; j <= 4; j++){
         
           fisher.at(i - 1,j - 1) = f.at(i - 1,j - 1);
         
       }
   }
   
   ctn = 4;
   tol = 1.0e-12;
   dim = 5;
   dinvx(f,ctn,tol,ir,jc,irank,dim);
  
   if(debug::kprint > 1){
     
      Rcpp::Rcout << "\nvcv4 = \n" << f << std::endl;
     
   }
   
   if(irank != 4) goto line74;
   
   // Compute the variance of the pth percentile
      uperc = wqm_quant(perc,idist);
      vec.at(0) = one;
      vec.at(1) = zivar;
      vec.at(2) = uperc * (1 - zivar) * (std::pow(thet1, zivar));
      vec.at(3) = uperc * zivar * (std::pow(thet1, (zivar - one)));
      VAR1 = zero;
      
      for(int i = 1; i <= 4; i++){
        
          for(int j = 1; j <= 4; j++){
            
              VAR1 = VAR1 + f.at(i - 1,j - 1) * vec.at(i - 1) * vec.at(j - 1);
            
          }
      }
      
   goto line76;

}

// ******for known theta1:******
// Compute the variance-covariance matrix
if(knownt == 2) {
  
   for(int i = 1; i <= 3; i++){
     
       for(int j = 1; j <= 3; j++){
         
           fisher.at(i - 1,j - 1) = f.at(i - 1,j - 1);
         
       }
   }

   ctn = 3;
   tol = 1.0e-12;
   dim = 5;
   dinvx(f,ctn,tol,ir,jc,irank,dim);
  
   if(debug::kprint > 1){
     
      Rcpp::Rcout << "\nvcv3 = \n" << f << std::endl;
     
   }
  
   if(irank != 3) goto line74;
   
   // Compute the variance of the pth percentile
      uperc = wqm_quant(perc,idist);
      vec.at(0) = one;
      vec.at(1) = zivar;
      vec.at(2) = uperc * (std::pow(thet1, zivar));
      VAR1 = zero;
      
      for(int i = 1; i <= 3; i++){
        
          for(int j = 1; j <= 3; j++){
            
              VAR1 = VAR1 + f.at(i - 1,j - 1) * vec.at(i - 1) * vec.at(j - 1);
            
          }
      }
      
      goto line76;

}

// ******for unknown theta1:******;
// Compute the variance-covariance matrix
if(knownt == 3) {
  
   for(int i = 1; i <= 4; i++){
     
       for(int j = 1; j <= 4; j++){
         
           fisher.at(i - 1,j - 1) = f.at(i - 1,j - 1);
         
       }
   }

   ctn = 4;
   tol = 1.0e-12;
   dim = 5;
   dinvx(f,ctn,tol,ir,jc,irank,dim);
  
   if(debug::kprint > 1){
     
      Rcpp::Rcout << "\nvcv4* = \n" << f << std::endl;
     
   }

   if(irank != 4) goto line74;
   
   // Compute the variance of the pth percentile
      uperc = wqm_quant(perc,idist);
      vec.at(0) = one;
      vec.at(1) = zivar;
      vec.at(2) = std::pow(zivar, 2);
      vec.at(3) = uperc;
      VAR1 = zero;
      
      for(int i = 1; i <= 4; i++){
        
          for(int j = 1; j <= 4; j++){
            
              VAR1 = VAR1 + f.at(i - 1,j - 1) * vec.at(i - 1) * vec.at(j - 1);
            
          }
      }
      
      goto line76;

}

line74: Rcpp::Rcout << "\neplan error ***\n" << std::endl;

line75: VAR1 = fvrdes(a,perc,idist);

line76: if(debug::kprint != 0){
  
           Rcpp::Rcout << "\nEND OF VAR1\n" << std::endl;
           Rcpp::Rcout << "a = " << a << std::endl;
           Rcpp::Rcout << "b1 = " << b1 << std::endl;
           Rcpp::Rcout << "b2 = " << b2 << std::endl;
           Rcpp::Rcout << "thet1 = " << thet1 << std::endl;
           Rcpp::Rcout << "knownt = " << knownt << std::endl;
           Rcpp::Rcout << "pi = " << pi << std::endl;
           Rcpp::Rcout << "z = " << z << std::endl;
           Rcpp::Rcout << "fp = " << fp << std::endl;
           Rcpp::Rcout << "pq = " << pq << std::endl;
           Rcpp::Rcout << "nlev = " << nlev << std::endl;
           Rcpp::Rcout << "idist = " << idist << std::endl;
           Rcpp::Rcout << "perc = " << perc << std::endl;
           Rcpp::Rcout << "6.0e0 = " << 6.0e0 << std::endl;
           //Rcpp::Rcout << "var2 = " << var2 << std::endl;
  
} 

return VAR1;

}

#include <base/base.hpp>
#include <utility/wqm_quant.hpp>
#include <slsinf/lsinf.hpp>

//' compute the large sample variance for 
//' testing all units at the design stress

double fvrdes(double zz,
              double pval,
              int idist){
  
double up,upsq,tup,zdummy = 0.0e00,det;
double f11 = 0.0e00,f12 = 0.0e00,f22 = 0.0e00;
int ifault = 0;
Rcpp::List LSINF;

up = wqm_quant(pval,idist);
upsq = up * up;
tup = two * up;

LSINF = lsinf(idist,2,zdummy,zz,f11,f12,f22,ifault);

ifault = Rcpp::as<int>(Rcpp::as<Rcpp::List>(LSINF)["ifault"]);
f11    = Rcpp::as<double>(Rcpp::as<Rcpp::List>(LSINF)["f11"]);
f12    = Rcpp::as<double>(Rcpp::as<Rcpp::List>(LSINF)["f12"]);
f22    = Rcpp::as<double>(Rcpp::as<Rcpp::List>(LSINF)["f22"]);


det = f11 * f22 - f12 * f12;

return (f22 - tup * f12 + upsq * f11) / det;

}

#include <base/base.hpp>

//' smallest extreme value distribution cdf

double zfcdf(double z,
             int idist){

double zf_cdf = 0.0e00;
  
if(idist == 1) {
  
  if(z < -13.e00) return 1.0e-10;
  if(z > 3.0e00)  return one;
  
  zf_cdf = one - std::exp(-1 * std::exp(z));
  
}

if(idist == 2) {
  
   if(z < -9.0e00) return 1.0e-10;
   if(z < 9.0e00)  return one;
   
   zf_cdf = R::pnorm(z, 0, 1, true, false);
  
}

if(idist == 3) {
  
   zf_cdf = one / (one + std::exp(-z));
  
}

return zf_cdf;
        
}
