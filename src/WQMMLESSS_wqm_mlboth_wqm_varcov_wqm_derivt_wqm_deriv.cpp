#include <base/base.hpp>
#include <wqmsphiall/wqm_phiall.hpp>

inline bool logically_equal(double a, double b, double error_factor=1.0)
{
  return a==b || 
    std::abs(a-b)<std::abs(std::min(a,b))*std::numeric_limits<double>::epsilon()*
                  error_factor;
}


//' Compute second derivative factors needed 
//' for the fisher information matrix
//' 
//' qder.at(0) first partial derivative wrt mu
//' qder.at(1) first partial derivative wrt sigma
//' qder.at(2) second partial derivative wrt mu2
//' qder.at(3) second mixed partial derivative wrt mu*sigma
//' qder.at(4) second partial derivative wrt sigma2

void wqm_deriv(Rcpp::NumericVector &qder,
               int &itypep,
               double &z,
               double &z2,
               int &kdist){
  
int itype = std::abs(itypep);
double ez, zsq, phiq, phiqds, qd2;
double phib, phibm, phis, phip, phipdb;
double phib2 = 0.0e00, phibm2, phis2, phip2;
double phibd, phibd2, zphis, zphis2;
double phipd, phisd, zphisd;
double zphip,zphip2,zphipd,tphip,tphip2,tphipd;
  
for(int i = 1; i <= 5; i++){
  
    qder.at(i - 1) = zero;
  
}
  
if(itype == 0) goto exit;
  
if(itypep < 0) {
  
   if(itype == 1) goto line2011;
   if(itype == 2) goto line2013;
   if(itype == 3) goto line2012;
   if(itype == 4) goto line2014;
  
 } else {
  
   if(itype == 1) goto line2011;
   if(itype == 2) goto line2012;
   if(itype == 3) goto line2013;
   if(itype == 4) goto line2014;
  
}

// Failure time observation;
line2011: if((kdist == 1) or (kdist == 2)) goto line101;
          if((kdist == 3) or (kdist == 4)) goto line102;
          if((kdist == 5) or (kdist == 6)) goto line103;
          if((kdist == 7) or (kdist == 8)) goto line104;

// SEV distribution;
line101: ez = std::exp(z);
         qder.at(0) = ez - one;
         qder.at(1) = z * ez - z - one;
         qder.at(2) = ez;
         qder.at(3) = z * ez + qder.at(0);
         qder.at(4) = two * qder.at(1) + z * z * ez + one;
         
         goto exit;

// Normal distribution;
line102: zsq = z * z;
         qder.at(0) = z;
         qder.at(1) = zsq - one;
         qder.at(2) = one;
         qder.at(3) = qder.at(0) + z;
         qder.at(4) = qder.at(1) + two * zsq;
         
         goto exit;

// Logistic distribution (general except for phiq);
line103: wqm_phiall(phib,phibm,phis,phip,z,kdist);

         // phiq is the second derivative of the density;
            phiq = phip - two * (phis * phis + phip * phib);
            qder.at(0) = -phip / phis;
            qd2 = qder.at(0) * z;
            qder.at(1) = qd2 - one;
            phiqds = phiq / phis;
            qder.at(2) = -phiqds + qder.at(0) * qder.at(0);
            qder.at(3) = -z*phiqds + qder.at(0) + qder.at(0) * qd2;
            qder.at(4) = -z * z * phiqds + two * qd2 + qd2 * qd2 - one;
         
            goto exit;

// LEV distribution (general except for phiq)
line104: wqm_phiall(phib,phibm,phis,phip,z,kdist);
         
         // phiq is the second derivative of the density
            phiq = std::exp(-z) * (phib - two * phis + phip);
            qder.at(0) = -phip / phis;
            qd2 = qder.at(0) * z;
            qder.at(1) = qd2 - one;
            phiqds = phiq / phis;
            qder.at(2) = -phiqds + qder.at(0) * qder.at(0);
            qder.at(3) = -z * phiqds + qder.at(0) + qder.at(0) * qd2;
            qder.at(4) = -z * z * phiqds + two * qd2 + qd2 * qd2 - one;
            
            goto exit;

// Right censored observation
line2012: wqm_phiall(phib,phibm,phis,phip,z,kdist);
          
          phipdb = phip / phibm;
          qder.at(0) = phis / phibm;
          qder.at(1) = z * qder.at(0);
          qder.at(2) = phipdb + qder.at(0) * qder.at(0);
          qder.at(3) = z * phipdb + qder.at(0) + qder.at(0) * qder.at(1);
          qder.at(4) = z * z * phipdb + two * qder.at(1) + qder.at(1) * qder.at(1);
          
          goto exit;
          
// Left censored observation;
line2013: wqm_phiall(phib,phibm,phis,phip,z,kdist);
          
          phipdb = phip / phib;
          qder.at(0) = -phis / phib;
          qder.at(1) = z * qder.at(0);
          qder.at(2) = -phipdb + qder.at(0) * qder.at(0);
          qder.at(3) = -z * phipdb + qder.at(0) + qder.at(0) * qder.at(1);
          qder.at(4) = -z * z * phipdb + two * qder.at(1) + qder.at(1) * qder.at(1);
          
          goto exit;
          
// Interval observation;
line2014: wqm_phiall(phib,phibm,phis,phip,z,kdist);
          
          wqm_phiall(phib2,phibm2,phis2,phip2,z2,kdist);
          
          phibd = phib - phib2;
          phibd2 = phibm2 - phibm;
          phibd = std::max(phibd,phibd2);
          phipd = phip2 - phip;
          phisd = phis2 - phis;
          phibd = phib2 - phib;
          
          if(logically_equal(phibd, zero)) {
            
             for(int i = 1; i <= 5; i++){
               
                 qder.at(i - 1) = zero;
               
             }
            
             goto exit;
            
          }
            
          zphis  = z  * phis;
          zphis2 = z2 * phis2;
          zphisd = zphis2 - zphis;
          zphip  = z  * phip;
          zphip2 = z2 * phip2;
          zphipd = zphip2 - zphip;
          tphip  = z  * zphip;
          tphip2 = z2 * zphip2;
          tphipd = tphip2 - tphip;
          qder.at(0) = -1 * phisd  / phibd;
          qder.at(1) = -1 * zphisd / phibd;
          qder.at(2) = -1 * phipd  / phibd + qder.at(0) * qder.at(0);
          qder.at(3) = -1 * zphipd / phibd + qder.at(0) + qder.at(0) * qder.at(1);
          qder.at(4) = -1 * tphipd / phibd + two * qder.at(1) + qder.at(1) * qder.at(1);

exit: if(debug::kprint >= 7){
  
         Rcpp::Rcout << "\nWQM_DERIV**END**\n" << std::endl;
         Rcpp::Rcout << "phib = " << phip << std::endl;
         Rcpp::Rcout << "phip = " << phip << std::endl;
         Rcpp::Rcout << "phibm = " << phibm << std::endl;
         Rcpp::Rcout << "phis = " << phis << std::endl;
         
         if(itype == 1){
           
            Rcpp::Rcout << "ez = " << ez << std::endl;
            Rcpp::Rcout << "qd2 = " << qd2 << std::endl;
            Rcpp::Rcout << "phiq = " << phiq << std::endl;
            Rcpp::Rcout << "phiqds = " << phiqds << std::endl;
           
         }
         
         if((itype == 2) or (itype == 3)) {
           
            Rcpp::Rcout << "phipdb = " << phipdb << std::endl; 
           
         }
         
         if(itype == 4){
           
            Rcpp::Rcout << "phip2 = " << phip2 << std::endl;
            Rcpp::Rcout << "phibm2 = " << phibm2 << std::endl;
            Rcpp::Rcout << "phis2 = " << phis2 << std::endl;
            Rcpp::Rcout << "zphis = " << zphis << std::endl;
            Rcpp::Rcout << "zphis2 = " << zphis2 << std::endl;
            Rcpp::Rcout << "zphisd = " << zphisd << std::endl;
            Rcpp::Rcout << "zphip = " << zphip << std::endl;
            Rcpp::Rcout << "zphip2 = " << zphip2 << std::endl;
            Rcpp::Rcout << "zphipd = " << zphipd << std::endl;
            Rcpp::Rcout << "tphip = " << tphip << std::endl;
            Rcpp::Rcout << "tphip2 = " << tphip2 << std::endl;
            Rcpp::Rcout << "tphipd = " << tphipd << std::endl;
            Rcpp::Rcout << "phibd = " << phibd << std::endl;
            Rcpp::Rcout << "phibd2 = " << phibd2 << std::endl;
            Rcpp::Rcout << "phipd = " << phipd << std::endl;
            Rcpp::Rcout << "phisd = " << phisd << std::endl;
            
         }
         
      }
  
 return;

}