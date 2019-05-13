#include <base/base.hpp>

double powerc(double x,
              double power,
              int mark){
  
double ten = 10.0e00, bigexp = 30.0e00;
double power_c = zero,xlog,prod;

if(x == zero) return power_c;
if(x <= zero){
  
   Rcpp::warning("x <= zero in powerc");
   Rcpp::Rcout << "\nPOWERC\n" << std::endl;
   Rcpp::Rcout << "x = " << x << std::endl;
   Rcpp::Rcout << "power = " << power << std::endl;
   Rcpp::Rcout << "xlog = " << xlog << std::endl;
   Rcpp::Rcout << "prod = " << prod << std::endl;
   Rcpp::Rcout << "powerc = " << power_c << std::endl;
   
   return power_c;
  
}

if(x == one){
  
   power_c = one;
   return power_c;
  
}

xlog = std::log10(x);
prod = xlog * power;

if(prod > bigexp){
  
   Rcpp::warning("\nprod (%f) > bigexp (%f) in powerc.\nAlso, power = %f \n x = %f \n xlog = %f \n powerc = %f", prod,bigexp,power,x,xlog,power_c);
   
   return power_c;
  
}

if(prod < -bigexp) return power_c;

power_c = std::pow(ten,prod);

return power_c;
        
}
