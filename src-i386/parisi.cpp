#include <base/base.hpp>

//' Compute cracks according to paris law, allowing different
//' parameters at each time interval

void parisi(Rcpp::NumericVector &times,
            int &number_times,
            Rcpp::NumericVector &paris_n,
            Rcpp::NumericVector &paris_c,
            Rcpp::NumericVector &paris_sigma, 
            double &a0,
            Rcpp::NumericVector &crack_size){

double time_difference, xpower;  

// Initilize the crack
   crack_size.at(0) = a0;

// Loop over times where solution is needed
   for(int itime = 2; itime <= number_times; itime++){
     
       time_difference = times.at(itime - 1) - times.at(itime - 2);

       // Diffyq solution is different for paris_n=2
          if(paris_n.at(itime - 1) == two){
            
             crack_size.at(itime - 1) = crack_size.at(itime - 2) *\
                                        std::exp(paris_c.at(itime - 1) *\
                                        std::pow(paris_sigma.at(itime - 1), paris_n.at(itime - 1)) *\
                                        time_difference);
            
           } else {
            
             xpower = (1 - paris_n.at(itime - 1) / two);
             
             crack_size.at(itime - 1) = std::pow(std::pow(crack_size.at(itime - 2), xpower) + xpower * paris_c.at(itime - 1) * (std::pow(paris_sigma.at(itime - 1), paris_n.at(itime - 1))) * time_difference, (two / (two - paris_n.at(itime - 1))));
             
           }
              
              
   }
   
return;

}