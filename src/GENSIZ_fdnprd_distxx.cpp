#include <base/base.hpp>
#include <utility/icheck.hpp>

using namespace fdnprd_g;

//' Set parameter types for specified model/data combination
//' 
//' @param kmod 
//' @param kdist
//' @param maxpn Maximum number of single distribution parameters 
//'        for input combination of kmod and kdist
//'        
//' @return igtyd A distribution parameter code vector ixiyiz. Where
//'         ix save space (including parameter) >=1,
//'         iy kodet range code
//'         iz label code (see pklab)
//'         
//' @return imarkd 1-loc 2-scale 3-prob 4-pwr 5-shape;

void distxx(int &kmod,
            int &kdist,
            int &llog,
            int &maxpn,
            Rcpp::IntegerVector &ipgty,
            Rcpp::IntegerVector &ipmark,
            int &npard,
            int &kmodp,
            int &ier){

npard = 0;

// Set llog marker to take logs if kdist is even
   if((kdist % 2) == 0) llog = 1;

if(kdist <= 100) {

if((kdist > 0) and (kdist <= 6)) {
  
    ipgty.at(0) = 111;
    ipgty.at(1) = 222;
    npard = npard + 2;
    
    // Mark the scale parameter position for easy access later
       ipmark.at(0) = 1;
       ipmark.at(1) = 2;
  
}

if((kdist == 7) or (kdist == 8)) {
  
    // Exponential distribution
       ipgty.at(0) = 111;
       ipmark.at(0) = 1;
       npard = npard + 1;
  
}

if((kdist == 9) or (kdist == 10)) {
  
    // Generalized gamma distribution
       ipgty.at(0) = 111;
       ipgty.at(1) = 222;
       ipgty.at(2) = 544;
       
    // Mark scale location
    // Mark the scale parameter position for easy access later
       ipmark.at(0) = 1;
       ipmark.at(1) = 2;
       
    // Mark the shape parmaeter
       ipmark.at(2) = 5;
       npard = npard + 3;
      
}

if((kdist == 11) or (kdist == 12)) {
  
    // Ordinary gamma distribution
       ipgty.at(0) = 111;
       ipgty.at(1) = 544;
       npard = npard + 2;
       
    // Mark the shape parmaeter
       ipmark.at(0) = 1;
       ipmark.at(1) = 5;

}
      
}

//yy call usdist(kdist,igtyd,imarkd,npard);
if(kmod <= 100) {

   icheck(kmod,0,4,0,0,ier,-5222);
   kmodp = kmod + 1;
  
if(!((kmodp == 1) or (kmodp == 6))) {

if(kmodp == 2) {
  
   // Pick up lfp parameter
      npard = npard + 1;
      ipgty.at(npard - 1) = 233;
   // Mark the probability parameter position for easy access later
      ipmark.at(npard - 1) = 3;
  
}

if(kmodp == 3) {
  
   // Pick up doa parameter
      npard = npard + 1;
      ipgty.at(npard - 1) = 236;
   // Mark the probability parameter position for easy access later
      ipmark.at(npard - 1) = 3;
      
}

if(kmodp == 4) {
  
   // Pick up exponential part of the steady state model
      npard = npard + 1;
      ipgty.at(npard - 1) = 115;
      ipmark.at(npard - 1) = 4;
   
}

if(kmodp == 5) {
  
   // Pick up power parameter for the pph model
      npard = npard + 1;
      ipgty.at(npard - 1) = 228;
   // Mark the power parameter position for easy access later
      ipmark.at(npard - 1) = 4;

}
}
}

 icheck(npard,1,maxpn,1,maxpn,ier,98122);

if(debug::kprint >= 4){
  
   for(int i = 0; i < npard; i++){
     
       Rcpp::Rcout << "\nDISTXX iter = " << i << std::endl;
       Rcpp::Rcout << "kdist = "         << kdist << std::endl;
       Rcpp::Rcout << "kmod = "          << kmod  << std::endl;
       Rcpp::Rcout << "kmodp = "         << kmodp << std::endl;
       Rcpp::Rcout << "npard = "         << npard << std::endl;
       Rcpp::Rcout << "igtyd(i) = "      << ipgty.at(i) << std::endl;
       Rcpp::Rcout << "ipmark(i) = "     << ipmark.at(i) << std::endl;
       Rcpp::Rcout << "maxpn = "         << maxpn << std::endl;
     
   }
   
}

return;

}