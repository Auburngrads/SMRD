#include <base/base.hpp>

// inline bool logically_equal(float a, float b, double error_factor=1.0)
// {
//   return a==b || 
//     std::abs(a-b)<std::abs(std::min(a,b))*std::numeric_limits<float>::epsilon()*error_factor;
// }

// bool approximatelyEqual(double a, double b, double epsilon)
// {
//     return std::abs(a - b) <= ( (std::abs(a) < std::abs(b) ? std::fabs(b) : std::fabs(a)) * epsilon);
// }
// 
// bool essentiallyEqual(double a, double b, double epsilon)
// {
//     return std::abs(a - b) <= ( (std::abs(a) > std::abs(b) ? std::abs(b) : std::abs(a)) * epsilon);
// }

// bool definitelyGreaterThan(float a, float b)
// {
//     return (a - b) > ( (std::abs(a) < std::abs(b) ? std::abs(b) : std::abs(a)) * std::numeric_limits<float>::epsilon());
// }
// 
// bool definitelyLessThan(float a, float b)
// {
//     return  (b - a) > ( (std::abs(a) < std::abs(b) ? std::abs(b) : std::abs(a)) * std::numeric_limits<float>::epsilon());
// }


//' Replace a three way arithmetic if in FORTRAN

LogicalVector threeCheck(double x, 
                         double epsilon) {
  
  Rcpp::LogicalVector chk = LogicalVector(3);

  for(int i = 0; i < 3; i++){
    
      chk.at(i) = false;
    
  }
  
// if(logically_equal(x, 0.0e00)) {
//   
//     chk.at(1) = true;
//     return chk;
//   
// }
   
// if(definitelyGreaterThan(x, 0.0e00)) {
//   
//    chk.at(2) = true;
//    return chk;
//   
// }
// 
// if(definitelyLessThan(x, 0.0e00)) {
//   
//    chk.at(0) = true;
//    return chk;
//   
// }

// if((!chk.at(0)) & (!chk.at(1)) & (!chk.at(2))){
//   
//    Rcpp::stop("None these bitches working");
//   
// }

   return chk;

}

/*** R
threeCheck(x = as.double(4.01 - 2.002), epsilon = 1e-12)
*/
