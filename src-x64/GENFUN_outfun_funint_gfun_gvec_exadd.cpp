#include <base/base.hpp>

//' extended precision add
//' accum(1)=0;accum(2)=0.
//' for(i=1,n)(call exadd(x(i),accum)))
//' ans=accum(1)+accum(2)

void exadd(double a,
           Rcpp::NumericVector &accum){
  
double big, small, reg,sum;
  
if(std::abs(accum.at(0)) >= std::abs(a)) goto line1;

big = a;
small = accum.at(0);
goto line2;

line1: big = accum.at(0);
       small = a;
       
line2: sum = big + small;
       reg = (big - sum) + small;
       reg = reg + accum.at(1);
       accum.at(0) = sum + reg;
       accum.at(1) = (sum - accum.at(0)) + reg;
       
return;
      
}