#include <base/base.hpp>

//  Find critical value of beta2 given beta1

double gdeg(int kdmodp,
            double dfp,
            double d0p,
            double sfactp,
            double tfp,
            double beta1){

double xm, g_deg = 0;

if(kdmodp == 1) {

  // get the asymptote that will just cause failure
  g_deg = std::log(-dfp / (one - std::exp( -tfp * std::exp(beta1))));

}

if(kdmodp == 2) {

  xm = beta1;
  // get the pre-exponential c that will just cause failure

  double power = one - xm / two;

  if(xm == two) {

    g_deg =  std::log(dfp / d0p) / ((std::pow(sfactp,2)) * tfp);

  } else {

    g_deg = (std::pow(dfp,power) - std::pow(d0p,power)) /(power * (std::pow(sfactp,xm))*tfp);

  }
  
}

return g_deg;

}
