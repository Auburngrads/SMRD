#include <base/base.hpp>
#include <utility/dmachine.hpp>

//' date written   800101   (yymmdd)
//' revision date  830518   (yymmdd)
//' category no.  h2a1a2
//' keywords  21-point gauss-kronrod rules
//' author  piessens,robert,appl. math. & progr. div. - k.u.leuven
//'         de doncker,elise,appl. math. & progr. div. - k.u.leuven
//' purpose  to compute i = integral of f over (a,b), with error
//'                         estimate
//'                     j = integral of abs(f) over (a,b)
//' description
//' '        integration rules
//'         standard fortran subroutine
//'         double precision version
//' '        parameters
//'          on entry
//'            f      - double precision
//'                     function subprogram defining the integrand
//'                     function f(x). the actual name for f needs to be
//'                     declared e x t e r n a l in the driver program.
//' '           a      - double precision
//'                     lower limit of integration
//' '           b      - double precision
//'                     upper limit of integration
//' '         on return
//'             result - double precision
//'                     approximation to the integral i
//'                     result is computed by applying the 21-point
//'                     kronrod rule (resk) obtained by optimal addition
//'                     of abscissae to the 10-point gauss rule (resg).
//' '           abserr - double precision
//'                     estimate of the modulus of the absolute error,
//'                     which should not exceed abs(i-result)
//' '           resabs - double precision
//'                     approximation to the integral j
//' '           resasc - double precision
//'                     approximation to the integral of abs(f-i/(b-a))
//'                     over (a,b)
//' ' references  (none)
//' routines called  d1mach

void dqk21(double (*f)(double),
           double &a,
           double &b,
           double &result,
           double &abserr,
           double &resabs,
           double &resasc){

double absc,centr,dhlgth,epmach;
double fc,f_sum,fval1,fval2,hlgth;
double resg,resk,reskh,uflow;
int jtw,jtwm1;

Rcpp::NumericVector fv1(10), fv2(10);
Rcpp::NumericVector wg(5), wgk(11), xgk(11);

//           the abscissae and weights are given for the interval (-1,1).
//           because of symmetry only the positive abscissae and their
//           corresponding weights are given.
//
//           xgk    - abscissae of the 21-point kronrod rule
//                    xgk(2), xgk(4), ...  abscissae of the 10-point
//                    gauss rule
//                    xgk(1), xgk(3), ...  abscissae which are optimally
//                    added to the 10-point gauss rule
//
//           wgk    - weights of the 21-point kronrod rule
//
//           wg     - weights of the 10-point gauss rule
//
//
// gauss quadrature weights and kronron quadrature abscissae and weights
// as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
// bell labs, nov. 1981.

wg = NumericVector::create(0.066671344308688137593568809893332e0,
                           0.149451349150580593145776339657697e0,
                           0.219086362515982043995534934228163e0,
                           0.269266719309996355091226921569469e0,
                           0.295524224714752870173892994651338e0);


xgk = NumericVector::create(0.995657163025808080735527280689003e0,
                            0.973906528517171720077964012084452e0,
                            0.930157491355708226001207180059508e0,
                            0.865063366688984510732096688423493e0,
                            0.780817726586416897063717578345042e0,
                            0.679409568299024406234327365114874e0,
                            0.562757134668604683339000099272694e0,
                            0.433395394129247190799265943165784e0,
                            0.294392862701460198131126603103866e0,
                            0.148874338981631210884826001129720e0,
                            0.000000000000000000000000000000000e0);

wgk = NumericVector::create(0.011694638867371874278064396062192e0,
                            0.032558162307964727478818972459390e0,
                            0.054755896574351996031381300244580e0,
                            0.075039674810919952767043140916190e0,
                            0.093125454583697605535065465083366e0,
                            0.109387158802297641899210590325805e0,
                            0.123491976262065851077958109831074e0,
                            0.134709217311473325928054001771707e0,
                            0.142775938577060080797094273138717e0,
                            0.147739104901338491374841515972068e0,
                            0.149445554002916905664936468389821e0);

//           list of major variables
//           -----------------------
//
//           centr  - mid point of the interval
//           hlgth  - half-length of the interval
//           absc   - abscissa
//           fval*  - function value
//           resg   - result of the 10-point gauss formula
//           resk   - result of the 21-point kronrod formula
//           reskh  - approximation to the mean value of f over (a,b),
//                    i.e. to i/(b-a)
//
//
//           machine dependent constants
//           ---------------------------
//
//           epmach is the largest relative spacing.
//           uflow is the smallest positive magnitude.

epmach = D1mach(4);
uflow  = D1mach(1);
centr  = 0.5e+00 * (a + b);
hlgth  = 0.5e+00 * (b - a);
dhlgth = std::abs(hlgth);

//  compute the 21-point kronrod approximation to
//  the integral, and estimate the absolute error.

resg = 0.0e+00;
fc = f(centr);
resk = wgk.at(10) * fc;
resabs = std::abs(resk);

for(int j = 1; j <= 5; j++){

    jtw = 2 * j;
    absc = hlgth * xgk.at(jtw - 1);
    fval1 = f(centr - absc);
    fval2 = f(centr + absc);
    fv1.at(jtw - 1) = fval1;
    fv2.at(jtw - 1) = fval2;
    f_sum = fval1 + fval2;
    resg = resg + wg.at(j - 1) * f_sum;
    resk = resk + wgk.at(jtw - 1) * f_sum;
    resabs = resabs + wgk.at(jtw - 1) * (std::abs(fval1) + std::abs(fval2));

}

for(int j = 1; j <= 5; j++){

    jtwm1 = 2 * j - 1;
    absc = hlgth * xgk.at(jtwm1 - 1);
    fval1 = f(centr - absc);
    fval2 = f(centr + absc);
    fv1.at(jtwm1 - 1) = fval1;
    fv2.at(jtwm1 - 1) = fval2;
    f_sum = fval1 + fval2;
    resk = resk + wgk.at(jtwm1 - 1) * f_sum;
    resabs = resabs + wgk.at(jtwm1 - 1) * (std::abs(fval1) + std::abs(fval2));

}

reskh  = resk * 0.5e+00;
resasc = wgk.at(10) * std::abs(fc - reskh);

for(int j = 1; j <= 10; j++){

    resasc = resasc + wgk.at(j - 1) * (std::abs(fv1.at(j - 1) - reskh) + std::abs(fv2.at(j - 1) - reskh));

}

result = resk * hlgth;
resabs = resabs * dhlgth;
resasc = resasc * dhlgth;
abserr = std::abs((resk - resg) * hlgth);

if((resasc != 0.0e+00) and (abserr != 0.0e+00)) {

    abserr = resasc * std::min(0.1e+01, std::pow((0.2e+03 * abserr / resasc),1.5e+00));

}

if(resabs > (uflow / (0.5e+02 * epmach))) {

   abserr = std::max((epmach * 0.5e+02) * resabs,abserr);

}

return;

}
