#include <base/base.hpp>
#include <utility/dmachine.hpp>

//**date written 800101 (yymmdd);
//**revision date 830518 (yymmdd);
//**category no. h2a3a2,h2a4a2;
//**keywords 15-point transformed gauss-kronrod rules;
//**author piessens,robert,appl. math. & progr. div. - k.u.leuven;
// de doncker,elise,appl. math. & progr. div. - k.u.leuven;
//**purpose the original (infinite integration range is mapped;
// onto the interval (0,1) and (a,b) is a part of (0,1).;
// it is the purpose to compute;
// i = integral of transformed integrand over (a,b),;
// j = integral of abs(transformed integrand) over (a,b).;
//**description;
// integration rule;
// standard fortran subroutine;
// double precision version;
// parameters;
// on entry;
// f - double precision;
// fuction subprogram defining the integrand;
// function f(x). the actual name for f needs to be;
// declared e x t e r n a l in the calling program.;
// boun - double precision;
// finite bound of original integration;
// range (set to zero if inf = +2);
// inf - integer;
// if inf = -1, the original interval is;
// (-infinity,bound),;
// if inf = +1, the original interval is;
// (bound,+infinity),;
// if inf = +2, the original interval is;
// (-infinity,+infinity) and;
// the integral is computed as the sum of two;
// integrals, one over (-infinity,0) and one over;
// (0,+infinity).;
// a - double precision;
// lower limit for integration over subrange;
// of (0,1);
// b - double precision;
// upper limit for integration over subrange;
// of (0,1);
// on return;
// result - double precision;
// approximation to the integral i;
// result is computed by applying the 15-point;
// kronrod rule(resk) obtained by optimal addition;
// of abscissae to the 7-point gauss rule(resg).;
// abserr - double precision;
// estimate of the modulus of the absolute error,;
// which should equal or exceed abs(i-result);
// resabs - double precision;
// approximation to the integral j;
// resasc - double precision;
// approximation to the integral of;
// abs((transformed integrand)-i/(b-a)) over (a,b);
//**references (none);
//**routines called d1mach;

void dqk15i(double (*f)(double),
            double &boun,
            int &inf,
            double &a,
            double &b,
            double &result,
            double &abserr,
            double &resabs,
            double &resasc){
  
double absc,absc1,absc2,centr,dinf;
double epmach,fc,fsum,fval1,fval2,hlgth;
double resg,resk,reskh,tabsc1,tabsc2,uflow;
Rcpp::NumericVector fv1(7),fv2(7),xgk(8),wgk(8),wg(8);

// The abscissae and weights are supplied for the interval (-1,1). 
// Because of symmetry only the positive abscissae and their corresponding weights are given.
// xgk - abscissae of the 15-point kronrod rule
// xgk(2), xgk(4), ... abscissae of the 7-point gauss rule
// xgk(1), xgk(3), ... abscissae which are optimally added to the 7-point gauss rule
// wgk - weights of the 15-point kronrod rule;
// wg - weights of the 7-point gauss rule, corresponding to the abscissae xgk(2), xgk(4)
// wg(1), wg(3), ... are set to zero

wg = Rcpp::NumericVector::create(0.0e0,
                                 0.129484966168869693270611432679082e0,
                                 0.0e0,
                                 0.279705391489276667901467771423780e0,
                                 0.0e0,
                                 0.381830050505118944950369775488975e0,
                                 0.0e0,
                                 0.417959183673469387755102040816327e0);

xgk = Rcpp::NumericVector::create(0.991455371120812639206854697526329e0,
                                  0.949107912342758524526189684047851e0,
                                  0.864864423359769072789712788640926e0,
                                  0.741531185599394439863864773280788e0,
                                  0.586087235467691130294144838258730e0,
                                  0.405845151377397166906606412076961e0,
                                  0.207784955007898467600689403773245e0,
                                  0.000000000000000000000000000000000e0);

wgk = Rcpp::NumericVector::create(0.022935322010529224963732008058970e0,
                                  0.063092092629978553290700663189204e0,
                                  0.104790010322250183839876322541518e0,
                                  0.140653259715525918745189590510238e0,
                                  0.169004726639267902826583426598550e0,
                                  0.190350578064785409913256402421014e0,
                                  0.204432940075298892414161999234649e0,
                                  0.209482141084727828012999174891714e0);

// List of major variables
// ----------------------------------------------------------
// centr - mid point of the interval
// hlgth - half-length of the interval
// absc* - abscissa
// tabsc* - transformed abscissa
// fval* - function value
// resg - result of the 7-point gauss formula
// resk - result of the 15-point kronrod formula
// reskh - approximation to the mean value of the transformed
// integrand over (a,b), i.e. to i/(b-a)

// machine dependent constants
// -----------------------------------------------------------
// epmach is the largest relative spacing
// uflow is the smallest positive magnitude
   epmach = D1mach(4);
   uflow = D1mach(1);
   dinf = std::min(1,inf);
   centr = 0.5e+00 * (a + b);
   hlgth = 0.5e+00 * (b - a);
   tabsc1 = boun + dinf * (0.1e+01 - centr) / centr;
   fval1 = f(tabsc1);
   if(inf == 2) fval1 = fval1 + f(-tabsc1);
   fc = (fval1 / centr) / centr;
   
// Compute the 15-point kronrod approximation to the integral, and estimate the error
   resg = wg.at(7) * fc;
   resk = wgk.at(7) * fc;
   resabs = std::abs(resk);
   
for(int j = 1; j <= 7; j++){

    absc = hlgth * xgk.at(j - 1);
    absc1 = centr - absc;
    absc2 = centr + absc;
    tabsc1 = boun + dinf * (0.1e+01 - absc1) / absc1;
    tabsc2 = boun + dinf * (0.1e+01 - absc2) / absc2;
    fval1 = f(tabsc1);
    fval2 = f(tabsc2);
    if(inf == 2) fval1 = fval1 + f(-tabsc1);
    if(inf == 2) fval2 = fval2 + f(-tabsc2);
    fval1 = (fval1 / absc1) / absc1;
    fval2 = (fval2 / absc2) / absc2;
    fv1.at(j - 1) = fval1;
    fv2.at(j - 1) = fval2;
    fsum = fval1 + fval2;
    resg = resg + wg.at(j - 1) * fsum;
    resk = resk + wgk.at(j - 1) * fsum;
    resabs = resabs + wgk.at(j - 1) * (std::abs(fval1) + std::abs(fval2));

}

reskh = resk * 0.5e+00;
resasc = wgk.at(7) * std::abs(fc - reskh);

for(int j = 1; j <= 7; j++){

    resasc = resasc + wgk.at(j - 1) * (std::abs(fv1.at(j - 1) - reskh) + std::abs(fv2.at(j - 1) - reskh));

}

result = resk * hlgth;
resasc = resasc * hlgth;
resabs = resabs * hlgth;
abserr = std::abs((resk - resg) * hlgth);

if((resasc != 0.0e+00) and (abserr != 0.e0)){
  
    abserr = resasc * std::min(0.1e+01,std::pow(0.2e+03 * abserr / resasc, 1.5e+00));

}

if(resabs > (uflow / (0.5e+02 * epmach))){
  
   abserr = std::max((epmach * 0.5e+02) * resabs,abserr);
  
}

return;

}

