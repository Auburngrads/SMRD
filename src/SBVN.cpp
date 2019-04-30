#include <base/base.hpp>
#include <sbvn/bvnprb.hpp>

//' Compute the probability that a bivariate normal with mean vectorc
// [[Rcpp::export]]
Rcpp::List SBVN(Rcpp::NumericVector ah,
                Rcpp::NumericVector ak,
                Rcpp::NumericVector xmu1,
                Rcpp::NumericVector xmu2,
                Rcpp::NumericVector v1,
                Rcpp::NumericVector v2,
                Rcpp::NumericVector c12,
                Rcpp::NumericVector prob,
                int n,
                int kprint){
  
debug::kprint = kprint;
  
for(int i = 1; i <= n; i++){
  
    bvnprb(i,ah,ak,xmu1,xmu2,v1,v2,c12,prob);
  
    if(debug::kprint > 0) {
      
       Rcpp::Rcout << "\nSBVN\n"           << std::endl;
       Rcpp::Rcout << "i = "     << i - 1  << std::endl;
       Rcpp::Rcout << "prob = "  << prob   << std::endl;
       Rcpp::Rcout << "ah = "    << ah     << std::endl;
       Rcpp::Rcout << "ak = "    << ak     << std::endl;
       Rcpp::Rcout << "xmu1 = "  << xmu1   << std::endl;
       Rcpp::Rcout << "xmu2 = "  << xmu2   << std::endl;
       Rcpp::Rcout << "v1 = "    << v1     << std::endl;
       Rcpp::Rcout << "v2 = "    << v2     << std::endl;
       Rcpp::Rcout << "c12 = "   << c12    << std::endl;
      
    }
}

return Rcpp::List::create(Named("ah") = ah,
                          Named("ak") = ak,
                          Named("xmu1") = xmu1,
                          Named("xmu2") = xmu2,
                          Named("v1") = v1,
                          Named("v2") = v2,
                          Named("c12") = c12,
                          Named("prob") = prob,
                          Named("n") = n);

}

#include <base/base.hpp>
#include <sbvn/bivnor.hpp>

//' @description Compute the probability that a bivariate 
//'              normal with mean \code{vectorc(xmu1,xmu2)}, 
//'              variances \code{v1} and \code{v2} and 
//'              covariance \code{c12} has \code{x1 > ah}
//'              and \code{x2 > ak}.
//'              
//' @source Daniel F. Heitjan 17 March 1991
void bvnprb(int &i,
            Rcpp::NumericVector &ah,
            Rcpp::NumericVector &ak,
            Rcpp::NumericVector &xmu1,
            Rcpp::NumericVector &xmu2,
            Rcpp::NumericVector &v1,
            Rcpp::NumericVector &v2,
            Rcpp::NumericVector &c12,
            Rcpp::NumericVector &prob){

double ahi,aki,r;
  
ahi = (ah.at(i - 1) - xmu1.at(i - 1)) / std::sqrt(v1.at(i - 1));
aki = (ak.at(i - 1) - xmu2.at(i - 1)) / std::sqrt(v2.at(i - 1));
r   = c12.at(i - 1) / std::sqrt(v1.at(i - 1) * v2.at(i - 1));

prob.at(i - 1) = bivnor(ahi,aki,r);

return;

}

#include <base/base.hpp>
#include <sbvn/derf.hpp>

double gauss(double t) {
  
  return (one + derf(t / std::sqrt(two))) / two;
  
  }

double bivnor(double ah,
              double ak,
              double r){
  
double b,xah,xak,gh,gk,rr,h2;
double a2,h4,ex,w2,ap,s2,sp,s1,sn,sgn,sqr,con,wh,wk,gw;
double t,g2,conex,cn,biv_nor;
double explim = 80.0e00, twopi = 6.283185307179587e00;
double four = 4.0e00, quart = 0.25e00;
double epsilon = 1.0e-40;
int idig = 15,is;
     
b = zero;
      
if(ah == zero){
  
	 xah = ak;
	 xak = ah;
	 
 } else {
  
	 xah = ah;
	 xak = ak;
}

gh = gauss(-xah) / two;
gk = gauss(-xak) / two;

if(r == zero) {
  
	 b = four * gh * gk;
	 goto line350;
	 
}

rr = one - r * r;

if(rr < zero) {
  
   if(debug::kprint > 0){
     
      Rcpp::Rcout << "\nerror in bivnor, r = " << r << std::endl;
     
   }
   
	 goto line390;
   
}

if(rr <= zero) {

if(r < zero) {
  
   if(xah + xak >= zero) goto line350;
   
   b = two * (gh + gk) - one;
   goto line350;

}

if((xah - xak) < zero) {
  
	  b = two * gk;
  
  } else {
  
	  b = two * gh;
  
}
  
  goto line350;

}
      
sqr = std::sqrt(rr);

if(idig == 15) {
  
	 con = twopi * 1.0e-15 / two;
  
 } else {
  
   // con=twopi/two/10**idig;
	 con = std::exp(std::log(twopi) - std::log(two) - std::log(10.0e00) * idig);
  
}

if(xah == zero) {
  
   if(xak != zero) goto line190;
   b = std::atan(r / sqr) / twopi + quart;
   goto line350;
      
}
 
b = gh;

if(std::abs(xah * xak) < epsilon) goto line200;
if((xah * xak) > zero) goto line190;

b = b - half;

line190: b = b + gk;

line200: wh = -xah;
         wk = (xak / xah - r) / sqr;
         gw = two * gh;
         is = -1;
         
line210: sgn = -1 * one;
         t = zero;
         if(wk == zero) goto line320;
         
         if(std::abs(std::abs(wk) - one) < epsilon) goto line230;
         if(std::abs(wk) < one) goto line270;
         if(std::abs(wk) > one) goto line240;
         
line230: t = wk * gw * (one - gw) / two;
         goto line310;
         
line240: sgn = -sgn;
         wh = wh * wk;
         g2 = gauss(wh);
         wk = one / wk;
         if(wk < zero) b = b + half;
         b = b - (gw + g2) / two + gw * g2;
         
line270: h2 = wh * wh;
         a2 = wk * wk;
         h4 = h2 / two;
         
if(h4 < explim) {
  
   ex = std::exp(-1 * h4);
  
 } else {
  
	 ex = zero;
  
}
 
w2 = h4 * ex;
ap = one;
s2 = ap - ex;
sp = ap;
s1 = zero;
sn = s1;

conex = std::abs(con / wk);
goto line290;

line280: sn = sp;
         sp = sp + one;
         s2 = s2 - w2;
         w2 = w2 * h4 / sp;
         ap = -ap * a2;
         
line290: cn = ap * s2 / (sn + sp);
         s1 = s1 + cn;
         if((std::abs(cn) - conex) > zero) goto line280;

         t = (std::atan(wk) - wk * s1) / twopi;
         
line310: b = b + sgn * t;
line320: if(is >= 0) goto line350;

if(xak != zero) {
  
	 wh = -xak;
	 wk = (xah / xak - r) / sqr;
	 gw = two * gk;
	 is = 1;
	 goto line210;
	 
}

line350: if(b < zero) b = zero;
         if(b > one)  b = one;
         
line390: biv_nor = b;

      return biv_nor;
        
}

#include <base/base.hpp>
#include <sbvn/wqm_calerf.hpp>

//' Compute the error function
//' @source W. J. Cody - January 8, 1985

double derf(double x){
  
int jint = 0;
double result = 0.0e00;

wqm_calerf(x,result,jint);

return result;   

}

#include <base/base.hpp>
#include <sbvn/wqm_calerf.hpp>

//' Compute the error function
//' @source W. J. Cody - January 8, 1985

double derfc(double x){
  
int jint = 1;
double result = 0.0e00;

wqm_calerf(x,result,jint);

return result;   

}

#include <base/base.hpp>

// this packet computes the error and complimentary error functions;
// for real arguments arg. it contains two function type;
// subprograms, erf and erfc (or derf and wqm_dxerc), and one;
// subroutine type subprogram, wqm_calerf. the calling statements;
// for the primary entries are;
// y=erf(x) (or y=derf(x) );
// and;
// y=erfc(x) (or y=dxerc(x) ).;
// the routine wqm_calerf is intended for internal packet use only,;
// all computations within the packet being concentrated in this;
// routine. the function subprograms invoke calerf with the;
// statement;
// call calerf(arg,result,jint);
// where the parameter usage is as follows;
// function parameters for calerf;
// call arg result jint;
// erf(arg) any real argument erf(arg) 0;
// erfc(arg) abs(arg) < xmax erfc(arg) 1;
// the main computation evaluates near minimax approximations;
// from 'rational chebyshev approximations for the error function';
// by w. j. cody, math. comp., 1969, pp. 631-638. this;
// transportable program uses rational functions that theoretically;
// approximate erf(x) and erfc(x) to at least 18 significant;
// decimal digits. the accuracy achieved depends on the arithmetic;
// system, the compiler, the intrinsic functions, and proper;
// selection of the machine-dependent constants.;
//******************************************************************;
//******************************************************************;
//explanation of machine-dependent constants;
// xsmall = argument below which erf(x) may be represented;
// by 2*x/sqrt(pi) and above which x*x will;
// not underflow. a conservative value is the;
// largest x such that 1.0 + x = 1.0 to machine;
// precision.;
// xmax = largest argument acceptable to erfc; solution to;
// equation: w(x) * (1-0.5/x**2) = xmin, where;
// w(x) = exp(-x*x)/(x*sqrt(pi)), and xmin is the;
// smallest positive machine number (see table below).;
// approximate values for some important machines are:;
// xsmall xmax xmin;
// ibm 195 (d.p.) 1.39d-17 13.306 5.40d-79;
// cdc 7600 (s.p.) 7.11e-15 25.922 3.13e-294;
// cray-1 (s.p.) 7.11e-15 75.326 4.58e-2467;
// univac 1108 (d.p.) 1.73d-18 26.582 2.78d-309;
// vax 11/780 (s.p.) 5.96e-8 9.269 2.94e-39;
// vax 11/780 (d.p.) 1.39d-17 9.269 2.94d-39;
// ibm pc (s.p.) 5.96e-8 9.194 1.18e-38;
// ibm pc (d.p.) 1.11d-16 26.543 2.23d-308;
//******************************************************************;
//******************************************************************;
//error returns;
// the program returns erfc = 0 for arg > xmax.;
//other subprograms required (single precision version);
// abs, exp;
//other subprograms required (double precision version);
// dabs, dexp;
// author: w. j. cody;
// mathematics and computer science division;
// argonne national laboratory;
// argonne, il 60439;
// latest modification: january 8, 1985;

void wqm_calerf(double &arg,
                double &result,
                int &jint){
  
double four = 4.0e00;
double thresh = 0.46875e0, sqrpi = 5.6418958354775628695e-1;
double x,xmax = 9.269e0,xden,xnum,xsmall = 4.2e-16,y,ysq;

// Coefficients for approximation to derf in first interval
   Rcpp::NumericVector a = NumericVector::create(3.16112374387056560e00,
                                                 1.13864154151050156e02,
                                                 3.77485237685302021e02,
                                                 3.20937758913846947e03,
                                                 1.85777706184603153e-1);

   Rcpp::NumericVector b = NumericVector::create(2.36012909523441209e01,
                                                 2.44024637934444173e02,
                                                 1.28261652607737228e03,
                                                 2.84423683343917062e03);

// Coefficients for approximation to dxerc in second interval
   Rcpp::NumericVector c = NumericVector::create(5.64188496988670089e-1,
                                                 8.88314979438837594e0,
                                                 6.61191906371416295e01,
                                                 2.98635138197400131e02,
                                                 8.81952221241769090e02,
                                                 1.71204761263407058e03,
                                                 2.05107837782607147e03,
                                                 1.23033935479799725e03,
                                                 2.15311535474403846e-8);
   
   Rcpp::NumericVector d = NumericVector::create(1.57449261107098347e01,
                                                 1.17693950891312499e02,
                                                 5.37181101862009858e02,
                                                 1.62138957456669019e03,
                                                 3.29079923573345963e03,
                                                 4.36261909014324716e03,
                                                 3.43936767414372164e03,
                                                 1.23033935480374942e03);

// Coefficients for approximation to dxerc in third interval
   Rcpp::NumericVector p = NumericVector::create(3.05326634961232344e-1,
                                                 3.60344899949804439e-1,
                                                 1.25781726111229246e-1,
                                                 1.60837851487422766e-2,
                                                 6.58749161529837803e-4,
                                                 1.63153871373020978e-2);
   
   Rcpp::NumericVector q = NumericVector::create(2.56852019228982242e00,
                                                 1.87295284992346047e00,
                                                 5.27905102951428412e-1,
                                                 6.05183413124413191e-2,
                                                 2.33520497626869185e-3);
   

x = arg;
y = std::abs(x);
if(y > four) goto line200;
if(y > thresh) goto line100;

// Evaluate erf for abs(x) <= 0.46875
   ysq = zero;
   if(y > xsmall) ysq = y * y;
   xnum = a.at(4) * ysq;
   xden = ysq;
   
for(int i = 1; i <= 3; i++){
  
    xnum = (xnum + a.at(i - 1)) * ysq;
    xden = (xden + b.at(i - 1)) * ysq;
    
}

result = x * (xnum + a.at(3)) / (xden + b.at(3));

if(jint != 0) result = one - result;
return;

// Evaluate erfc for 0.46875 < abs(x) <= 4.0
line100: ysq = y * y;
         xnum = c.at(8) * y;
         xden = y;
         
for(int i = 1; i <= 7; i++){
  
    xnum = (xnum + c.at(i - 1)) * y;
    xden = (xden + d.at(i - 1)) * y;
    
}

result = std::exp(-ysq) * (xnum + c.at(7)) / (xden + d.at(7));
goto line300;

// Evaluate erfc for abs(x) > 4.0
line200: result = zero;
         if(y >= xmax) goto line300;
         
         ysq = one / (y * y);
         xnum = p.at(5) * ysq;
         xden = ysq;
         
for(int i = 1; i <= 4; i++){
  
    xnum = (xnum + p.at(i - 1)) * ysq;
    xden = (xden + q.at(i - 1)) * ysq;
    
}

result = ysq * (xnum + p.at(4)) / (xden + q.at(4));
result = (std::exp(-y * y) / y) * (sqrpi - result);

// Fix up for neg. arg., erf, etc.
line300: if(jint == 0) goto line350;
         if(x < zero) result = two - result;
         return;
            
line350: result = (half - result) + half;
         if(x < zero) result = -result;
         

return;

}
