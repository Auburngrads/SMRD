#include <base/base.hpp>
#include <mlsim2/dlbeta.hpp>
#include <utility/dmachine.hpp>

//' august 1980 edition.  w. fullerton, c3, los alamos scientific lab.
//' based on bosten and battiste, remark on algorithm 179, comm. acm,
//' v 17, p 153, (1974).
//' 
//' input arguments --
//' x      upper limit of integration.  x must be in (0,1) inclusive.
//' p      first beta distribution parameter.  p must be gt 0.0.
//' q      second beta distribution parameter.  q must be gt 0.0.
//' betai  the incomplete beta function ratio is the probability that a
//'       random variable from a beta distribution having parameters
//'       p and q will be less than or equal to x.
double dbetai(double x, 
              double pin, 
              double qin){
  
double c,finsum,p,d_betai = 0.0e00;
double ps, p1, q, term, xb, xi, y;
double eps = 0.0e00, alneps = 0.0e00;
double alnsml = 0.0e00, sml = 0.0e00;
int n, ib;

if (eps == 0.0e0) {
  
    eps = D1mach(3);
    alneps = std::log(eps);
    sml = D1mach(1);
    alnsml = std::log(sml);

}

// xxx      if (x.lt.0.d0 .or. x.gt.1.d0) call seterr (
// xxx      1  35hdbetai  x is not in the range (0,1), 35, 1, 2)
// xxx       if (pin.le.0.d0 .or. qin.le.0.d0) call seterr (
// xxx      1  29hdbetai  a and/or b is le zero, 29, 2, 2)

y = x;
p = pin;
q = qin;
if ((q <= p) and (x < 0.8e00)) goto line20;
if (x < 0.2e00) goto line20;
y = 1.0e00 - y;
p = qin;
q = pin;

line20:   if (((p + q) * y / (p + 1.0e00)) < eps) goto line80;

// evaluate the infinite sum first.  term will equal
// y**p/beta(ps,p) * (1.-ps)-sub-i * y**i / fac(i) .
   ps = q - (double)(std::floor(q));
   if (ps == 0.0e00) ps = 1.0e00;
   xb = p * std::log(y) - dlbeta(ps,p) - std::log(p);
   d_betai = 0.0e00;
   if(xb < alnsml) goto line40;
   
   d_betai = std::exp(xb);
   term = d_betai * p;
   if(ps == 1.0e00) goto line40;
   n = std::max((alneps / std::log(y)), 4.0);
   
   for(int i = 1; i <= n; i++){
     
       xi = i
       term = term * (xi - ps) * y / xi;
       d_betai = d_betai + term / (p + xi);
     
   }

// Now evaluate the finite sum, maybe.
   line40: if (q <= 1.0e00) goto line70;

   xb = p * std::log(y) + q * std::log(1.0e00 - y) - dlbeta(p,q) - std::log(q);
   ib = std::max((xb / alnsml), 0.0);
   term = std::exp(xb - double(ib) * alnsml);
   c = 1.0e00 / (1.0e00 - y);
   p1 = q * c / (p + q - 1.0e00);
   
   finsum = 0.0e00;
   n = q;
   if (q == double(n)) n = n - 1;
   
   for(int i = 1; i <= n; i++){
     
       if ((p1 <= 1.0e00) and ((term / eps) <= finsum)) goto line60;
       xi = i;
       term = (q - xi + 1.0e00) * c * term / (p + q - xi);

       if(term > 1.0e00) ib = ib - 1;
       if(term > 1.0e00) term = term * sml;
       if (ib == 0) finsum = finsum + term;
          
   }

line60: d_betai = d_betai + finsum;
line70: if((y != x) or (p != pin)) d_betai = 1.0e00 - d_betai;
        d_betai = std::max(std::min(d_betai, 1.0e00), 0.0e00);
        return d_betai;

line80: d_betai = 0.0e00;
        xb = p * std::log(std::max(y,sml)) - std::log(p) - dlbeta(p,q);
        if((xb > alnsml) and (y != 0.0e00)) d_betai = std::exp(xb);
        if((y != x) or (p != pin)) d_betai = 1.0e00 - d_betai;

return d_betai;

}

#include <base/base.hpp>
#include <postkp/dlgama.hpp>

double dlbeta(double p,
              double q){
  
double dl_beta = dlgama(p) + dlgama(q) - dlgama(p + q);
      
return dl_beta;

}
