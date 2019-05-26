#include <base/base.hpp>
#include <utility/dmachine.hpp>

//' Epsilon Algorithm
//'
//' @source  Robert Piessens, Applied Math. & Progr. Div. - K.U.Leuven
//'          Elise De Doncker, Applied Math & Progr. Div. - K.U.Leuven
//'
//' @description Determine the limit of a given sequence
//'              of approximations, by means of the epsilon
//'              algorithm of P. Wynn.  An estimate of the
//'              absolute error is also given. The condensed
//'              epsilon table is computed. Only those elements
//'              needed for the computation of the next diagonal
//'              are preserved.
//'
//'  @param n Epsilon table element number
//'  @param epstab Numerical vector of dimension 52
//'         containing the elements of the two lower
//'         diagonals of the triangular epsilon table.
//'  @param result Resulting approximation to the integral
//'  @param abserr Estimate of the absolute error computed
//'         from result and the 3 previous results
//'  @param res3la Vector of dimension 3 containing the
//'         last 3 results
//'  @param nres Number of calls to the routine
//'
//'  @details \code{epstab(n)} contains the new element in the
//'           first column of the epsilon table.
//'           \code{nres} should be zero at first call.
//'           The elements in \code{epstab} are numbered
//'           starting at the right-hand corner of the
//'           triangle.
//'
//'           Refer to dqagie,dqagoe,dqagpe,dqagse.
//'           Routines called - d1mach.
//'           Revision date May 18, 1983.
//'           Keywords: epsilon algorithm, convergence acceleration
//'  extrapolation;

void dqelg(int &n,
           Rcpp::NumericVector &epstab,
           double &result,
           double &abserr,
           Rcpp::NumericVector &res3la,
           int &nres){

double delta1,delta2,delta3;
double epmach,epsinf,error,err1,err2,err3,e0,e1,e1abs,e2,e3;
double oflow,res,ss,tol1,tol2,tol3;
int ib,ib2,ie,indx,k1,k2,k3,limexp,newelm,num;

// list of major variables;
//
// e0 - the 4 elements on which the computation of a new;
// e1 element in the epsilon table is based;
// e2;
// e3 e0;
// e3 e1 new;
// e2;
// newelm - number of elements to be computed in the new;
// diagonal;
// error - error = abs(e1-e0)+abs(e2-e1)+abs(new-e2);
// result - the element in the new diagonal with least value;
// of error;
// machine dependent constants;
// ---------------------------;
// epmach is the largest relative spacing.;
// oflow is the largest positive magnitude.;
// limexp is the maximum number of elements the epsilon;
// table can contain. if this number is reached, the upper;
// diagonal of the epsilon table is deleted.

epmach = D1mach(4);
oflow = D1mach(2);
nres = nres + 1;
abserr = oflow;
result = epstab.at(n - 1);
if(n < 2) goto line100;

limexp = 50;
epstab.at(n + 1) = epstab.at(n - 1);
newelm = (n - 1) / 2;
epstab.at(n - 1) = oflow;
num = n;
k1 = n;

for(int i = 1; i <= newelm; i++){

k2 = k1 - 1;
k3 = k1 - 2;
res = epstab.at(k1 + 1);
e0 = epstab.at(k3 - 1);
e1 = epstab.at(k2 - 1);
e2 = res;
e1abs = std::abs(e1);
delta2 = e2 - e1;
err2 = std::abs(delta2);
tol2 = std::max(std::abs(e2),e1abs) * epmach;
delta3 = e1 - e0;
err3 = std::abs(delta3);
tol3 = std::max(e1abs,std::abs(e0)) * epmach;

if((err2 > tol2) or (err3 > tol3)) goto line10;

// if e0, e1 and e2 are equal to within machine;
// accuracy, convergence is assumed.;
// result = e2;
// abserr = abs(e1-e0)+abs(e2-e1);
   result = res;
   abserr = err2 + err3;

// jump out of do-loop;
   goto line100;

line10: e3 = epstab.at(k1 - 1);
        epstab.at(k1 - 1) = e1;
        delta1 = e1 - e3;
        err1 = std::abs(delta1);
        tol1 = std::max(e1abs,std::abs(e3)) * epmach;

// If two elements are very close to each other, omit
// a part of the table by adjusting the value of n;
   if((err1 <= tol1) or (err2 <= tol2) or (err3 <= tol3)) {
   
     goto line20;
   
   }
   
   ss = 0.1e+01 / delta1 + 0.1e+01 / delta2 - 0.1e+01 / delta3;
   epsinf = std::abs(ss * e1);
   
// Test to detect irregular behaviour in the table, and eventually omit 
// a part of the table adjusting the value of n.
   if(epsinf > 0.1e-03) goto line30;

line20: n = i + i - 1;

// jump out of do-loop
   goto line50;

// Compute a new element and eventually adjust the value of result.
   line30: res = e1 + 0.1e+01 / ss;
           epstab.at(k1 - 1) = res;
           k1 = k1 - 2;
           error = err2 + std::abs(res - e2) + err3;

           if(error > abserr) continue;

           abserr = error;
           result = res;

}

// Shift the table
line50: if(n == limexp) n = 2 * std::floor(limexp / 2) - 1;
        ib = 1;
        if((num % 2) == 0) ib = 2;

        ie = newelm + 1;

for(int i = 1; i <= ie; i++){

    ib2 = ib + 2;
    epstab.at(ib - 1) = epstab.at(ib2 - 1);
    ib = ib2;

}

if(num != n) {

   indx = num - n + 1;
   
   for(int i = 1; i <= n; i++){
   
       epstab.at(i - 1) = epstab.at(indx - 1);
       indx = indx + 1;
   
   }
}

if(nres < 4) {

   res3la.at(nres - 1) = result;
   abserr = oflow;
   goto line100;

}

// Compute error estimate
abserr = std::abs(result - res3la.at(2)) + std::abs(result - res3la.at(1)) + std::abs(result - res3la.at(0));

res3la.at(0) = res3la.at(1);
res3la.at(1) = res3la.at(2);
res3la.at(2) = result;


line100: abserr = std::max(abserr, 0.5e+01 * epmach * std::abs(result));

return;

}
