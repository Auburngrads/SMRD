#include <base/base.hpp>

//'  function to compute the complimentary error function
double wqm_dxerc(double x){

  return two * R::pnorm(x * std::sqrt(two), 0, 1, false, false);
  
}

//' Check for overflow/underflow for calls to dexp.
//' Ignore underflow and return 0
//' If overflow, return a big number.
double dexpc(double x){

if(x >  700.0e00) return bigexp;

if(x < -700.0e00) return smexp;

return std::exp(x);

}

//' check for negative arg of log. 
//' if ok, just return the log, 
//' otherwise return -big.

double dlogc(double x) {
  
if(x <= zero) return -big20;

   return std::log(x);
   
}

double dsign(double x, double c) {

  return std::fabs(x) * R::sign(c);
  
}

double dsqrtc(double x) { 
  
  return std::sqrt(std::fabs(x));
   
}

//' @description Sort routine for numeric vectors

Rcpp::IntegerVector merge_sortd(Rcpp::NumericVector x,
                                Rcpp::IntegerVector iorder,
                                bool second) {

  int n = x.length();
  //double MIN = x.at(which_min(x));
  double MAX = x.at(which_max(x));
  Rcpp::NumericVector MAXV(n, MAX + 1);

  int tmin = 0;

  Rcpp::IntegerMatrix tempmat(n,n), tempmat2(n,n);
  Rcpp::IntegerVector tempvec(n)  , tempvec2(n);
  Rcpp::IntegerVector key(n);
  Rcpp::NumericVector rowi;

  if(iorder.at(n - 1) - iorder.at(0) < 0.0001) {

     for(int i = 0; i < n; i++){

         iorder.at(i) = i;

     }
  }

    for(int j = 0; j < n; j++){

        tempmat.row(j) = clone(x);

    }

    for(int i = 0; i < n; i++) {

      rowi = tempmat.row(i);
      tmin = which_min(rowi);
      tempvec.at(tmin) = i;
      tempmat.column(tmin) = MAXV;

    }

    // second round

    for(int j = 0; j < n; j++){

        tempmat2.row(j) = clone(tempvec);

     }


  for(int i = 0; i < n; i++) {

      tempvec2 = tempmat2.row(i);
      tmin = which_min(tempvec2);
      key.at(i) = iorder.at(tmin);
      tempmat2.column(tmin) = MAXV;

    }

  if(second) return key;
  
  return tempvec;

}


//' @description Sort routine for integer vectors

Rcpp::IntegerVector merge_sorti(Rcpp::IntegerVector x,
                                Rcpp::IntegerVector iorder,
                                bool second) {

  int n = x.length();
  //double MIN = x.at(which_min(x));
  double MAX = x.at(which_max(x));
  Rcpp::IntegerVector MAXV(n, MAX + 1);

  int tmin = 0;

  Rcpp::IntegerMatrix tempmat(n,n), tempmat2(n,n);
  Rcpp::IntegerVector tempvec(n)  , tempvec2(n);
  Rcpp::IntegerVector key(n);
  Rcpp::IntegerVector rowi;

  if(iorder.at(n - 1) - iorder.at(0) < 0.0001) {

     for(int i = 0; i < n; i++){

         iorder.at(i) = i;

     }

  }

    for(int j = 0; j < n; j++){

        tempmat.row(j) = clone(x);

     }

    for(int i = 0; i < n; i++) {

        rowi = tempmat.row(i);
        tmin = which_min(rowi);
        tempvec.at(tmin) = i;
        tempmat.column(tmin) = MAXV;

    }


    // second round

    for(int j = 0; j < n; j++){

        tempmat2.row(j) = clone(tempvec);

     }


  for(int i = 0; i < n; i++) {

      tempvec2 = tempmat2.row(i);
      tmin = which_min(tempvec2);
      key.at(i) = iorder.at(tmin);
      tempmat2.column(tmin) = MAXV;

    }

  if(second) return key;
  
  return tempvec;

}

/***R
# set.seed(42)
# units <- sample(1:5, size = 20, replace = T)
# times <- sample(rexp(5, 0.02), size = 20, replace = T)
iorder <- integer(13)
#
# iorder2 <- merge_sortd(times, iorder)
#
# merge_sorti(units, iorder2)$key

v = c(77, 11, 27, 69, 49, 79, 72, 52, 56, 35, 56, 28, 33)

iorder2 = merge_sorti(1:13, iorder, FALSE)
merge_sortd(v, iorder2, TRUE) + 1

*/

//' Copy over an integer vector 
void wqm_copyi(Rcpp::IntegerVector &iin, 
               Rcpp::IntegerVector &iout, 
               int n){
  
if(n > 0) {
  
   for(int i = 0; i < n; i++){
     
       iout.at(i) = iin.at(i);
     
   }
}
      
   return;
  
}


void wqm_copyd(Rcpp::NumericVector &iin, 
               Rcpp::NumericVector &iout, 
               int n){

if(n > 0) {
  
   for(int i = 0; i < n; i++){
     
      iout.at(i) = iin.at(i);
     
   }
}
      
   return;
  
}
#include <base/base.hpp>

//' copy real matrix to a real matrix

void copyrr(Rcpp::NumericMatrix &x,
            Rcpp::NumericMatrix &xnew,
            int &ncol,
            int &nrow,
            int &nrownw){

// Adjusts for the different number of rows in x and xnew
   for(int j = 1; j <= ncol; j++){
     
       for(int i = 1; i <= nrownw; i++){
         
           xnew.at(i - 1,j - 1) = x.at(i - 1,j - 1);
         
       }
   }
   
return;
  
}

#include <utility/wqm_dxerc.hpp>

double g(double x) {

  double numer = wqm_dxerc(x / std::sqrt(2.0e0));

  return  numer / 2.0e0;

}

double gp(double x){

  return std::exp(-x * x / 2.0e0) / dr;

}

//'  Odeh's inverse normal with one iteration
//'  of Newton for polish.

double wqm_pinv(double p){

double a0 = 0.505300845045718e-1;
double a1 = 0.6207981783784e0;
double a2 = 1.0e0;
double a3 = 0.255758400033573e0;
double a4 = 0.120012384541901e-1;
double a5 = 0.274100549062517e-4;
double a6 = -0.109846617732390e-6;

double b0 = 0.129635913467631e-1;
double b1 = 0.252771250307626e0;
double b2 = 0.713433125153414e0;
double b3 = 0.472899800222222e0;
double b4 = 0.722351199288358e-1;
double b5 = 0.221326694412374e-2;

double wqmpinv = 0.0e00;
double y, x, xu, xb;

if(p != 0.5e0) {

   y  = std::sqrt(-2.0e0 * std::log(p));

   xu = ((((((y * a6 + a5) * y + a4) * y + a3) * y + a2) * y + a1) * y + a0);
   xb = (((((y * b5 + b4) * y + b3) * y + b2) * y + b1) * y + b0);
   x  = y - xu / xb;

   wqmpinv = x - (p - g(x)) / gp(x);

}
      return wqmpinv;
}

#include <utility/wqm_pinv.hpp>

//' location-scale distribution quantiles

double wqm_quant(double p,
                 int kdist){

double smallp = 1.0e-25;
double wqmquant = 0.0e00;
double d = p;

if(p < smallp) d = smallp;
if(p >= one)   d = one - 1.0e-15;

//  smallest extreme value distribution
if((kdist == 1) or (kdist == 2)) {

    wqmquant = std::log(-1 * std::log(one - d));

}
// normal distribution
if((kdist == 3) or (kdist == 4)) {

    if(d < half) {

       wqmquant = -1 * wqm_pinv(d);

    } else {

       wqmquant = wqm_pinv(one - d);

    }

}
//  logistic distribution
if((kdist == 5) or (kdist == 6)) {

    wqmquant = -1 * std::log(1 / d - 1);

}
//  largest extreme value distribution
if((kdist == 7) or (kdist == 8)) {

    wqmquant = -1 * std::log(-1 * std::log(d));

}
      return wqmquant;
}

#include <base/base.hpp>

// Exchange k1 and k2;
void wqm_exchan(int &k1, 
                int &k2){
  
  int k = k1;
      k1 = k2;
      k2 = k;
  
return;

}

//'  Integer function to compare two elements of a vector
//'  
//'  returns:
//'            1     if(yd(k1).gt.yd(k2))
//'            0     if(yd(k1).eq.yd(k2))
//'           -1     if(yd(k1).lt.yd(k2))

int icompd(int k1,
           int k2,
           Rcpp::NumericVector yd){

      if(yd.at(k1 - 1) - yd.at(k2 - 1) < 0) return -1;
      if(yd.at(k1 - 1) - yd.at(k2 - 1) > 0) return  1;

      return 0;
}

//' wqm_sortxx will perform a quick sort and will return key(n),;
//' the ordering vector of indices for y(n). y is not changed.;
//' wqm_icomp is a comparison routine according to variable type and;
//' must be declared as external in the calling routine;
//' if n is negative,key is assumed to be full when wqm_sortxx is called;
//' this is useful for secondary sort on a second variable;
//' 
//' the function name wqm_icomp is important. When it was icomp only;
//' this sort routine did not work correctly;

void wqm_sortd(Rcpp::NumericVector &y,
               int &np,
               Rcpp::IntegerVector &key){

Rcpp::IntegerVector il(20),iu(20);
int n = std::abs(np);
il.at(0) = 1;
iu.at(0) = n;
int ip, j, i, kpivot;

// Initilize the key vector
   if(np < 0) goto line23;

   for(int i = 1; i <= n; i++){
   
       key.at(i - 1) = i;
   
   }
   
   line23: ip = 1;

// While(15) (ip>0)

line10: if(ip <= 0) goto line15;

if(il.at(ip - 1) < iu.at(ip - 1)) goto line101;

// Subset has only one element so pop stack
   ip = ip - 1;
   goto line10;

// Else partition y(key(il(ip))) to y(key(iu(ip)))
   line101: i = il.at(ip - 1) - 1;
   j = iu.at(ip - 1);

// Choose last element in current subset as the pivot element
   kpivot = key.at(j - 1);

// While(75) (i<j)
   line70: if(i >= j) goto line75;
   
// for(i=i+1;icomp(key(i),kpivot,y)<0;i=i+1) (25)
   i = i + 1;

// while(25) icomp(key(i),kpivot,y) < 0
   line20: if(icompd(key.at(i - 1),kpivot,y) >= 0) goto line25;
   i = i + 1;
   goto line20;
   
// for(j = j - 1; j > i; j = j - 1) (35)
   line25: j = j - 1;

// while(35) (j>i)
   line30: if(j <= i) goto line35;
   if(icompd(key.at(j - 1),kpivot,y) <= 0) goto line35;
   j = j - 1;
   goto line30;
   
   line35: if(i <= j) wqm_exchan(key.at(i - 1),key.at(j - 1));
   goto line70;
   
   line75: j = iu.at(ip - 1);
   wqm_exchan(key.at(i - 1),key.at(j - 1));

// Fix stack so that shorter is done first
   if(i - il.at(ip - 1) >= iu.at(ip - 1) - i) goto line102;
   il.at(ip + 1 - 1) = il.at(ip - 1);
   iu.at(ip + 1 - 1) = i - 1;
   il.at(ip - 1) = i + 1;
   goto line103;
   
// else
   line102: il.at(ip + 1 - 1) = i + 1;
   iu.at(ip + 1 - 1) = iu.at(ip - 1);
   iu.at(ip - 1) = i - 1;
   
// push onto stack
   line103: ip = ip + 1; 
   goto line10;
   line15: return;

}
#include <base/base.hpp>

// Check double precision value
void dcheck(double &xc,
            double xl,
            double xu,
            double defl,
            double defu,
            int &ier,
            int ier_num){

if((xc >= xl) and (xc <= xu)) return;

   if(xc < xl) {
     
     xc = defl;
     ier = ier_num;
     Rcpp::warning("dcheck quantity below lower limit");
     if(ier_num != 0) Rcpp::stop("ier = ", ier_num);
     
   }
   
   if(xc > xu) {
     
     xc = defu;
     ier = ier_num;
     Rcpp::warning("dcheck quantity above upper limit");
     if(ier_num != 0) Rcpp::stop("ier = ", ier_num);
     
   }

 return;

}

#include <base/base.hpp>

//' check integer value
void icheck(int &ic,
            int il,
            int iu,
            int idefl,
            int idefu,
            int &ier,
            int ier_num){

if((ic >= il) and (ic <= iu)) return;

   if(ic < il) {
     
     ic = idefl;
     ier = ier_num;
     Rcpp::warning("icheck quantity below lower limit");
     if(ier_num != 0) Rcpp::stop("ier = ", ier_num);
     
   }
   
   if(ic > iu) {
     
     ic = idefu;
     ier = ier_num;
     Rcpp::warning("icheck quantity above upper limit");
     if(ier_num != 0) Rcpp::stop("ier = ", ier_num);
     
   }

 return;

}

#include <base/base.hpp>

//' Find the maximum of a vector of length n
void vmaxr(Rcpp::NumericVector &vec,
           int &n,
           double &vmax){
  
      vmax = -1.0e10;
  
      if(n > 0){
        
         for(int i = 0; i < n; i++){
           
             vmax = std::max(vmax,vec.at(i));
           
         }

      }
      
      return;
        
}
#include <base/base.hpp>

//' Find the minimum of a vector of length n
void vminr(Rcpp::NumericVector &vec,
           int &n, 
           double &vmin){
  
      vmin = 1.0e10;
  
      if(n > 0){
        
         for(int j = 0; j < n; j++){
           
             vmin = std::min(vmin,vec.at(j));
           
         }
      
      }
      
      return;
      
}

#include <base/base.hpp>

//' Fill a double precision vector x with a constant
void wqm_filld(int con,
               Rcpp::NumericVector &x,
               int start,
               int n){

if(n > 0){
   
   for(int i = start; i < (start + n); i++){
      
       x.at(i - 1) = con;
      
   }
         
}
      return;
}

// Fill an integer vector with a constant
void filli(int con,
           Rcpp::IntegerVector &x,
           int start,
           int n){

      if(n <= 0) return;
      
      for(int i = start; i < (start + n); i++){
        
          x.at(i - 1) = con;
        
      }
      
      return;
      
}
//' Find the maximum of a vector of length n

double vmax(Rcpp::NumericVector vec,
            int n){
  
double v_max = -1.0e10;
  
if(n <= 0) return v_max;

for(int i = 1; i <= n; i++){
  
    v_max = std::max(v_max,vec.at(i - 1));
  
}

return v_max;
      
}
// Find the minimum of a vector of length n
double vmin(Rcpp::NumericVector vec,
            int n){
  
double v_min = 1.0e10;
  
if(n <= 0) return v_min;

for(int i = 1; i <= n; i++){
  
    v_min = std::min(v_min,vec.at(i - 1));
  
}

return v_min;

}
