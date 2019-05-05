#include <base/base.hpp>
#include <wqmmlesss/pythag.hpp>
#include <utility/dsign.hpp>

//' DON'T MESS WITH THIS - IT WORKS!!! (09 MARCH 2019)
//' 
//' Modified to take single precision input and to 
//' always work on an x matrix and to always return 
//' the components of the decomp
//' received from netlib eispack and modified by wqm for wqm_mlfit;
// this subroutine is a translation of the algol procedure svd,;
// num. math. 14, 403-420(1970) by golub and reinsch.;
// handbook for auto. comp., vol ii-linear algebra, 134-151(1971).;
// this subroutine determines the singular value decomposition;
// t;
// a=usv of a real m by n rectangular matrix. householder;
// bidiagonalization and a variant of the qr algorithm are used.;
// on input;
// nrow is the number of rows of a (and u).;
// nter is the number of columns of a (and u) and the order of v.;
// nparm dimension of vectors and matrices (must be >= nter);
// xold(nrow,nter) contains the rectangular input matrix to be decomposed.;
// lfix(nparm) vector with element = true if the coeff corresp;
// to row is fixed (known) and = false otherwise;
// on output;
// w.at(nparm) contains the n (non-negative) singular values of a (the;
// diagonal elements of s). they are unordered. if an;
// error exit is made, the singular values should be correct;
// for indices ierr+1,ierr+2,...,n.;
// u.at(nrow,nparm) contains the matrix u (orthogonal column vectors) of the;
// decomposition;
// if an error exit is made, the columns of u corresponding;
// to indices of correct singular values should be correct.;
// v.at(nparm) contains the matrix v (orthogonal) of the decomposition;
// if an error;
// exit is made, the columns of v corresponding to indices of;
// correct singular values should be correct.;
// ierr is set to;
// zero for normal return,;
// k if the k-th singular value has not been;
// determined after 30 iterations.;
// rv1.at(nter) is a temporary storage array.;
// calls pythag for dsqrt(a*a + b*b) .;
// questions and comments should be directed to burton s. garbow,;
// mathematics and computer science div, argonne national laboratory;
// this version dated august 1983.;

void wqm_svdstr(int &nrow,
                int &nter,
                int &nparm,
                Rcpp::LogicalVector &lfix,
                Rcpp::NumericMatrix &xold,
                Rcpp::NumericVector &w, // diag
                Rcpp::NumericMatrix &u, // xnew
                Rcpp::NumericMatrix &v, // tmat
                int &ierr,
                Rcpp::NumericVector &rv1){

int K,n,l = 0,its,mn;
double c,f,h,s,y,z,tst1,tst2;

bool matu = true;
bool matv = true;
int m = nrow;
int nrter = 0;
ierr = 0;

for(int j = 1; j <= nter; j++){
  
    if(lfix.at(j - 1)) continue;
    nrter = nrter + 1;
    
    for(int i = 1; i <= nrow; i++){
      
        u.at(i - 1,nrter - 1) = xold.at(i - 1,j - 1);
      
    }
}

n = nrter;
// .......... householder reduction to bidiagonal form ..........;
double g = 0.0e0;
double scale = 0.0e0;
double x = 0.0e0;

for(int i = 1; i <= n; i++){

    l = i + 1;
    rv1.at(i - 1) = scale * g;
    g = 0.0e0;
    s = 0.0e0;
    scale = 0.0e0;
    if(i > m) goto line210;

    for(int k = i; k <= m; k++){

        scale = scale + std::abs(u.at(k - 1,i - 1));

    }

    if(scale == 0.0e0) goto line210;

    for(int k = i; k <= m; k++){

        u.at(k - 1,i - 1) = u.at(k - 1,i - 1) / scale;
        s = s + std::pow(u.at(k - 1,i - 1),2);

    }

    f = u.at(i - 1,i - 1);
    g = -dsign(std::sqrt(s),f);
    h = f * g - s;
    u.at(i - 1,i - 1) = f - g;

    if(i != n) {

       for(int j = l; j <= n; j++){

           s = 0.0e0;

           for(int k = i; k <= m; k++){

               s = s + u.at(k - 1,i - 1) * u.at(k - 1,j - 1);

           }

           f = s / h;

           for(int k = i; k <= m; k++){

               u.at(k - 1,j - 1) = u.at(k - 1,j - 1) + f * u.at(k - 1,i - 1);

           }
       }
    }

    for(int k = i; k <= m; k++){

        u.at(k - 1,i - 1) = scale * u.at(k - 1,i - 1);

    }

    line210: w.at(i - 1) = scale * g;
    g = 0.0e0;
    s = 0.0e0;
    scale = 0.0e0;

    if((i > m) or (i == n)) goto line290;

    for(int k = l; k <= n; k++){

        scale = scale + std::abs(u.at(i - 1,k - 1));

    }

    if(scale == 0.0e0) goto line290;

    for(int k = l; k <= n; k++){

        u.at(i - 1,k - 1) = u.at(i - 1,k - 1) / scale;
        s = s + std::pow(u.at(i - 1,k - 1),2);

    }

    f = u.at(i - 1,l - 1);
    g = -dsign(std::sqrt(s),f);

    h = f * g - s;
    u.at(i - 1,l - 1) = f - g;

    for(int k = l; k <= n; k++){

        rv1.at(k - 1) = u.at(i - 1,k - 1) / h;

    }

    if(i == m) goto line270;

    for(int j = l; j <= m; j++){

        s = 0.0e0;

        for(int k = l; k <= n; k++){

            s = s + u.at(j - 1,k - 1) * u.at(i - 1,k - 1);

        }

        for(int k = l; k <= n; k++){

            u.at(j - 1,k - 1) = u.at(j - 1,k - 1) + s * rv1.at(k - 1);

        }

    }

    line270: for(int k = l; k <= n; k++){

                 u.at(i - 1,k - 1) = scale * u.at(i - 1,k - 1);

             }

    line290: x = std::max(x,std::abs(w.at(i - 1)) + std::abs(rv1.at(i - 1)));

}

// .......... accumulation of right-hand transformations ..........;
if(!matv) goto line410;
l = 0;
//.......... for i=n step -1 until 1 do -- ..........;
for(int ii = 1; ii <= n; ii++){

    int i = (n + 1) - ii;
    if(i != n) {
      
       if(g != 0.0e0) {
   
          for(int j = l; j <= n; j++){
      
              // double division avoids possible underflow
              v.at(j - 1,i - 1) = (u.at(i - 1,j - 1) / u.at(i - 1,l - 1)) / g;
      
          }
      
          for(int j = l; j <= n; j++){
      
              s = 0.0e0;
      
              for(int k = l; k <= n; k++){
      
                  s = s + u.at(i - 1,k - 1) * v.at(k - 1,j - 1);
      
              }
      
              for(int k = l; k <= n; k++){
      
                  v.at(k - 1,j - 1) = v.at(k - 1,j - 1) + s * v.at(k - 1,i - 1);
      
              }
          }
          }
       
       for(int j = l; j <= n; j++){
         
           v.at(i - 1,j - 1) = 0.0e0;
           v.at(j - 1,i - 1) = 0.0e0;
   
         }
   
       }
    
    v.at(i - 1,i - 1) = 1.0e0;
    g = rv1.at(i - 1);
    l = i;

}

// accumulation of left-hand transformations
line410: if(!matu) goto line510;

// for i=min(m,n) step -1 until 1 do --
mn = n;
if(m < n) mn = m;

for(int ii = 1; ii <= mn; ii++){

    int i = mn + 1 - ii;
    l = i + 1;
    g = w.at(i - 1);

    if(i != n) {

       for(int j = l; j <= n; j++){

           u.at(i - 1,j - 1) = 0.0e0;

       }

    }

    if(g == 0.0e0) goto line475;
    if(i == mn) goto line460;

    for(int j = l; j <= n; j++){

        s = 0.0e0;
        for(int k = l; k <= m; k++){

            s = s + u.at(k - 1,i - 1) * u.at(k - 1,j - 1);

        }
        //double division avoids possible underflow
        f = (s / u.at(i - 1,i - 1)) / g;

        for(int k = i; k <= m; k++){

            u.at(k - 1,j - 1) = u.at(k - 1,j - 1) + f * u.at(k - 1,i - 1);

        }

    }
    line460: for(int j = i; j <= m; j++){

                 u.at(j - 1,i - 1) = u.at(j - 1,i - 1) / g;

             }

    goto line490;

    line475: for(int j = i; j <= m; j++){

                 u.at(j - 1,i - 1) = 0.0e0;

             }

    line490: u.at(i - 1,i - 1) = u.at(i - 1,i - 1) + 1.0e0;
    
}

// .......... diagonalization of the bidiagonal form ..........;
line510: tst1 = x;
//
// .......... for k=n step -1 until 1 do -- ..........;
// where the issues start
for(int kk = 1; kk <= n; kk++){

    int k1 = n - kk;
    int k = k1 + 1;
    its = 0;
    // test for splitting
    // for l=k step -1 until 1 do -- ..........;
    int l1 = 0, l = 0;
    line520: for(int ll = 1; ll <= k; ll++){

                 l1 = k - ll;
                 l = l1 + 1;
                 tst2 = tst1 + std::abs(rv1.at(l - 1));
                 if(tst2 == tst1) goto line565;
                 // rv1.at(1) is always zero, so there is
                 // no exit through the bottom of the loop
                 tst2 = tst1 + std::abs(w.at(l1 - 1));
                 if(tst2 == tst1) goto line540;

             }

    // cancellation of rv1.at(l) if l greater than 1
    line540: c = 0.0e0;
    s = 1.0e0;

    for(int i = l; i <= k; i++){

        f = s * rv1.at(i - 1);
        rv1.at(i - 1) = c * rv1.at(i - 1);
        tst2 = tst1 + std::abs(f);
        if(tst2 == tst1) goto line565;
        g = w.at(i - 1);
        h = pythag(f,g);
        w.at(i - 1) = h;
        c = g / h;
        s = -f / h;

        if(matu) {

           for(int j = 1; j <= m; j++){

               y = u.at(j - 1,l1 - 1);
               z = u.at(j - 1,i - 1);
               u.at(j - 1,l1 - 1) = y * c + z * s;
               u.at(j - 1,i - 1) = -y * s + z * c;

           }
        }
    }

    // .......... test for convergence ..........;
    line565: z = w.at(k - 1);
    if(l == k) goto line650;
    // .......... shift from bottom 2 by 2 minor ..........;
    if(its == 30) { K = k; goto line1000; }
    its = its + 1;
    x = w.at(l - 1);
    y = w.at(k1 - 1);
    g = rv1.at(k1 - 1);
    h = rv1.at(k - 1);
    f = 0.5e0 * (((g + z) / h) * ((g - z) / y) + y / h - h / y);
    g = pythag(f,1.0e0);
    f = x - (z / x) * z + (h / x) * (y / (f + dsign(g,f)) - h);
    // .......... next qr transformation ..........;
    c = 1.0e0;
    s = 1.0e0;

    for(int i1 = l; i1 <= k1; i1++){

        int i = i1 + 1;
        g = rv1.at(i - 1);
        y = w.at(i - 1);
        h = s * g;
        g = c * g;
        z = pythag(f,h);
        rv1.at(i1 - 1) = z;
        c = f / z;
        s = h / z;
        f = x * c + g * s;
        g = -x * s + g * c;
        h = y * s;
        y = y * c;

        if(matv) {

           for(int j = 1; j <= n; j++){

               x = v.at(j - 1,i1 - 1);
               z = v.at(j - 1,i - 1);
               v.at(j - 1,i1 - 1) = x * c + z * s;
               v.at(j - 1,i - 1) = -x * s + z * c;

           }

        }

        z = pythag(f,h);
        w.at(i1 - 1) = z;

        // rotation can be arbitrary if z is zero
        if(z != 0.0e0) {

           c = f / z;
           s = h / z;

        }

        f = c * g + s * y;
        x = -s * g + c * y;

        if(matu) {

           for(int j = 1; j <= m; j++){

               y = u.at(j - 1,i1 - 1);
               z = u.at(j - 1,i - 1);
               u.at(j - 1,i1 - 1) = y * c + z * s;
               u.at(j - 1,i - 1) = -y * s + z * c;

           }
        }
    }

    rv1.at(l - 1) = 0.0e0;
    rv1.at(k - 1) = f;
    w.at(k - 1) = x;

    goto line520;
    // .......... convergence ..........;
    line650: if(z >= 0.0e0) continue;
    // .......... w.at(k) is made non-negative ..........;
    w.at(k - 1) = -z;
    if(!matv) continue;

    for(int j = 1; j <= n; j++){

        v.at(j - 1,k - 1) = -v.at(j - 1,k - 1);

    }

}

goto line1001;
// set error - no convergence to a singular
// value after 30 iterations
line1000: ierr = K;


line1001: return ;

}