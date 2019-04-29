#include <base/base.hpp>
#include <wqmmlesss/wqm_svdstr.hpp>
#include <utility/wqm_copyd.hpp>

//' Find the svd of columns of xold corresponding to unknown
//' coefficients in the linear model and put in xnew.
//' 
//' Also, return all of the components in the decomposition.
//' 
//' Expand everything to account for columns corresponding to
//' sigma and any known (i.e. fixed) coefficients in the model.

void wqm_svdfix(Rcpp::LogicalVector &lfix,
                int &nrow,
                int &nter,
                int &nparm,
                Rcpp::NumericMatrix &xold,
                Rcpp::NumericMatrix &xnew,
                Rcpp::NumericVector &diag,
                Rcpp::NumericMatrix &tmat,
                double &dgmin,
                int &ierr,
                Rcpp::NumericMatrix &vcvg,
                Rcpp::NumericVector &rv1,
                bool &lsvd){

dgmin = 1.0e20;
int jnow, inow;

if(debug::kprint >= 2) {
  
   Rcpp::Rcout << "\nWQM_SVDFIX**1**\n" << std::endl;
   Rcpp::Rcout << "xold = \n" << xold     << std::endl;
     
}

  
if(!lsvd) { goto line40; }

// Find the svd of xold
   wqm_svdstr(nrow,nter,nparm,lfix,xold,
              diag,xnew,tmat,ierr,rv1);

if(debug::kprint >= 3) {
  
   Rcpp::Rcout << "\nWQM_SVDFIX**2**\n" << std::endl; 
   Rcpp::Rcout << "xnew without fixed columns = \n" << xnew << std::endl;
  
}

// Expand xnew to contain columns corresponding to fixed parmameters

for(int i = 0; i < nrow; i++){

    // #save the new row;

    for(int j = 0; j < nter; j++){

        rv1.at(j) = xnew.at(i,j);

    }

    jnow = 0;

    // #fill in the row with the new or fixed value;

    for(int j = 0; j < nter; j++){

        if(!lfix.at(j)) {

           xnew.at(i,j) = rv1.at(jnow);
           jnow = jnow + 1;
           continue;

        }

         xnew.at(i,j) = xold.at(i,j);

    }
}

if(debug::kprint >= 3) {
  
   Rcpp::Rcout << "\nWQM_SVDFIX**3**\n" << std::endl; 
   Rcpp::Rcout << "diag without fixed columns = " << diag << std::endl;
  
}


// Expand the diag matrix to contian 1's where things are fixed;
rv1 = clone(diag);

// rv1  = Rcpp::as<NumericVector>(Rcpp::as<List>(COPYD)["iout"]);
// diag = Rcpp::as<NumericVector>(Rcpp::as<List>(COPYD)["iin"]);
jnow = 0;

for(int i = 0; i < nparm; i++){

    if(!((lfix.at(i)) or (i == (nparm - 1)))) {

       diag.at(i) = rv1.at(jnow);
       jnow = jnow + 1;
       dgmin = std::min(std::fabs(dgmin),diag.at(i));
       continue;

    }

    diag.at(i) = one;

}

if(debug::kprint >= 3) {
  
   Rcpp::Rcout << "\nWQM_SVDFIX**4**\n" << std::endl; 
   Rcpp::Rcout << "tmat without fixed columns = \n" << tmat << std::endl;
  
}


// Expand the tmat for fixed parameters and sigma
// Save the original tmat;
   vcvg = clone(tmat);
   inow = 0;

for(int i = 0; i < nparm; i++){

    if((lfix.at(i)) or (i == (nparm - 1))) goto line65;

// This row is not fixed; do one-by-one

jnow = 0;

for(int j = 0; j < nparm; j++){

    if(!(lfix.at(j) or j == (nparm - 1))) {

       tmat.at(i,j) = vcvg.at(inow,jnow);
       jnow = jnow + 1;
       continue;

    }

    tmat.at(i,j) = zero;

}

inow = inow + 1;
continue;

// Fixed row--use 0 0 0 ... 1 .. 0 0;

line65: for(int j = 0; j < nparm; j++){

            tmat.at(i,j) = zero;

         }

tmat.at(i,i) = one;

}

return;

// Set up dummy null transformation to bypass svd;

line40: xnew = clone(xold);

for(int i = 0; i < nparm; i++){

    tmat.at(i,i) = one;
    diag.at(i)   = one;

}

if(debug::kprint >= 2) {
  
   Rcpp::Rcout << "\nWQM_SVDFIX**5**\n" << std::endl; 
   Rcpp::Rcout << "xnew with fixed columns = \n" << xnew << std::endl;
   Rcpp::Rcout << "tmat with fixed columns = \n" << tmat << std::endl;
   Rcpp::Rcout << "diag with fixed columns = \n" << diag << std::endl;

  
}

 return;

}