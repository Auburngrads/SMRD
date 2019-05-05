#include <base/base.hpp>
#include <wqm_cdfest/wqm_cdffac.hpp>

//' @description Computes the Fisher information matrix;
//' @details If \code{ltrunc = false (true)}, compute
//'          the censoring (truncation) factors

void wqm_cdfcm(Rcpp::IntegerVector &ilcv,
               Rcpp::IntegerVector &iucv,
               Rcpp::IntegerVector &iltv,
               Rcpp::IntegerVector &iutv,
               Rcpp::NumericVector &probd,
               Rcpp::NumericVector &f,
               Rcpp::IntegerVector &weight,
               int &m,
               int &n,
               int &nty,
               int &m1,
               int &mm1,
               double &small,
               int &mnzs,
               int &nnzs){

bool ltrunc;
int ilc,ilt,iuc,iut,wt;

// Initialize f to be all zeros
   for(int i = 1; i <= mnzs; i++) { f.at(i - 1) = zero; }

// For each observation, 
// add (subtract) censoring (truncation) 
// factor to information matrix
   for(int i = 0; i < n; i++){

       ilc = ilcv.at(i - 1);
       if(ilc <= 0) continue;
       iuc = iucv.at(i - 1);

       if(nty == 0) {

          ilt = 1;
          iut = m;

        } else {

          ilt = iltv.at(i - 1);
          iut = iutv.at(i - 1);

        }

   wt = weight.at(i - 1);

   ltrunc =  false;
   
   // Call subroutine to compute and add censoring
   // factor to information matrix
      wqm_cdffac(ilc,iuc,f,probd,wt,ltrunc,small,
                 nnzs,mnzs,m,nty);

      ltrunc = true;

   // Call subroutine to compute and add truncation
   // factor to information matrix
      wqm_cdffac(ilt,iut,f,probd,wt,ltrunc,small,
                 nnzs,mnzs,m,nty);
      
}
   
return;
   
}
