#include <base/base.hpp>
#include <wqmcdfest/wqm_cdffac.hpp>

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

   for(int i = 1; i <= n; i++){

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
      if(debug::kprint >= 6){
         
         Rcpp::Rcout << "\nCDFCM CDFFAC1\n" << std::endl;
         Rcpp::Rcout << "i = " << i << std::endl;
         Rcpp::Rcout << "ilc = " << ilc << std::endl;
         Rcpp::Rcout << "iuc = " << iuc << std::endl;
         Rcpp::Rcout << "f = " << f << std::endl;
         Rcpp::Rcout << "probd = " << probd << std::endl;
         Rcpp::Rcout << "wt = " << wt << std::endl;
         Rcpp::Rcout << "ltrunc = " << ltrunc << std::endl;
         Rcpp::Rcout << "small = " << small << std::endl;
         Rcpp::Rcout << "m = " << m << std::endl;
         Rcpp::Rcout << "nty = " << nty << std::endl;
         
      }
      wqm_cdffac(ilc,iuc,f,probd,wt,ltrunc,small,
                 nnzs,mnzs,m,nty);

      ltrunc = true;

   // Call subroutine to compute and add truncation
   // factor to information matrix
      if(debug::kprint >= 6){
         
         Rcpp::Rcout << "\nCDFCM CDFFAC2\n" << std::endl;
         Rcpp::Rcout << "i = " << i << std::endl;
         Rcpp::Rcout << "ilt = " << ilt << std::endl;
         Rcpp::Rcout << "iut = " << iut << std::endl;
         Rcpp::Rcout << "f = " << f << std::endl;
         Rcpp::Rcout << "probd = " << probd << std::endl;
         Rcpp::Rcout << "wt = " << wt << std::endl;
         Rcpp::Rcout << "ltrunc = " << ltrunc << std::endl;
         Rcpp::Rcout << "small = " << small << std::endl;
         Rcpp::Rcout << "m = " << m << std::endl;
         Rcpp::Rcout << "nty = " << nty << std::endl;
         
      }

      wqm_cdffac(ilt,iut,f,probd,wt,ltrunc,small,
                 nnzs,mnzs,m,nty);
      
}
   
return;
   
}
