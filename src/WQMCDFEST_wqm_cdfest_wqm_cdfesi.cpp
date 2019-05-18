#include <base/base.hpp>
#include <wqmcdfest/wqm_cdfcm.hpp>
#include <wqmcdfest/wqm_cdfmat.hpp>
#include <wqmcdfest/wqm_cdfcs.hpp>
#include <wqmcdfest/wqm_cdffac.hpp>
#include <wqmcdfest/wqm_cdfzs.hpp>
#include <wqmcdfest/wqm_invpx.hpp>

//' Function to call all other functions
//' 
//' @param weight Length n integer vector containing the weights
//' @param ilcv Length n integer vector containing the index of 
//'             the first p,q interval contained in the censoring 
//'             interval.
//' @param iucv Length n integer vector containing the index of 
//'             the last p,q interval contained in the censoring 
//'             interval.
//' @param iltv Length n integer vector containing the index of 
//'             the first p,q interval contained in the truncation 
//'             interval.
//' @param iutv Length n integer vector containing the index of 
//'             the last p,q interval contained in the truncation 
//'             interval.
//' @param probd Length m numeric vector containing the cdf step 
//'              probabilities
//' @param f (mnzs) where the information matrix is stored in 
//'                 symmetric storage mode. (see details)
//' # the element in the i-th row and j-th column;
//' # is stored as the j+i*(i-1)/2 element in f;
//' # the maximum size of f is m1=m*(m-1)/2;
//' # m the number of p,q intervals;
//' # n the number of observations;
//' # iprint if iprint ge 1, the lower triangle of the information;
//' # matrix and the variance matrix will be printed;
//' # small if any probabilities are less than this number, that;
//' # probability is suspected to be zero;
//' # nty if nty = 0, there is no truncation;
//' # if nty = 0, there is either left or right truncation;
//' # if nty = 2, both left or right truncation or interval;
//' # truncation can be present;
//' # maxmsd if nnzs is greater than maxmsd, standard errors will;
//' # not be given because the information matrix will be;
//' # too large;
//' #outputs:;
//' # sd(m) vector containing the standard errors of the cdf;
//' # f(mnzs) vector containing the variance matrix;
//' # the maximum size of f is m1=m*(m-1)/2;
//' # ier error code for subroutine wqm_cdfesi:;
//' # if ier = 0, matrix is pos. def.;
//' # if ier = 21, matrix is too big;
//' # if ier = 22, matrix is not pos. def.;
//' # if ier = 23, only one nonzero step probability;

void wqm_cdfesi(Rcpp::IntegerVector &weight,
                Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                Rcpp::NumericVector &probd,
                Rcpp::NumericVector &sd,
                int &m,
                int &n,
                double small,
                int &nty,
                int &ier,
                int &maxmsd){
  
int m1 = m * std::floor((m - 1) / 2);
int mm1 = m - 1;

// compute number of nonzero probability values;
// nnzs = the number of non-zero probabilities minus one;
// mnzs = nnzs*(nnzs-1)/2, the length of vector f;
int nnzs = m;
int mnzs = 0;
double pdk = 0;

for(int k = 1; k <= m; k++){

    pdk  = probd.at(k - 1);
    if(pdk > small) nnzs = m;
    nnzs = nnzs - 1;

}

nnzs = nnzs - 1;

if((nnzs <= 0) or (nnzs > maxmsd)) {

  wqm_cdfzs(m, ier, sd, nnzs, maxmsd);
  return;

}

mnzs = (nnzs + 1) * nnzs / 2;
Rcpp::NumericVector f = Rcpp::NumericVector(m1*10);

if(debug::kprint >= 5){
   
   Rcpp::Rcout << "\nCDFESI BEFORE CDFCM\n" << std::endl;
   Rcpp::Rcout << "mnzs = " << mnzs << std::endl;
   Rcpp::Rcout << "nnzs = " << nnzs << std::endl;
   Rcpp::Rcout << "ilcv = " << ilcv << std::endl;
   Rcpp::Rcout << "iucv = " << iucv << std::endl;
   Rcpp::Rcout << "iltv = " << iltv << std::endl;
   Rcpp::Rcout << "iutv = " << iutv << std::endl;
   Rcpp::Rcout << "probd = " << probd << std::endl;
   Rcpp::Rcout << "f = " << f << std::endl;
   Rcpp::Rcout << "weight = " << weight << std::endl;
   Rcpp::Rcout << "m = " << m << std::endl;
   Rcpp::Rcout << "n = " << n << std::endl;
   Rcpp::Rcout << "nty = " << nty << std::endl;
   Rcpp::Rcout << "m1 = " << m1 << std::endl;
   Rcpp::Rcout << "mm1 = " << mm1 << std::endl;
   Rcpp::Rcout << "small = " << small << std::endl;
   
}

// call subroutine to compute information matrix;
   wqm_cdfcm(ilcv,iucv,iltv,iutv,probd,f,
             weight, m, n, nty, m1,
             mm1, small, mnzs, nnzs);

// Call subroutine to print information matrix if iprint ge 1
   wqm_cdfmat(f,mm1,small,probd,mnzs,m,nnzs);
   
// Call matrix inversion subroutine
   wqm_invpx(f,nnzs,ier);
   
   if(debug::kprint >= 7){
      
      Rcpp::Rcout << "\ncheck f after wqm_invpx\n" << std::endl;
      Rcpp::Rcout << "f = " << f << std::endl;
      
   }
   
if(ier > 0) ier = 22;

// Call subroutine to print variance matrix if iprint ge 1
   wqm_cdfmat(f,mm1,small,probd,mnzs,m,nnzs);
   
// call subroutine to compute standard errors;
   wqm_cdfcs(sd,f,m,m1,mm1,probd,small,mnzs,nnzs);
   
if(nnzs > 0) return;

// call subroutine to return vector of zero standard deviations if nnzs=0
   wqm_cdfzs(m, ier, sd, nnzs, maxmsd);
   
return;

}
