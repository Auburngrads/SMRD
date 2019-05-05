#include <base/base.hpp>
#include <wqm_cdfest/wqm_cdfezk.hpp>
#include <wqm_cdfest/wqm_divckd.hpp>

//' @description Computes a single iteration of
//'              Turnbull's self-consistancy
//'              algorithm

void wqm_cdfemi(Rcpp::NumericVector &s,
                Rcpp::NumericVector &probd,
                int &m,
                Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &weight,
                int &nty,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                int &n,
                double &xnobs){

double suma,sumb,facta,factb = 0,xm;
int ilc = 0,iuc = 0,ilt = 0,iut = 0,iltm = 0,iutp = 0;

xm = xnobs;
if(nty != 0) xm = one;

for(int j = 1; j <= m; j++) { probd.at(j - 1) = zero; }

// #increment over all observations;
for(int i = 1; i <= n; i++) {

    ilc = ilcv.at(i - 1);
    iuc = iucv.at(i - 1);
    if((ilc <= 0) or (iuc <= 0)) continue;
    suma = zero;
    
    // Compute the failure probability denominator
       for(int j = ilc; j <= iuc; j++){
          
           suma = suma + s.at(j - 1);

       }

      facta = weight.at(i - 1) / (xm * suma);

if(nty != 0) {

   factb = zero;
   sumb = zero;
   ilt = iltv.at(i - 1);
   iut = iutv.at(i - 1);

   // #compute the observation (non-truncation) failure probability denominator;
   for(int j = ilt; j <= iut; j++) {

       sumb = sumb + s.at(j - 1);

   }

   factb = weight.at(i - 1) / sumb;
}

// Compute mu(i,j) and add into prob(j) over i=1,n
   for(int j = ilc; j <= iuc; j++) {
   
       probd.at(j - 1) = probd.at(j - 1) + s.at(j - 1) * facta;
   
   }

if(nty == 0) continue;

if(ilt > 1) {

   iltm = ilt - 1;

   for(int j = 1; j <= iltm; j++) {

       probd.at(j - 1) = probd.at(j - 1) + s.at(j - 1) * factb;

   }
}

if(iut >= m) continue;

iutp = iut + 1;

for(int j = iutp; j <= m; j++) {

    probd.at(j - 1) = probd.at(j - 1) + s.at(j - 1) * factb;

}
}

if(nty == 0) return; 

// Normalize by dividing by estimated sample size m
   xm = zero;

   for(int j = 1; j <= m; j++) { xm = xm + probd.at(j - 1); }

   for(int j = 1; j <= m; j++){

       probd.at(j - 1) = wqm_divckd(probd.at(j - 1),xm);

   }

return;
   
}

#include <base/base.hpp>
#include <wqm_cdfest/wqm_cdfegr.hpp>

//' @description Function to check probabilities
//'              to determine when one is close to 0

void wqm_cdfezk(Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                Rcpp::IntegerVector &weight,
                int &nty,
                int &n,
                Rcpp::NumericVector &probd,
                int &m,
                Rcpp::NumericVector &pgrad,
                double &biggr,
                double &smpro,
                int &indc){
  
double zero = 0.0,grmax,absp;
int mm1, jhold;
indc = 0;


if(m <= 2) return;

  wqm_cdfegr(ilcv,iucv,iltv,iutv,weight,
             nty,n,probd,m,pgrad);

mm1 = m - 1;

grmax = std::abs(pgrad.at(0));
jhold = 1;

  for(int j = 2; j <= mm1; j++){

      absp = std::abs(pgrad.at(j - 1) );

      if(grmax > absp) continue;

      grmax = absp;
      jhold = j;
      
  }

if(!((grmax >= biggr) and (probd.at(jhold - 1) <= smpro))) return;

probd.at(jhold - 1) = zero;
pgrad.at(jhold - 1) = zero;

indc = 1;

return;

}

#include <base/base.hpp>

//' @description Function to check for underflow 
//'              before dividing

double wqm_divckd(double xnum,
                  double xden){

double xnumr, xdenr, iexp1, iexp2;
int iexpb = 31;

if(xnum == zero) return zero;

xnumr = xnum;
xdenr = xden;

iexp1 = std::log10( std::abs(1.0 / xnumr) );
iexp2 = std::log10( std::abs(xdenr) );
  
if((iexp1 + iexp2) > iexpb) return zero;

return  xnum / xden;

}
            