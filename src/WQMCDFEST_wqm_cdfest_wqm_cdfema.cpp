#include <base/base.hpp>
#include <wqmcdfest/wqm_cdfemi.hpp>
#include <wqmcdfest/wqm_cdfezk.hpp>

//' @description Use the em algorithm according to
//'              Turnbull to find the nonparametric
//'              maximum likelihood estimate of the cdf

void wqm_cdfema(Rcpp::IntegerVector &weight,
                Rcpp::IntegerVector &codes,
                Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &iltv,
                Rcpp::IntegerVector &iutv,
                Rcpp::NumericVector &pgrad,
                Rcpp::NumericVector &prob,
                Rcpp::NumericVector &s,
                Rcpp::NumericVector &probd,
                int &n,
                int &m,
                int &nty,
                int &nstart,
                int &maxit,
                double &tol,
                double &pchmax){

double cprob = 0;
double absdm = 0,absd = 0,tolabs = 0;

// #set up derivative check constants;
NumericVector biggrp(4, 0.0);
NumericVector smprop(4, 0.0);
int nnow = 4;
int itype = 0;
int indc = 0;
double zero = 0.0;
double xnobs = zero;
double x2obs = zero;
double x3obs = zero;
IntegerVector inc   = IntegerVector::create(10,5,2,1);
NumericVector biggr = NumericVector::create(5.0,2.0,1.2,0.2);
NumericVector smpro = NumericVector::create(1.0e-3,1.0e-2,1.0e-1,1.0e01);

// count the number of real observations using weights
// to get a good relative tolerance factor

for(int i = 1; i <= n; i++){

    itype = codes.at(i - 1);
    if(itype == 0) continue;
    xnobs = xnobs + weight.at(i - 1);

    if((itype == 1) or (itype == 4) or (itype == 5)){
      
        x2obs = x2obs + weight.at(i - 1);
        x3obs = x3obs + weight.at(i - 1);
        
    }

    if(itype == 2){
      
       x2obs = x2obs + weight.at(i - 1);
      
    }
    
    if(itype == 3){
      
       x3obs = x3obs + weight.at(i - 1);
      
    }

}

tolabs = tol * std::min(x2obs,x3obs) / xnobs;
double xninv = 1.0 / xnobs;

for(int inow = 1; inow <= nnow; inow++){
  
    // biggrp(inow)=biggr(inow)/tolabs;
    // smprop(inow)=smpro(inow)*tolabs;
    biggrp.at(inow - 1) = 1.01;
    smprop.at(inow - 1) = xninv;
    // smprop(4)=1.0d00/xnobs;
    // biggrp(4)=1.01;
    
    if(debug::kprint > 1){
      
       Rcpp::Rcout << "\nWQM_CDFEMA\n"      << std::endl;
       Rcpp::Rcout << "inow = "      << inow - 1            << std::endl;
       Rcpp::Rcout << "tolabs = "    << tolabs          << std::endl;
       Rcpp::Rcout << "biggr(inow) = "  << biggr.at(inow - 1)  << std::endl;
       Rcpp::Rcout << "biggrp(inow) = " << biggrp.at(inow - 1) << std::endl;
       Rcpp::Rcout << "smpro(inow) = "  << smpro.at(inow - 1)  << std::endl;
       Rcpp::Rcout << "smprop(inow) = " << smprop.at(inow - 1) << std::endl;
      
    }

}

int inow = 1;

// #initial probability estimates;
for(int j = 1; j <= m; j++){

    if(nstart != 0) s.at(j - 1) = prob.at(j - 1);
    if(nstart <= 0) s.at(j - 1) = 1.0e00 / m;

}
// #iterations for turnbull self-consistancy algorithm;
for(int iter = 1; iter <= maxit; iter++){

    if(iter == (maxit - 100)) inow = 2;
    if(iter == (maxit - 50))  inow = 3;

    wqm_cdfemi(s,probd,m,ilcv,iucv,
               weight,nty,iltv,iutv,n,xnobs);

// Check for cell probabilities approaching the boundary
   if((iter < 10) or ((iter % inc.at(inow - 1)) != 1)) goto line50;
   
      wqm_cdfezk(ilcv,iucv,iltv,iutv,weight,
                 nty,n,probd,m,pgrad,
                 biggrp.at(inow - 1),smprop.at(inow - 1),indc);
     
// Check for convergence
   line50: absdm = zero;

for(int j = 1; j <= m; j++){

    absd = std::abs(s.at(j - 1) - probd.at(j - 1));
    absdm = std::max(absd,absdm);
    
    if((debug::kprint >= 10) and ((iter % debug::kprint) == 1)) {
      
        Rcpp::Rcout << "\nWQM_CDFEMA\n"                 << std::endl;
        Rcpp::Rcout << "iter = "     << iter - 1        << std::endl;
        Rcpp::Rcout << "j = "        << j - 1           << std::endl;
        Rcpp::Rcout << "probd(j) = " << probd.at(j - 1) << std::endl;
        Rcpp::Rcout << "s(j) = "     << s.at(j - 1)     << std::endl;
        Rcpp::Rcout << "pgrad(j) = " << pgrad.at(j - 1) << std::endl;
        Rcpp::Rcout << "absd = "     << absd            << std::endl;
        Rcpp::Rcout << "absdm= "     << absdm           << std::endl;
      
      
    }
    
    s.at(j - 1) = probd.at(j - 1);

}

   if(debug::kprint >= 1){
     
      Rcpp::Rcout << "\nWQM_CDFEMA\n" << std::endl;
      Rcpp::Rcout << "iteration = "   << iter  << std::endl;
      Rcpp::Rcout << "max change = "  << absdm << std::endl;
      Rcpp::Rcout << "tolabs = "      << tolabs << std::endl;
     
   }


   if(absdm <= tolabs) goto line97;

}

pchmax = absdm;

line97: inow = 4;

line98: wqm_cdfemi(s,probd,m,ilcv,iucv,weight,nty,
                   iltv,iutv,n,xnobs);

        wqm_cdfezk(ilcv,iucv,iltv,iutv,weight,nty,
                   n,probd,m,pgrad,biggrp.at(inow - 1),
                   smprop.at(inow - 1),indc);
        
for(int j = 1; j <= m; j++) { s.at(j - 1) = probd.at(j - 1); }

if(indc != 0) goto line98;

// Accumulate the cdf estimate into prob

cprob = zero;

for(int j = 1; j <= m; j++) {

    cprob = cprob + probd.at(j - 1);
    prob.at(j - 1) = cprob;

}

return;

}


