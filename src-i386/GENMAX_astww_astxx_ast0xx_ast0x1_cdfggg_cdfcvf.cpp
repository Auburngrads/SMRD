#include <base/base.hpp>

//' Check for and remove double jump at the same point in time

void cdfcvf(Rcpp::NumericVector &p,
            Rcpp::NumericVector &q,
            Rcpp::NumericVector &prob,
            Rcpp::NumericVector &sd,
            int &lsd,
            int &m){
  
int mm = 1,ii;
  
for(int i = 2; i <= m; i++){
  
    if(p.at(i - 1) != p.at(mm - 1)) mm = mm + 1;
    
    if(debug::kprint >= 4){
      
       Rcpp::Rcout << "\nCDFCVF**4**\n" << std::endl;
       Rcpp::Rcout << "m = " << m << std::endl;
       Rcpp::Rcout << "i = " << i - 1 << std::endl;
       Rcpp::Rcout << "mm = " << mm << std::endl;
       Rcpp::Rcout << "p(i) = "  << p.at(i - 1) << std::endl;
       Rcpp::Rcout << "p(mm) = " << p.at(mm - 1) << std::endl;
      
    }
    
    p.at(mm - 1) = p.at(i - 1);
    q.at(mm - 1) = q.at(i - 1);
    prob.at(mm - 1) = prob.at(i - 1);
    if(lsd == 1) sd.at(mm - 1) = sd.at(i - 1);
        
}

m = mm;

// If wqm_cdfest does not start at zero, make it do so at the level prob=0

// 10 april 1991
// Not sure why we implemented the below procedure,
// but it does cause problems with some of the probability
// plotting stuff.  for now, we will skip over this part.
   if(p.at(0) > zero) return;
   if(p.at(0) <= zero) return;
   
   for(int i = 1; i <= m; i++){
     
       ii = m - i + 1;
       p.at(ii) = p.at(ii - 1);
       q.at(ii) = q.at(ii - 1);
       prob.at(ii) = prob.at(ii - 1);
       if(lsd == 1) sd.at(ii) = sd.at(ii - 1);
        
   }
   
   p.at(0) = zero;
   q.at(0) = p.at(1);
   prob.at(0) = zero;
   if(lsd == 1) sd.at(0) = zero;
   m = m + 1;
   
return;

}