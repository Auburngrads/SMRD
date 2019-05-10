#include <base/base.hpp>
#include <utility/icheck.hpp>
#include <utility/filli.hpp>

//' Looks at exp var relationships and model and decides
//' whether or not to center each explanatory variable. If 
//' an explanatory variable is to be centered, iscd is set 
//' to 1 otherwise it is set to 0

void fdiscd(Rcpp::IntegerVector &iscd,
            int &ncolx,
            Rcpp::IntegerVector &intd,
            Rcpp::List &ipxcd,
            Rcpp::IntegerVector &irelad,
            Rcpp::IntegerVector &nxd,
            int &npard,
            int &kcentr,
            int &kmod,
            int &ier){

// Initialize new variables
   int nterd, icolp;
   Rcpp::IntegerVector pcols;
   
// If kcentr=0, no centering so set indicators to 0 and return;
   int ncolxp = ncolx + 1;
   filli(0,iscd,1,ncolxp);
   if(kcentr <= 0) return;

// Otherwise look at the model and relationships
// start with yes on all except the column of ones
   filli(1,iscd,2,ncolx);

// If this is a proportional hazards model, we will always center
// because of the implied intercept in the baseline distribution
   if(kmod == 4) return;

// Go over each distribution parameter
   for(int i = 1; i <= npard; i++){

       // If we have an intercept and a linear relationship, we will center
          if((intd.at(i - 1) == 1) and (irelad.at(i - 1) == 1)) continue;
       
       // If nonlinear with intercept and kcentr=2, center since 
       // we want output in terms of the centered variables
       // Also, center all if param ph and doing location parameter
          bool check1 = ((intd.at(i - 1) == 1) and (kcentr == 2));
          bool check2 = ((i == 1) and (kmod == 4));
          
          if((check1) or (check2)) continue;
       
       // Otherwise, mark all of the x's so that we do not center;
          nterd = nxd.at(i - 1) + intd.at(i - 1);
          
       for(int ix = 0; ix < nterd; ix++){
          
           // Get column number plus 1
              pcols = as<IntegerVector>(ipxcd)[i - 1];
              icolp = pcols.at(ix) + 1;
          
              if(debug::kprint >= 4){
                 
                 Rcpp::Rcout << "\nfdiscd**4**"     << std::endl;
                 Rcpp::Rcout << "i = "     << i     << std::endl;
                 Rcpp::Rcout << "ix = "    << ix    << std::endl;
                 Rcpp::Rcout << "icolp = " << icolp << std::endl;
                 
              }
              
              icheck(icolp,1,ncolxp,1,1,ier,-3352);
              iscd.at(icolp - 1) = 0;
       
       }

}
   
if(debug::kprint >= 4){
   
   Rcpp::Rcout << "\nfdiscd**4**"   << std::endl;
   Rcpp::Rcout << "iscd = " << iscd << std::endl; 
   
}

return;

}
