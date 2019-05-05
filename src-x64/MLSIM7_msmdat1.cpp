#include <base/base.hpp>
#include <mlsim2/smldat.hpp>

// # generate a staggered entry data set using time censoring;
// # specify the common ending time;
//
// # this was originally designed to do prediction;

void msmdat1(Rcpp::NumericVector &theta,
             int &nparm,
             Rcpp::IntegerVector &nsamsz,
             Rcpp::NumericVector &centim,
             int &kdist,
             Rcpp::NumericMatrix &x,
             Rcpp::NumericMatrix &y,
             Rcpp::IntegerVector &cen,
             Rcpp::IntegerVector &wt,
             int &nrow,
             int &nter,
             int &ny,
             int &nty,
             Rcpp::NumericMatrix &ty,
             Rcpp::IntegerVector &tcodes,
             Rcpp::IntegerVector &krfail,
             int &ngroup,
             int &nrownw,
             double &prdelt,
             int &kpredt,
             int &iersim){
  
kpredt = 0;
int iobs = 1,nrleft,kctype,nmr,kpred = 0;
double pretim;

// Loop across the groups
   for(int igroup = 1; igroup <= ngroup; igroup++){

       // Get the prediction-period end time
          pretim = centim.at(igroup - 1) + prdelt;
          nrleft = nrow - iobs + 1;
   
       // Check for memory problems (make sure enough memory has been allocated)
       // this check is too stringent;
       
       // if(nsamsz(igroup) > nrleft) write(6,433)igroup,ngroup,;
       // & nsamsz(igroup),nrleft,nrow;
       //433 format(' msmdat1: igroup,ngroup,nsamsz,nrleft,nrow',5i8);

       // Simulate the data and count number failing in delta intervals

       // kctype = 1 for type 1 censored data
          kctype = 1;
          smldat(theta,nparm,nsamsz,kctype,centim,
                 pretim,kdist,x,y,cen,wt,
                 nrleft,nter,ny,nty,ty,tcodes,nrownw,
                 krfail,igroup,iobs,kpred,iersim);

       // Get the pointer to the next observation
          iobs = iobs + nrownw;
          
   if((iersim > 0) & (debug::kprint > 0)) {
     
      Rcpp::Rcout << "\nmsmdat1 error\n"                            << std::endl;
      Rcpp::Rcout << "igroup = "           << igroup - 1            << std::endl;
      Rcpp::Rcout << "nsamsz(igroup) = "   << nsamsz.at(igroup - 1) << std::endl;
      Rcpp::Rcout << "nrleft = "           << nrleft         << std::endl;
      Rcpp::Rcout << "nrow = "             << nrow           << std::endl;
      Rcpp::Rcout << "iersim = "           << iersim         << std::endl;
      Rcpp::Rcout << "nrownw = "           << nrownw         << std::endl;
      Rcpp::Rcout << "iobs = "             << iobs           << std::endl;
     
   }

// Sum up the total number of units that failed in the delta period
   nmr = nsamsz.at(igroup - 1) - krfail.at(igroup - 1);
   
if(debug::kprint >= 1) {
  
   Rcpp::Rcout << "\nMSMDAT1**A\n"                            << std::endl;
   Rcpp::Rcout << "igroup"           << igroup - 1            << std::endl;
   Rcpp::Rcout << "centim(igroup)"   << centim.at(igroup - 1) << std::endl;
   Rcpp::Rcout << "prdelt"           << prdelt                << std::endl;
   Rcpp::Rcout << "pretim"           << pretim                << std::endl;
   Rcpp::Rcout << "nsamsz(igroup)"   << nsamsz.at(igroup - 1) << std::endl;
   Rcpp::Rcout << "krfail(igroup)"   << krfail.at(igroup - 1) << std::endl;
   Rcpp::Rcout << "nmr"              << nmr                   << std::endl;
   Rcpp::Rcout << "kpred"            << kpred                 << std::endl;
   Rcpp::Rcout << "kpredt"           << kpredt                << std::endl;
  
}
  
kpredt = kpredt + kpred;

if(debug::kprint >= 1) {
  
   Rcpp::Rcout << "\nMSMDAT1**B\n"                            << std::endl;
   Rcpp::Rcout << "igroup"           << igroup - 1            << std::endl;
   Rcpp::Rcout << "iobs"             << iobs - 1              << std::endl;
   Rcpp::Rcout << "centim(igroup)"   << centim.at(igroup - 1) << std::endl;
   Rcpp::Rcout << "prdelt"           << prdelt                << std::endl;
   Rcpp::Rcout << "pretim"           << pretim                << std::endl;
   Rcpp::Rcout << "nsamsz(igroup)"   << nsamsz.at(igroup - 1) << std::endl;
   Rcpp::Rcout << "krfail(igroup)"   << krfail.at(igroup - 1) << std::endl;
   Rcpp::Rcout << "nrleft"           << nrleft                << std::endl;
   Rcpp::Rcout << "kpred"            << kpred                 << std::endl;
   Rcpp::Rcout << "kpredt"           << kpredt                << std::endl;
   Rcpp::Rcout << "nrownw"           << nrownw                << std::endl;
   Rcpp::Rcout << "theta(0)"         << theta.at(0)           << std::endl;
   Rcpp::Rcout << "theta(1)"         << theta.at(1)           << std::endl;
  
}

   }
   
nrownw = iobs - 1;

// Later will need to call a scruncher to take care of nrow vs nrownw
// Now should be ok because we only use first col of any matrix
      
return;

}
