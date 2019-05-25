#include <base/base.hpp>
#include <utility/wqm_quant.hpp>

// kctype = 1 for type 1 at centim;
// kctype = 2 for type 2 with centim failures;
// looks like pretim (prediction time) is not used here;
// simulate the data for a group of obs;
// using the fast algorithm from chapter 4 of meeker and escobar;
// we do nothing with x here;
// ranf(1.) generates a single unif random number;
// nrownw returns the number of rows filled in this trip;

void smldat(Rcpp::NumericVector &theta,
            int &nparm,
            Rcpp::IntegerVector &nsamsz, // igroup
            int &kctype,
            Rcpp::NumericVector &centim, // igroup
            double &pretim,
            int &kdist,
            Rcpp::NumericMatrix &x, // iobs
            Rcpp::NumericMatrix &y, // iobs
            Rcpp::IntegerVector &cen, // iobs
            Rcpp::IntegerVector &wt, // iobs
            int &nrow,
            int &nter,
            int &ny,
            int &nty,
            Rcpp::NumericMatrix &ty, // iobs
            Rcpp::IntegerVector &tcodes, // iobs
            int &nrownw,
            Rcpp::IntegerVector &krfail, // igroup
            int &igroup,
            int &iref,
            int &kpred,
            int &iersim){
  
double xpower,uranod,uhold;
double smallp = 1.e-15, xcen = 0.0e00, time;
int kcenum, iobs;
iersim = 0;

// Initilize the counters
   uranod = 0.0300;
   kcenum = centim.at(igroup - 1);
   nrownw = 0;
   krfail.at(igroup - 1) = 0;
   kpred  = 0;
   
   if(nsamsz.at(igroup - 1) == 0) return;
   
for(iobs = 1; iobs <= nsamsz.at(igroup - 1); iobs++){

    // Check for type 2 censoring;
    // Quit when we have kcenum failures;
    if(iobs > 1) xcen = y.at((iobs - 2) + (iref - 1),ny - 1);
    
    if((kctype == 2) and (iobs > kcenum)) goto line100;

    xcen = centim.at(igroup - 1);
    xpower = one / (nsamsz.at(igroup - 1) - iobs + 1);
    uhold = R::unif_rand();
    uranod = one - (one - uranod) * std::pow(one - uhold, xpower);
    if(uranod >= (one - smallp)) uranod = one - smallp;

    if(uranod <= smallp) {
      
       uranod = smallp;
       Rcpp::Rcout << "\nZero uranod fixed in smldat\n" << std::endl;
       Rcpp::Rcout << "iobs = " << iobs - 1 << std::endl;
       Rcpp::Rcout << "nsamsz(igroup) = " << nsamsz.at(igroup - 1) << std::endl;
       Rcpp::Rcout << "kdist = " << kdist << std::endl;
       Rcpp::Rcout << "uhold = " << uhold << std::endl;
       Rcpp::Rcout << "uranod = " << uranod << std::endl;
    
    }

    time = theta.at(0) + theta.at(1) * wqm_quant(uranod,kdist);
    
    if((kdist % 2) == 0) time = std::exp(time);
    
    if(debug::kprint >= 5){
  
       Rcpp::Rcout << "\nSMLDAT**\n" << std::endl;
       Rcpp::Rcout << "iobs = " << iobs  << std::endl;
       Rcpp::Rcout << "nsamsz(igroup) = " << nsamsz.at(igroup - 1)  << std::endl;
       Rcpp::Rcout << "kdist = " <<  kdist << std::endl;
       Rcpp::Rcout << "centim(igroup) = " << centim.at(igroup - 1)  << std::endl;
       Rcpp::Rcout << "time = " << time  << std::endl;
       Rcpp::Rcout << "uranod = " << uranod  << std::endl;
       Rcpp::Rcout << "theta = " <<  theta << std::endl;
       Rcpp::Rcout << "iref = "  << iref << std::endl; 
       Rcpp::Rcout << "krfail(igroup) = " << krfail.at(igroup - 1) << std::endl;
  
    }

    // Check for type 1 censoring
    // If the most recent failure is greater than censoring time, quit
       if((kctype == 1) and (time >= centim.at(igroup - 1))) goto line100;
   
    // We have a failure (before cens time for type 1)
    // Check to see if there is enough space
       if(iobs > nrow) {
   
          // If nor mark error and return
             iersim = 1;
             return;
             
       }
       
       y.at((iobs - 1) + (iref - 1),ny - 1) = time;
       cen.at((iobs - 1) + (iref - 1)) = 1;
       wt.at((iobs - 1) + (iref - 1)) = 1;

}

iobs = nsamsz.at(igroup - 1) + 1;

// Compute the number failing before the censoring time
   line100: krfail.at(igroup - 1) = iobs - 1;

// Compute the number rows of data; return if no censoring
   nrownw = nsamsz.at(igroup - 1);
   kpred = 0;
   if(krfail.at(igroup - 1) == nsamsz.at(igroup - 1)) return;
   nrownw = krfail.at(igroup - 1) + 1;

// If we need the number in the pred region, use rbino later
   kpred = 0;
   if(debug::kprint >= 4) {
      
      Rcpp::Rcout << "\nSMLDAT**3\n" << std::endl;
      Rcpp::Rcout << "krfail(igroup) = " << krfail.at(igroup - 1) << std::endl;
      Rcpp::Rcout << "nrownw = "         << nrownw << std::endl;
      Rcpp::Rcout << "iref = "           << iref   << std::endl;
      
   }
// Now record the censoring times, if any
   y.at((nrownw - 1) + (iref - 1),ny - 1) = xcen;
   cen.at((nrownw - 1) + (iref - 1)) = 2;
   wt.at((nrownw - 1) + (iref - 1)) = nsamsz.at(igroup - 1) - krfail.at(igroup - 1);

   if(debug::kprint >= 4) {
      
      Rcpp::Rcout << "\nSMLDAT**4\n" << std::endl;
      Rcpp::Rcout << "  y = \n" << y   << std::endl;
      Rcpp::Rcout << "cen = "   << cen << std::endl;
      Rcpp::Rcout << " wt = "   << wt  << std::endl;
      
   }

   
// If we need the number in the pred region, use rbino later
   kpred = 0;

return;

}