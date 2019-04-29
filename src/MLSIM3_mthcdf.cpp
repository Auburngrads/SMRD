#include <base/base.hpp>

// subroutine to match bootstrap ecdf with the sample ecdf;
void mthcdf(Rcpp::NumericVector &pold,
            Rcpp::NumericVector &qold,
            Rcpp::NumericVector &prbold,
            Rcpp::NumericVector &sdold,
            int &mold,
            Rcpp::NumericVector &pnew,
            Rcpp::NumericVector &qnew,
            Rcpp::NumericVector &prbnew,
            Rcpp::NumericVector &sdnew,
            int &mnew,
            Rcpp::NumericVector &prtvec,
            Rcpp::NumericVector &srtvec){

bool ltest;
int inew = 1;

// Check print
if(debug::kprint >= 2) {
  
   for(int iold = 1; iold <= mold; iold++){
     
       Rcpp::Rcout << "\nMTHCDF**2\n"      << std::endl;
     
       if(iold <= mnew) {
         
          Rcpp::Rcout << "iold = "         << iold                << std::endl;
          Rcpp::Rcout << "mnew = "         << mnew                << std::endl;
          Rcpp::Rcout << "pold(iold) = "   << pold.at(iold - 1)   << std::endl; 
          Rcpp::Rcout << "qold(iold) = "   << qold.at(iold - 1)   << std::endl; 
          Rcpp::Rcout << "prbold(iold) = " << prbold.at(iold - 1) << std::endl;
          Rcpp::Rcout << "pnew(iold) = "   << pnew.at(iold - 1)   << std::endl;
          Rcpp::Rcout << "qnew(iold) = "   << qnew.at(iold - 1)   << std::endl;
          Rcpp::Rcout << "prbnew(iold) = " << prbnew.at(iold - 1) << std::endl;
         
        } else {
         
          Rcpp::Rcout << "iold = "         << iold                << std::endl;
          Rcpp::Rcout << "mnew = "         << mnew                << std::endl;
          Rcpp::Rcout << "pold(iold) = "   << pold.at(iold - 1)   << std::endl; 
          Rcpp::Rcout << "qold(iold) = "   << qold.at(iold - 1)   << std::endl; 
          Rcpp::Rcout << "prbold(iold) = " << prbold.at(iold - 1) << std::endl;
         
        }
    }
}

// Do the work
for(int iold = 1; iold <= mold; iold++){
  
    line20: ltest = qold.at(iold - 1) <= qnew.at(inew - 1);
      
    if(debug::kprint >= 2) {
      
       Rcpp::Rcout << "\nMTHCDF**3\n"                          << std::endl;
       Rcpp::Rcout << "ltest = "        << ltest               << std::endl;
       Rcpp::Rcout << "iold = "         << iold                << std::endl;
       Rcpp::Rcout << "pold(iold) = "   << pold.at(iold - 1)   << std::endl; 
       Rcpp::Rcout << "qold(iold) = "   << qold.at(iold - 1)   << std::endl; 
       Rcpp::Rcout << "inew = "         << inew                << std::endl;
       Rcpp::Rcout << "pnew(inew) = "   << pnew.at(inew - 1)   << std::endl;
       Rcpp::Rcout << "qnew(inew) = "   << qnew.at(inew - 1)   << std::endl;
      
    }
    
    if(qold.at(iold - 1) <= qnew.at(inew - 1)) {
      
       prtvec.at(iold - 1) = prbnew.at(inew - 1);
       srtvec.at(iold - 1) = sdnew.at(inew - 1);
       continue;
    
     } 
      
     inew = inew + 1;
      
     if(inew <= mnew) goto line20;
         
     if(iold > mold) return;
            
     for(int ioldx = iold; ioldx <= mold; ioldx++){
           
         prtvec.at(ioldx - 1) = prtvec.at(iold - 2);
         srtvec.at(ioldx - 1) = srtvec.at(iold - 2);
             
     }
             
     if(debug::kprint >= 2) {
               
        Rcpp::Rcout << "\nProblem in mthcdf\n" << std::endl;
        Rcpp::Rcout << "inew = " << inew << std::endl;
        Rcpp::Rcout << "iold = " << iold << std::endl;
        Rcpp::Rcout << "mnew = " << mnew << std::endl;
        Rcpp::Rcout << "mold = " << mold << std::endl;
        
        for(int iold = 1; iold <= mold; iold++) {
          
            Rcpp::Rcout << "iold = "         << iold                << std::endl;
            Rcpp::Rcout << "pold(iold) = "   << pold.at(iold - 1)   << std::endl;
            Rcpp::Rcout << "pold(iold) = "   << pold.at(iold - 1)   << std::endl;
            Rcpp::Rcout << "prbold(iold) = " << prbold.at(iold - 1) << std::endl;
            Rcpp::Rcout << "prtvec(iold) = " << prtvec.at(iold - 1) << std::endl;
          
        }
             
     }
             
     return;
    
}

// Check print
   if(debug::kprint >= 2) {
     
      Rcpp::Rcout << "\nMTHCDF Check Print\n" << std::endl;
     
      for(int iold = 1; iold <= mold; iold++) {
        
          Rcpp::Rcout << "iold = "         << iold                << std::endl;
          Rcpp::Rcout << "pold(iold) = "   << pold.at(iold - 1)   << std::endl;
          Rcpp::Rcout << "pold(iold) = "   << pold.at(iold - 1)   << std::endl;
          Rcpp::Rcout << "prbold(iold) = " << prbold.at(iold - 1) << std::endl;
          Rcpp::Rcout << "prtvec(iold) = " << prtvec.at(iold - 1) << std::endl;
        
      }
   
   }

return;

}
