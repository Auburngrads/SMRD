#include <base/base.hpp>

using namespace explan_g;

//' Set up explanatory variable relationships for parameters.
//' Non local stack pointers are picked up for ipxcd.

void explan(Rcpp::IntegerVector &kparv,
            Rcpp::IntegerVector &nrvar,
            Rcpp::IntegerMatrix &mrelat,
            int &nrelat,
            int &mnrvar,
            Rcpp::IntegerVector &nxd,
            Rcpp::IntegerVector &intd,
            Rcpp::List &ipxcd,
            Rcpp::IntegerVector &irelad,
            int &ncolx,
            int &ier,
            int &npardm,
            int &nnum,
            int &kparm,
            int &iup,
            int &nterd){

Rcpp::IntegerVector my_vec, my_col, my_col2;

if(debug::kprint >= 4){
   
   for(int ik = 0; ik < 4; ik++){
     
       if(ipxcd[ik] != R_NilValue) {
      
          SEXP l = ipxcd[ik]; Rcpp::IntegerVector y(l);
          Rcpp::Rcout << "i = " << ik << std::endl;
          Rcpp::Rcout << "ipxcd(i) = " << y << std::endl;
     
   }
   }
  
   Rcpp::Rcout << "\nEXPLAN START\n" << std::endl;
   Rcpp::Rcout << "nnum = "  << nnum << std::endl;
   Rcpp::Rcout << "iup = "   << iup << std::endl;
   Rcpp::Rcout << "kparm = " << kparm << std::endl;
   Rcpp::Rcout << "nxd = "   << nxd << std::endl;
   Rcpp::Rcout << "intd = "  << intd << std::endl;
   Rcpp::Rcout << "irel = "  << irelad << std::endl;
  
}

if(ncolx == 0) goto exit;
if(nrelat == 0) goto exit;

for(int irnow = 0; irnow < nrelat; irnow++){

    nnum = nrvar.at(irnow) + 1;
  
    if(nnum == 0) goto exit;
    
    if((nnum < 2) or (nnum > 40)){
        
        ier = 1;
        goto exit;
      
    }
    
    kparm = kparv.at(irnow);

    // #check that parameter number is in range;
    if((kparm < 1) or (kparm > npardm)){
        
        ier = 2;
        goto exit;
      
    }

    if(intd.at(kparm - 1) != 1000) { ier = 3; goto exit; }
    
    // #iup=1 for intercept;
    iup = 1;

    // #iup=2 for no intercept;
    if(mrelat.at(0,irnow) == 0) iup = 2;
    nxd.at(kparm - 1) = nnum - iup;
    intd.at(kparm - 1) = 2 - iup;
    irelad.at(kparm - 1) = 1;

    // #make sure that we have a constant if no explanatory variables;
    if(nxd.at(kparm - 1) == 0) intd.at(kparm - 1) = 1;
    nterd = nxd.at(kparm - 1) + intd.at(kparm - 1);
    //ipxcd.at(kparm - 1) = nterd;

    // #if nxd(j)=0, we will use the default x pointer from above;
    if(nxd.at(kparm - 1) == 0) {
     
       ipxcd[irnow] = IntegerVector(1,0);
       
     // if(irnow == 0) explan_g::mu_cols = IntegerVector(1,0);
     // if(irnow == 1) explan_g::si_cols = IntegerVector(1,0);
     // if(irnow == 2) explan_g::p1_cols = IntegerVector(1,0);
     // if(irnow == 3) explan_g::p2_cols = IntegerVector(1,0);
     // if(irnow == 4) explan_g::p3_cols = IntegerVector(1,0);
      
    } else {

       // #otherwise we need a new pointer to a longer vector;
       // #copy over the column numbers for the explanatory variables;
       // #first set up intercept;

       my_col = mrelat.column(irnow);
       //my_col2 = IntegerVector((nxd.at(kparm - 1) - (iup)),0);
       int vec_n = nrvar.at(irnow); 
       // for(int j = 0; j < vec_n; j++){
       //   
       //     my_col2.at(j) = my_col.at(iup - 1 + j);
       //   
       // }
       
       if(intd.at(kparm - 1) == 1) {
          
          my_vec = IntegerVector(vec_n + 1, 0);
          
          for(int i = 1; i < vec_n + 1; i++){
            
            my_vec.at(i) = my_col.at((iup - 1) + (i - 1));
            
          }
          
         
       } else {
         
          my_vec = IntegerVector(vec_n, 0);
          
          for(int i = 0; i < vec_n; i++){
            
            my_vec.at(i) = my_col.at((iup - 1) + (i));
            
          }
         
       } ;

       // Then copy over the others;
          ipxcd[irnow] = my_vec;
       // if(irnow == 0) explan_g::mu_cols = my_vec;
       // if(irnow == 1) explan_g::si_cols = my_vec;
       // if(irnow == 2) explan_g::p1_cols = my_vec;
       // if(irnow == 3) explan_g::p2_cols = my_vec;
       // if(irnow == 4) explan_g::p3_cols = my_vec;

    }

if(debug::kprint >= 4){
  
   for(int ik = 0; ik < 4; ik++){
     
       if(ipxcd[ik] != R_NilValue) {
      
          SEXP l = ipxcd[ik]; Rcpp::IntegerVector y(l);
          Rcpp::Rcout << "i = " << ik << std::endl;
          Rcpp::Rcout << "ipxcd(i) = " << y << std::endl;
     
   }
     
   }
   
   Rcpp::Rcout << "\nEXPLAN iter = " << irnow << std::endl;
   Rcpp::Rcout << "nnum = "  << nnum  << std::endl;
   Rcpp::Rcout << "iup = "   << iup   << std::endl;
   Rcpp::Rcout << "kparm = " << kparm << std::endl;
   Rcpp::Rcout << "nxd(kparm) = "   << nxd.at(kparm)    << std::endl;
   Rcpp::Rcout << "intd(kparm) = "  << intd.at(kparm)   << std::endl;
   Rcpp::Rcout << "irel(kparm) = "  << irelad.at(kparm) << std::endl;

}

}

exit: for(int j = 0; j < npardm; j++){

          if(intd.at(j) == 1000) intd.at(j) = 1;

      }

return;
}
