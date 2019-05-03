#include <base/base.hpp>
#include <genmax/unscv1.hpp>

using namespace genx03;
using namespace genx05;
using namespace genx07;
using namespace genx08;
using namespace genx20;
using namespace genx21;

//' Compute the vcv matrix corresponding to the unscaled and uncentered x

void unscvx(Rcpp::NumericMatrix &vcvs,
            Rcpp::NumericMatrix &vcv,
            int &idim){
   
      Rcpp::NumericVector ig1 = Rcpp::NumericVector(genx07::g_nparm);
      Rcpp::NumericVector ig2 = Rcpp::NumericVector(genx07::g_nparm);
      Rcpp::NumericMatrix ivcvn = Rcpp::NumericMatrix(idim,idim);
      
      Rcpp::IntegerVector NXG = clone(genx20::nxg);
      Rcpp::IntegerVector INTG = clone(genx20::intg);
      Rcpp::IntegerVector NTERG = clone(genx20::nterg);
      Rcpp::List IPXCG = clone(genx20::ipxcg);
      Rcpp::IntegerVector IPTHET = clone(genx21::ipthet);
      Rcpp::NumericVector IPXBRU = clone(genx05::g_ipxbru);
      Rcpp::NumericVector IPSD = clone(genx05::g_ipsd);
      
      unscv1(vcvs,vcv,ivcvn,idim,genx07::g_nparm,NXG,
             NTERG,INTG,IPXCG,IPTHET,genx03::g_ngame,
             IPXBRU,IPSD,ig1,ig2);
      
      genx20::nxg = clone(NXG);
      genx20::intg = clone(INTG);
      genx20::ipxcg = clone(IPXCG);
      genx05::g_ipxbru = clone(IPXBRU);
      genx05::g_ipsd = clone(IPSD);
      genx21::ipthet = clone(IPTHET);
      genx20::nterg = clone(NTERG);
      
return;

}

#include <base/base.hpp>
#include <genmax/pkgvs.hpp>
#include <genmax/pkgvx.hpp>

void unscv1(Rcpp::NumericMatrix &vcvs,
            Rcpp::NumericMatrix &vcv,
            Rcpp::NumericMatrix &vcvn,
            int &idim,
            int &nparm,
            Rcpp::IntegerVector &nxg,
            Rcpp::IntegerVector &nterg,
            Rcpp::IntegerVector &intg,
            Rcpp::List &ipxcg,
            Rcpp::IntegerVector &ipthet,
            int &ngame,
            Rcpp::NumericVector &xbaru,
            Rcpp::NumericVector &sd,
            Rcpp::NumericVector &g1,
            Rcpp::NumericVector &g2){
   
int nterg1,inthe1,nterg2,inthe2;
Rcpp::IntegerVector ipoinx, ipcol1, ipcol2;
   
// Pick up the proper sd's in g1 for scaling
   for(int igame = 1; igame <= ngame; igame++){
      
       // Skip save areas
          if(nterg.at(igame - 1) == 0) continue;
          
       // Get pointer to the pointer to the int x col numbers in is()
          SEXP l = ipxcg[igame - 1];
          Rcpp::IntegerVector ipoinx(l); 
          
          if(debug::kprint >= 6){
             
             Rcpp::Rcout << "\nUNSCV1 BEFORE PKGVS\n" << std::endl;
             Rcpp::Rcout << "igame = " << igame - 1 << std::endl;
             Rcpp::Rcout << "ipxcg[igame] = " << ipoinx << std::endl;
             
          }
          
       // Get scale factors from sd
          pkgvs(g1,nparm,ipthet.at(igame - 1),
                nterg.at(igame - 1),ipoinx,sd);
         
         
   }
   
if(debug::kprint >= 4){
   
   Rcpp::Rcout << "\nUNSCV1**4**\n" << std::endl;
   Rcpp::Rcout << "g1 = "     << g1 << std::endl;
   Rcpp::Rcout << "sd = "     << sd << std::endl;
}     

// Unscale the vcvs matrix
   for(int i = 1; i <= nparm; i++){
      
       for(int j = 1; j <= nparm; j++){
          
           vcvn.at(i - 1,j - 1) = vcvs.at(i - 1,j - 1) / (g1.at(i - 1) * g1.at(j - 1));
          
       }
   }
   
if(debug::kprint >= 4){
   
   Rcpp::Rcout << "\nUNSCV1**5**\n" << std::endl;
   Rcpp::Rcout << "ipthet = " << ipthet << std::endl;
   Rcpp::Rcout << "ipxcg = " << ipoinx << std::endl;
   Rcpp::Rcout << "intg = " << intg << std::endl;
   Rcpp::Rcout << "nxg = " << nxg << std::endl;
   Rcpp::Rcout << "nterg = " << nterg << std::endl;
   
}     

// Go through the row parameters
   for(int igame1 = 1; igame1 <= ngame; igame1++){
      
       nterg1 = nterg.at(igame1 - 1);
      
       // Skip save aread (nterg=0)
          if(nterg1 == 0) continue;
          
       // Get pointer to current theta vector and the current x column pointers
          SEXP l1 = ipxcg[igame1 - 1];
          Rcpp::IntegerVector ipcol1(l1); 
          
       // Now iterate for each term for the current dist parameter
          for(int iterg1 = 1; iterg1 <= nterg1; iterg1++){
             
              inthe1 = ipthet.at(igame1 - 1) + iterg1 - 1;
             
              // Pick up the g1 vector of linear transformation coefficients
                 pkgvx(iterg1,g1,nparm,nterg.at(igame1 - 1),
                       intg.at(igame1 - 1),nxg.at(igame1 - 1),
                       inthe1,xbaru,ipcol1);
                 
                 if(debug::kprint >= 4){
                    
                    Rcpp::Rcout << "\nUNSCVX**4**\n"  <<  std::endl;
                    Rcpp::Rcout << "iterg1 = "        << iterg1 - 1 << std::endl;
                    Rcpp::Rcout << "igame1 = "        << igame1 - 1 << std::endl;
                    Rcpp::Rcout << "inthe1 = "        << inthe1 - 1 << std::endl;
                    Rcpp::Rcout << "ipxcg(igame1) = " << ipcol1 << std::endl;
                    Rcpp::Rcout << "intg(igame1) = "  << intg.at(igame1 - 1) << std::endl;
                    Rcpp::Rcout << "nxg(igame1) = "   << nxg.at(igame1 - 1) << std::endl;
                    Rcpp::Rcout << "nterg(igame1) = " << nterg.at(igame1 - 1) << std::endl;
                    Rcpp::Rcout << "g1 = "            << g1 << std::endl;
                 }

                 // Go through the column parameters
                    for(int igame2 = 1; igame2 <= ngame; igame2++){
                       
                        nterg2 = nterg.at(igame2 - 1);
                       
                        if(nterg2 == 0) continue;
                        
                        SEXP l2 = ipxcg[igame2 - 1];
                        Rcpp::IntegerVector ipcol2(l2); 
                        
                        for(int iterg2 = 1; iterg2 <= nterg2; iterg2++){
                           
                            inthe2 = ipthet.at(igame2 - 1) + iterg2 - 1;
                           
                            // Pick up the g2 vector of linear transformation coefficients
                               pkgvx(iterg2,g2,nparm,nterg.at(igame2 - 1),
                                     intg.at(igame2 - 1),nxg.at(igame2 - 1),
                                     inthe2,xbaru,ipcol2);
                               
                               vcv.at(inthe1 - 1,inthe2 - 1) = zero;
                            // Get new variance (or map from old)
                               for(int i = 1; i <= nparm; i++){
                                  
                                   for(int j = 1; j <= nparm; j++){
                                      
                                       vcv.at(inthe1 - 1,inthe2 - 1) = vcv.at(inthe1 - 1,inthe2 - 1) + g1.at(i - 1) * g2.at(j - 1) * vcvn.at(i - 1,j - 1);
                                      
                                   }
                                     
                               }
if(debug::kprint >= 4){
   
   Rcpp::Rcout << "\nUNSCV1**end**\n"                              << std::endl;
   Rcpp::Rcout << "iterg2 = "              << iterg2 - 1           << std::endl;
   Rcpp::Rcout << "igame2 = "              << igame2 - 1           << std::endl;
   Rcpp::Rcout << "inthe2 = "              << inthe2 - 1           << std::endl;
   Rcpp::Rcout << "ipxcg(igame2) = "       << ipcol2               << std::endl;
   Rcpp::Rcout << "intg(igame2) = "        << intg.at(igame2 - 1)  << std::endl;
   Rcpp::Rcout << "nxg(igame2) = "         << nxg.at(igame2 - 1)   << std::endl;
   Rcpp::Rcout << "nterg(igame2) = "       << nterg.at(igame2 - 1) << std::endl;
   Rcpp::Rcout << "g2 = "                  << g2                   << std::endl;
   Rcpp::Rcout << "inthe1 = "              << inthe1 - 1           << std::endl;
   Rcpp::Rcout << "inthe2 = "              << inthe2 - 1           << std::endl;
   Rcpp::Rcout << "vcv(inthe1,inthe2) = "  <<  vcv.at(inthe1 - 1,inthe2 - 1) << std::endl;
   Rcpp::Rcout << "vcvs(inthe1,inthe2) = " << vcvs.at(inthe1 - 1,inthe2 - 1) << std::endl;
   Rcpp::Rcout << "vcvn(inthe1,inthe2) = " << vcvn.at(inthe1 - 1,inthe2 - 1) << std::endl;
   
}
         
                        } // end iterg2
         
                 } // end igame2
         
          } // end iterg1
         
   } // end igame1
   
return;

}