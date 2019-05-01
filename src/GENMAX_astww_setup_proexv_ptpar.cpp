#include <base/base.hpp>
#include <genmax/gsplit.hpp>
#include <utility/filli.hpp>

//' This function does the following
//' 1) Adds a distribution parameter to the gammd list
//' 2) Set ipthet for thetas translate
//' 3) Add to game information vectors
//' 4) Count the number of theta and gamma parameters
//' 5) Get kodet from igtyd

void ptpar(int &kdnow,
           int &itnext,
           int &ignext,
           Rcpp::IntegerVector &kodet,
           int &nregr,
           int &kscloc,
           int &kprloc,
           int &kpwloc,
           Rcpp::IntegerVector &nxd,
           Rcpp::IntegerVector &intd,
           Rcpp::IntegerVector &irelad,
           Rcpp::IntegerVector &igtyd,
           Rcpp::List &ipxcd,
           Rcpp::IntegerVector &imarkd,
           Rcpp::IntegerVector &nxg,
           Rcpp::IntegerVector &intg,
           Rcpp::IntegerVector &irelag,
           Rcpp::IntegerVector &igtyg,
           Rcpp::List &ipxcg,
           Rcpp::IntegerVector &ipthet,
           Rcpp::IntegerVector &nterg){

// Break apart igtyd(kdnow)
// igsave =  number of save words for the parameter
// igkode =  kodet for parameter
// iglab  =  label code
   int igsave = 0;
   int igkode = 0;
   int iglab = 0;
   gsplit(igtyd.at(kdnow - 1),igsave,igkode,iglab);

   if(debug::kprint >= 5){
      
      Rcpp::Rcout << "\nPTPAR AFTER GSPLIT\n" << std::endl;
      Rcpp::Rcout << "igsave = " << igsave << std::endl;
      Rcpp::Rcout << "igkode = " << igkode << std::endl;
      Rcpp::Rcout << "iglab = " << iglab << std::endl;
      Rcpp::Rcout << "ignext = " << ignext << std::endl;
      Rcpp::Rcout << "itnext = " << itnext << std::endl;
      Rcpp::Rcout << "nxg = " << nxg << std::endl;
      Rcpp::Rcout << "nxd = " << nxd << std::endl;
      Rcpp::Rcout << "nterg = " << nterg << std::endl;
      Rcpp::Rcout << "kodet = " << kodet << std::endl;
      Rcpp::Rcout << "intg = " << intg << std::endl;
      Rcpp::Rcout << "ipthet = " << ipthet << std::endl;
      Rcpp::Rcout << "irelag = " << irelag << std::endl;
      
   }
// copy over nx so that we know what to do in gtgame
   filli(nxd.at(kdnow - 1),nxg,ignext,igsave);

// fill others with zeros
   filli(0,nterg ,ignext,igsave);
   filli(0,intg  ,ignext,igsave);
   filli(0,igtyg ,ignext,igsave);
   filli(0,ipthet,ignext,igsave);
   filli(0,irelag,ignext,igsave);
//   filli(0,ipxcg ,ignext,igsave);

// Copy over the useful information
   nxg.at(ignext - 1) = nxd.at(kdnow - 1);

// Count number of regression relationships
   if(nxg.at(ignext - 1) > 0) nregr = nregr + 1;
   nterg.at(ignext - 1)  = nxd.at(kdnow - 1) + intd.at(kdnow - 1);
   intg.at(ignext - 1)   = intd.at(kdnow - 1);
   irelag.at(ignext - 1) = irelad.at(kdnow - 1);
   igtyg.at(ignext - 1)  = igkode;
   kodet.at(itnext - 1)  = igkode;

// If this is a variable with explanatory variables, set all kodet=1
   if(nxg.at(ignext - 1) > 0) filli(1,kodet,itnext,nterg.at(ignext - 1));

// Make sure that we have a constant if no explanatory variables
   if(nxd.at(kdnow - 1) == 0) intd.at(kdnow - 1) = 1;

// Move pointer to the appropriate col of x (or col of ones)e
   ipxcg[ignext - 1] = ipxcd[kdnow - 1];

// Get pointer to the thetas vector
   ipthet.at(ignext - 1) = itnext;

// Set pointers to special parameters
// Scale probability power
   if(imarkd.at(kdnow - 1) == 2) kscloc = ignext;
   if(imarkd.at(kdnow - 1) == 3) kprloc = ignext;
   if(imarkd.at(kdnow - 1) == 4) kpwloc = ignext;

// Increment parameter counters
   itnext = itnext + nterg.at(ignext - 1);
// if(igsave == 0) igsave = 1
   ignext = ignext + igsave;

return;

}
