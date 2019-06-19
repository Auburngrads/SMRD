#include <base/base.hpp>
#include <genfun/gvec1.hpp>

//' Compute the gradient of func evaluated at theta, return in d(n)
//' increments must be provided in delta
//' values of theta(i) with delta(i)=0 are ignored

void gvec(Rcpp::List (*func)(Rcpp::List),
          int &npoint,
          Rcpp::NumericVector &theta,
          Rcpp::NumericVector &delta,
          int &n,
          int &ktrcde,
          Rcpp::IntegerVector &kodet,
          Rcpp::NumericVector &d){
   
int maxnp = 20;
   
// Set up scratch arrays
   Rcpp::NumericVector itm = Rcpp::NumericVector(n);
   Rcpp::NumericVector itp = Rcpp::NumericVector(n);
   Rcpp::NumericVector itms = Rcpp::NumericVector(maxnp);
   Rcpp::NumericVector itps = Rcpp::NumericVector(maxnp);
   
   gvec1(func,npoint,theta,delta,n,ktrcde,
         kodet,d,itm,itp,itms,itps);
   
return;
   
}

#include <base/base.hpp>
#include <genfun/fdup.hpp>
#include <genfun/sett.hpp>
#include <genfun/gett.hpp>
#include <genfun/exadd.hpp>
#include <genfun/ufxsdi.hpp>

//' Compute the gradient vector by by
//' npoint=0    single call to func(theta,n)
//' npoint>0    npoint calls to func(i,theta,n)

void gvec1(Rcpp::List (*func)(Rcpp::List),
           int &npoint,
           Rcpp::NumericVector &theta,
           Rcpp::NumericVector &delta,
           int n,
           int &ktrcde,
           Rcpp::IntegerVector &kodet,
           Rcpp::NumericVector &d,
           Rcpp::NumericVector &tm,
           Rcpp::NumericVector &tp,
           Rcpp::NumericVector &tms,
           Rcpp::NumericVector &tps){
   
double small = 1.0e-30;
double deltai,thetai,tdelt,xmlll,xplll;
double funcm,funcp,acct,Diff,Diff2;
double fout,foutsp,foutsm;
Rcpp::List fargsm,fargsp,flist;
Rcpp::NumericVector accum = Rcpp::NumericVector(2);
int weight;

for(int i = 1; i <= n; i++){
   
    tm.at(i - 1) = theta.at(i - 1);
    tp.at(i - 1) = theta.at(i - 1);
    
}

for(int i = 1; i <= n; i++){
   
    d.at(i - 1) = zero;
    thetai = theta.at(i - 1);
    deltai = delta.at(i - 1);
       
    if(deltai < small) goto line126;
    
    tdelt = two * deltai;
    tm.at(i - 1) = fdup(thetai,-1 * deltai,ktrcde,kodet.at(i - 1));
    tp.at(i - 1) = fdup(thetai,deltai,ktrcde,kodet.at(i - 1));
    
    if(npoint == 0) goto line125;
    
    accum.at(0) = zero;
    accum.at(1) = zero;
    xmlll = zero;
    xplll = zero;

    // Get untrans theta and store xsave stuff at the end of tms and tps
       gett(tm,tms);
       gett(tp,tps);

       for(int k = 1; k <= npoint; k++){

           // Restore the xsave stuff in common for next func eval
              sett(tms);
              fargsm = Rcpp::List::create(Named("lk") = k,
                                          Named("lt") = tms,
                                          Named("ln") = n);
              flist  = func(fargsm);
              fout   = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
              weight = Rcpp::as<int>(Rcpp::as<List>(flist)["weight"]);
              funcm  = fout;
              
              sett(tps);
              fargsp = Rcpp::List::create(Named("lk") = k,
                                          Named("lt") = tps,
                                          Named("ln") = n);
              flist  = func(fargsp);
              fout   = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
              weight = Rcpp::as<int>(Rcpp::as<List>(flist)["weight"]);

              funcp = fout;
              xmlll = xmlll + weight * funcm;
              xplll = xplll + weight * funcp;

              exadd((-1 * weight * funcp),accum);
              exadd(( 1 * weight * funcm),accum);
              acct = accum.at(0) + accum.at(1);

              if(debug::kprint >= 9){

                 Rcpp::Rcout << "\nGVEC**8**\n" << std::endl;
                 Rcpp::Rcout << " funcp = " << funcp << std::endl;
                 Rcpp::Rcout << " funcm = " << funcm << std::endl;
                 Rcpp::Rcout << " accum = " << accum << std::endl;
                 Rcpp::Rcout << "  acct = " << acct << std::endl;
                 Rcpp::Rcout << "weight = " << weight << std::endl;

              }

              Diff  = (xplll - xmlll) / tdelt;
              Diff2 = (accum.at(0) + accum.at(1)) / tdelt;

              if(debug::kprint >= 9){

                 Rcpp::Rcout << "\nGVEC1**9**\n" << std::endl;
                 Rcpp::Rcout << "xmlll = " << xmlll << std::endl;
                 Rcpp::Rcout << "xplll = " << xplll << std::endl;
                 Rcpp::Rcout << " diff = " << Diff << std::endl;
                 Rcpp::Rcout << "diff2 = " << Diff2 << std::endl;

              }

       }

       d.at(i - 1) = (accum.at(0) + accum.at(1)) / tdelt;

       if(debug::kprint >= 4){

          Rcpp::Rcout << "\nGVEC1**4**\n" << std::endl;
          Rcpp::Rcout << "    i = "     << i - 1 << std::endl;
          Rcpp::Rcout << "tdelt = " << tdelt << std::endl;
          Rcpp::Rcout << "xmlll = " << xmlll << std::endl;
          Rcpp::Rcout << "xplll = " << xplll << std::endl;
          Rcpp::Rcout << "tdelt = " << tdelt << std::endl;
          Rcpp::Rcout << "accum = " << accum << std::endl;
          Rcpp::Rcout << " d(i) = "  << d.at(i - 1) << std::endl;

       }

      goto line126;
       
line125: fargsp  = Rcpp::List::create(Named("lt") = tp,
                                      Named("ln") = n);
         flist   = func(fargsp);
         foutsp  = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
         
         fargsm  = Rcpp::List::create(Named("lt") = tm,
                                      Named("ln") = n);
         flist   = func(fargsm);
         foutsm  = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
         
         d.at(i - 1) = (foutsp - foutsm) / tdelt;

line126: if(debug::kprint >= 4){
   
            Rcpp::Rcout << "\nGVEC1**5**\n" << std::endl;
            Rcpp::Rcout << "       i = " << i - 1 << std::endl;
            Rcpp::Rcout << "kodet(i) = " << kodet.at(i - 1) << std::endl;
            Rcpp::Rcout << "npoint = "   << npoint << std::endl;
            Rcpp::Rcout << "ktrcde = "   << ktrcde << std::endl;
            Rcpp::Rcout << "deltai = "   << deltai << std::endl;
            Rcpp::Rcout << "thetai = "   << thetai << std::endl;
            Rcpp::Rcout << "  d(i) = "   << d.at(i - 1) << std::endl;
   
         }

         tm.at(i - 1) = thetai;
         tp.at(i - 1) = thetai;
         
         // If delta was transformed, convert via chain rule
            if(ktrcde ==  1) ufxsdi(d.at(i - 1),thetai,kodet.at(i - 1),d.at(i - 1));
         
}

return;

}
