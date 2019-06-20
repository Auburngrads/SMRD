#include <base/base.hpp>
#include <genfun/fdup.hpp>

//' compute a vector delta (powers of 10) to give approximately
//' eps change in func for each theta(i)
//' Return 0 in delta if kodet=0

void fdel(Rcpp::List (*func)(Rcpp::List),
          Rcpp::NumericVector &theta,
          int &ktrcde,
          Rcpp::IntegerVector &kodet,
          int &n,
          double &eps,
          Rcpp::NumericVector &delta){
   
double ten = 10.0e00,fval,thetai,deltai;
double funtu,funtl,diffu,diffl,fout,funtab;
int nitr = 15,lsmall;
Rcpp::List fargs,flist;

for(int i = 1; i <= n; i++){
   
    delta.at(i - 1) = zero;
    lsmall = 0;
    fargs = Rcpp::List::create(Named("lt") = theta,
                               Named("ln") = n);
    flist = func(fargs);
    fout = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
    fval = fout;
    
    // Bring down kmodel and kdist for when klmod>2
    // or for non location-scale distributions
       if(kodet.at(i - 1) == 0) continue;
       thetai = theta.at(i - 1);
       deltai = 1.0e-08;
       
    // Use a maximum of nitr iterations to find a suitable delta
       for(int k = 1; k <= nitr; k++){
          
           theta.at(i - 1) = fdup(thetai,deltai,ktrcde,kodet.at(i - 1));
           fargs = Rcpp::List::create(Named("lt") = theta,
                                      Named("ln") = n);
           flist = func(fargs);
           fout = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);

           funtu = fout;
           diffu = std::abs(funtu - fval);
           if(debug::kprint >= 5){
              
              Rcpp::Rcout << "\nFDEL**2**\n" << std::endl;
              Rcpp::Rcout << "i = " << i - 1 << std::endl;
              Rcpp::Rcout << "k = " << k - 1 << std::endl;
              Rcpp::Rcout << "lsmall = " << lsmall << std::endl;
              Rcpp::Rcout << "kodet(i) = " << kodet.at(i - 1) << std::endl;
              Rcpp::Rcout << "thetai = " << thetai << std::endl;
              Rcpp::Rcout << "theta(i) = " << theta.at(i - 1) << std::endl;
              Rcpp::Rcout << "deltai = " << deltai << std::endl;
              Rcpp::Rcout << "funtu = " << funtu << std::endl;
              Rcpp::Rcout << "fval = " << fval << std::endl;
              Rcpp::Rcout << "diffu = " << diffu << std::endl;
              
           }
           
      theta.at(i - 1) = fdup(thetai,-1 * deltai,ktrcde,kodet.at(i - 1));
      fargs = Rcpp::List::create(Named("lt") = theta,
                                 Named("ln") = n);
      flist = func(fargs);
      fout = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);

      funtl = fout;
      diffl = std::abs(funtl - fval);
      funtab = std::max(diffu,diffl);
      
      if(debug::kprint >= 4){
        
        Rcpp::Rcout << "\nFDEL**4**\n" << std::endl;
        Rcpp::Rcout << "i = " << i - 1 << std::endl;
        Rcpp::Rcout << "k = " << k - 1 << std::endl;
        Rcpp::Rcout << "lsmall = " << lsmall << std::endl;
        Rcpp::Rcout << "kodet(i) = " << kodet.at(i - 1) << std::endl;
        Rcpp::Rcout << "thetai = " << thetai << std::endl;
        Rcpp::Rcout << "theta(i) = " << theta.at(i - 1) << std::endl;
        Rcpp::Rcout << "deltai = " << deltai << std::endl;
        Rcpp::Rcout << "funtl = " << funtl << std::endl;
        Rcpp::Rcout << "fval = " << fval << std::endl;
        Rcpp::Rcout << "diffl = " << diffl << std::endl;
        
       }
      
      if(funtab > eps) goto line10;
      lsmall = 1;
      deltai = deltai * ten;
      goto line21;
      
line10: if(lsmall == 1) goto line23;
        deltai = deltai / ten;
        
line21: if((k > 4) and ((std::max(funtl,funtu) - fval) == zero)) goto line23;

       }
       
line23: delta.at(i - 1) = deltai;
        theta.at(i - 1) = thetai;
   
}

return;

}