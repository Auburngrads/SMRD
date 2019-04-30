#include <base/base.hpp>

//' Does good stuff
//' 
//' @name prcs
// [[Rcpp::export]]
Rcpp::List PRCS(NumericVector zmax,
                NumericVector z1,
                int nsim,
                NumericVector dvec,
                NumericVector answer,
                int nd){

int kount;
for(int id = 0; id < nd; id++){

    kount = 0;

    for(int isim = 0; isim < nsim; isim++) {

        if(z1.at(isim) > (zmax.at(isim) - dvec.at(id))) {

          kount = kount + 1;

        }
    }

    answer.at(id) = float(kount) / float(nsim);
    
    }

 return Rcpp::List::create(Named("answer") = answer,
                           Named("dvec")   = dvec);

}

/***R
*/
