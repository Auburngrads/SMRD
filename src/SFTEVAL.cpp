#include <base/base.hpp>
#include <sfteval/fteval.hpp>

//' Does good stuff
//' @name sfteval
// [[Rcpp::export]]
Rcpp::List SFTEVAL(int kdmod,
                   Rcpp::NumericVector xmu2,
                   Rcpp::NumericVector sig2,
                   Rcpp::NumericVector xmu3,
                   Rcpp::NumericVector sig3,
                   Rcpp::NumericVector rho,
                   Rcpp::NumericVector df,
                   Rcpp::NumericVector d0,
                   Rcpp::NumericVector sfact,
                   Rcpp::NumericVector tf,
                   int number,
                   Rcpp::NumericVector answer,
                   Rcpp::IntegerVector ier,
                   int kprint){

Rcpp::NumericVector RES(number);
Rcpp::List ints,numvec,intvec;

for(int i = 0; i < number; i++) {

 fteval(kdmod,
        xmu2.at(i),
        sig2.at(i),
        xmu3.at(i),
        sig3.at(i),
        rho.at(i),
        df.at(i),
        d0.at(i),
        sfact.at(i),
        tf.at(i),
        answer.at(i),
        ier.at(i));
  
}

ints = Rcpp::List::create(Named("number") = number,
                          Named("kdmod") = kdmod);

intvec = Rcpp::List::create(Named("ier") = ier);

numvec = Rcpp::List::create(Named("answer") = answer,
                            Named("xmu2") = xmu2,
                            Named("sig2") = sig2,
                            Named("xmu3") = xmu3,
                            Named("sig3") = sig3,
                            Named("rho") = rho,
                            Named("df") = df,
                            Named("d0") = d0,
                            Named("sfact") = sfact,
                            Named("tf") = tf);

return Rcpp::List::create(Named("ints") = ints,
                          Named("intvec") = intvec,
                          Named("numvec") = numvec);

}