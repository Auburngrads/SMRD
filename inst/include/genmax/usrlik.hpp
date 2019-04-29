#ifndef smrd_usrlik_H
#define smrd_usrlik_H
double usrlik(Rcpp::NumericVector thetas,
              int nparm,
              int kdist,
              int kmod,
              double yl,
              double yu,
              int kccode,
              double tryl,
              double tryu,
              int ktcode);
#endif