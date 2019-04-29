#ifndef SMRD_flkg_H
#define SMRD_flkg_H
double flkg(double (*flkgx)(int,double,double,Rcpp::NumericVector,int),
            int kdist,
            double yl,
            double yu,
            int kccode,
            int ncolty,
            double tryl,
            double tryu,
            int ktcode,
            Rcpp::NumericVector gamme,
            int ngame);
#endif