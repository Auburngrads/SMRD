#include <base/base.hpp>
#include <wqmmlesss/wqm_varco1.hpp>

//' Compute the variance-covariance matrix of the 
//' maximum likelihood estimates of the model 
//' parameters

void wqm_varcov(Rcpp::NumericMatrix &xnew,
                Rcpp::NumericMatrix &y,
                Rcpp::IntegerVector &cen,
                Rcpp::IntegerVector &wt,
                int &nty,
                Rcpp::NumericMatrix &ty,
                Rcpp::IntegerVector &tcodes,
                int &nrow,
                int &nter,
                int &ny,
                int &kdist,
                Rcpp::NumericVector &thetg,
                Rcpp::NumericVector &fsderd,
                Rcpp::LogicalVector &lfix,
                int &nparm,
                double &upcen,
                Rcpp::NumericMatrix &vcvg,
                Rcpp::NumericVector &dscrat,
                Rcpp::IntegerVector &iscrat,
                int &ier,
                Rcpp::NumericVector &ivd,
                Rcpp::NumericMatrix &ivcvd,
                Rcpp::NumericMatrix &ivcvdd,
                Rcpp::IntegerVector &iir,
                Rcpp::IntegerVector &ijc){
  
int nparmp = nparm + 1;

  wqm_varco1(xnew,y,cen,wt,nty,ty,tcodes,nrow,
             nter,ny,kdist,thetg,fsderd,lfix,
             nparm,upcen,vcvg,ivd,ivcvd,ivcvdd,
             iir,ijc,nparmp,ier);
  
if(debug::kprint > 0) {
  
   Rcpp::Rcout << "\nVARCOV\n"        << std::endl;
   Rcpp::Rcout << " thetg = " << thetg << std::endl;
   Rcpp::Rcout << "fsderd = " << fsderd << std::endl;
   Rcpp::Rcout << "  vcvg = \n" << vcvg << std::endl;
        
}

return;

}


#include <base/base.hpp>
#include <wqmmlesss/wqm_dfxmu.hpp>
#include <wqmsphiall/wqm_ztran.hpp>
#include <wqmmlesss/wqm_verrck.hpp>
#include <wqmmlesss/wqm_dinsss.hpp>
#include <wqmmlesss/wqm_derivt.hpp>

void wqm_varco1(Rcpp::NumericMatrix &xnew,
                Rcpp::NumericMatrix &y,
                Rcpp::IntegerVector &cen,
                Rcpp::IntegerVector &wt,
                int &nty,
                Rcpp::NumericMatrix &ty,
                Rcpp::IntegerVector &tcodes,
                int &nrow,
                int &nter,
                int &ny,
                int &kdist,
                Rcpp::NumericVector &thetg,
                Rcpp::NumericVector &fsderd,
                Rcpp::LogicalVector &lfix,
                int &nparm,
                double &upcen,
                Rcpp::NumericMatrix &vcvg,
                Rcpp::NumericVector &thetad,
                Rcpp::NumericMatrix &vcvd,
                Rcpp::NumericMatrix &vcvdd,
                Rcpp::IntegerVector &ir,
                Rcpp::IntegerVector &jc,
                int &nparmp,
                int &ier){
  
Rcpp::NumericVector qder(5);
double xtol = 1.0e-12;
double sigma,sigma2,xmu,z,z2,tru,trl;
double s1,s2,s3,xj,xk,sfder,s = 0.0e00;
int nparmm, jj,kk = 0;
int wti, ittype, itype, irank = 0;
ier = 0;

// zero the accumulation matrix;
for(int j = 1; j <= nparm; j++){
  
    thetad.at(j - 1) = thetg.at(j - 1);
    fsderd.at(j - 1) = zero;
    
    for(int k = 1; k <= j; k++){
      
        vcvd.at(k - 1,j - 1) = zero;

      }
}

sigma = thetg.at(nparm - 1);
    
if(sigma < 1.0e-15){
  
   ier = 8;
   return;

}

sigma2 = sigma * sigma;

// accumulate the elements of the info mat one observation at a time;
for(int i = 1; i <= nrow; i++){
  
    itype = cen.at(i - 1);
    if(itype == 0) continue;
    wti = wt.at(i - 1); 
    xmu = wqm_dfxmu(i,xnew,nrow,nter,thetad,nparm,upcen,sigma);
    z = wqm_ztran((y.at(i - 1,0) - xmu) / sigma,kdist);

    if(itype == 4) z2 = wqm_ztran((y.at(i - 1,ny - 1) - xmu) / sigma, kdist);

    // truncation stuff;
    if(nty > 0) {

       trl = (ty.at(i - 1,0) - xmu) / sigma;
       trl = wqm_ztran(trl,kdist);
       ittype = tcodes.at(i - 1);

       if(ittype == 4){

          tru = (ty.at(i - 1,1) - xmu) / sigma;
          tru = wqm_ztran(tru,kdist);

       }
    }

    // compute the second derivative factors;
       wqm_derivt(qder,itype,z,z2,nty,ittype,trl,tru,kdist);

    s1 = qder.at(2);
    s2 = qder.at(3);
    s3 = qder.at(4);
    wti = wt.at(i - 1);

    for(int j = 1; j <= nparm; j++){

        if(lfix.at(j - 1)) continue;

        // pick up appropriate x factor;
           xj = one;
           if(j < nparm) xj = xnew.at(i - 1,j - 1);
 
        // first derivative factor;
           if(j < nparm){ sfder = qder.at(0); } else { sfder = qder.at(1); }
 
           fsderd.at(j - 1) = fsderd.at(j - 1) + sfder * xj * wti;
   
           for(int k = 1; k <= j; k++){
    
               if(!lfix.at(k - 1)) {
                 
                  xk = one;
      
                  if(k < nparm) xk = xnew.at(i - 1,k - 1);
                  if((j < nparm)  and (k < nparm))  s = s1;
                  if((j == nparm) and (k < nparm))  s = s2;
                  if((j == nparm) and (k == nparm)) s = s3;
                  vcvd.at(k - 1,j - 1) = vcvd.at(k - 1,j - 1) + xj * xk * wti * s;
               
                  if(debug::kprint >= 4){
                    
                     Rcpp::Rcout << "\nVARCO1**4**\n" << std::endl;
                     Rcpp::Rcout << "        j = " << j - 1 << std::endl;
                     Rcpp::Rcout << "        k = " << k - 1 << std::endl;
                     Rcpp::Rcout << " thetg(j) = " << thetg.at(j - 1) << std::endl;
                     Rcpp::Rcout << " thetg(k) = " << thetg.at(k - 1) << std::endl;
                     Rcpp::Rcout << "        s = " << s << std::endl;
                     Rcpp::Rcout << "    sfder = " << sfder << std::endl;
                     Rcpp::Rcout << "fsderd(j) = " << fsderd.at(j - 1) << std::endl;
                     Rcpp::Rcout << "vcvd(k,j) = " << vcvd.at(k - 1,j - 1) << std::endl;
                     
                  }
               
               }
               
            }
    }
    
    if(debug::kprint >= 3) {
      
       Rcpp::Rcout << "\nVARCO1**3**\n" << std::endl;
       Rcpp::Rcout << "     i = " << i << std::endl;
       Rcpp::Rcout << " itype = " << itype << std::endl;
       Rcpp::Rcout << "ittype = " << ittype << std::endl;
      
    }
}

// Divide by the scale factor sigma2 and eliminate zero rows/cols
jj = 0;

for(int j = 1; j <= nparm; j++){

    if(lfix.at(j - 1)) continue;
    jj = jj + 1;
    kk = 0;

    for(int k = 1; k <= j; k++){

        if(!lfix.at(k - 1)) {
          
           kk = kk + 1;
           vcvdd.at(kk - 1,jj - 1) = vcvd.at(k - 1,j - 1) / sigma2;
           vcvdd.at(jj - 1,kk - 1) = vcvdd.at(kk - 1,jj - 1);
        
        }

    }

    fsderd.at(j - 1) = fsderd.at(j - 1) / sigma;

}

// the following check will signal a fatal error if all parameters are fixed;
ier = 1;
if(jj > 0) {

   ier = 0;
   nparmm = jj;

   // find the variance-covariance matrix by inversion if the info matrix;
      wqm_dinsss(vcvdd,nparmm,xtol,irank,jc,ir,nparmp);

   if(irank != kk) ier = 1;

   // expand the vcv matrix for fixed parameters;
      jj = 0;

   for(int j = 1; j <= nparm; j++){

       if(lfix.at(j - 1)) {

          for(int k = 1; k <= j; k++){

              vcvg.at(j - 1,k - 1) = zero;
              vcvg.at(k - 1,j - 1) = zero;

          }

        } else {

          jj = jj + 1;
          kk = 0;

          for(int k = 1; k <= j; k++){

              if(lfix.at(k - 1)) {

                 vcvg.at(j - 1,k - 1) = zero;
                 vcvg.at(k - 1,j - 1) = zero;

               } else {

                 kk = kk + 1;
                 vcvg.at(k - 1,j - 1) = vcvdd.at(kk - 1,jj - 1);
                 vcvg.at(j - 1,k - 1) = vcvdd.at(kk - 1,jj - 1);

               }
           }
       }
   }
}

// call routine to check the fisher information matrix;
   wqm_verrck(vcvd,vcvdd,fsderd,lfix,xtol,nparm,nparmm,ier);

 return;

}