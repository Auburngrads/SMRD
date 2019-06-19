#include <base/base.hpp>
#include <genmax/hmat1.hpp>

//' Compute the hessian matrix of func evaluated at theta
//' values of theta with delta=0 are ignored.

void hmat(Rcpp::List (*func)(Rcpp::List),
          int &npoint,
          Rcpp::NumericVector &theta,
          Rcpp::NumericVector &delta,
          int &n,
          Rcpp::NumericMatrix &v,
          int &idim){

int maxnp = 20;   
         
// Set up scratch arrays
   Rcpp::NumericVector itmm = Rcpp::NumericVector(n);
   Rcpp::NumericVector itmp = Rcpp::NumericVector(n);
   Rcpp::NumericVector itpm = Rcpp::NumericVector(n);
   Rcpp::NumericVector itpp = Rcpp::NumericVector(n);
   Rcpp::NumericVector itmms = Rcpp::NumericVector(n + maxnp);
   Rcpp::NumericVector itmps = Rcpp::NumericVector(n + maxnp);
   Rcpp::NumericVector itpms = Rcpp::NumericVector(n + maxnp);
   Rcpp::NumericVector itpps = Rcpp::NumericVector(n + maxnp);
   Rcpp::NumericVector itfunv = Rcpp::NumericVector(npoint);
   
   hmat1(func,npoint,theta,delta,n,v,idim,itmm,itmp,
         itpm,itpp,itmms,itmps,itpms,itpps,itfunv);
   
   return;
   
}

#include <base/base.hpp>
#include <genfun/sett.hpp>
#include <genfun/gett.hpp>
#include <genfun/exadd.hpp>

//' compute the hessian matrix by by
//' npoint=0    single call to func(theta,n)
//' npoint>0    npoint calls to func(i,theta,n)

void hmat1(Rcpp::List (*func)(Rcpp::List),
           int &npoint,
           Rcpp::NumericVector &theta,
           Rcpp::NumericVector &delta,
           int &n,
           Rcpp::NumericMatrix &v,
           int &idim,
           Rcpp::NumericVector &tmm,
           Rcpp::NumericVector &tmp,
           Rcpp::NumericVector &tpm,
           Rcpp::NumericVector &tpp,
           Rcpp::NumericVector &tmms,
           Rcpp::NumericVector &tmps,
           Rcpp::NumericVector &tpms,
           Rcpp::NumericVector &tpps,
           Rcpp::NumericVector &tfunv){
   
Rcpp::NumericVector accum = Rcpp::NumericVector(2);
double small = 1.0e-30,thetai,deltai,deltaj,thetaj;
double xmmlll, xmplll, xpmlll, xpplll, xflll;
double tfuns = 0.0e00,fout = 0.0e00;
double foutmm = 0.0e00,foutmp = 0.0e00,foutpm = 0.0e00,foutpp = 0.0e00;
double d12,funcmm,funcmp,funcpm,funcpp;
int jm1,weight;
Rcpp::List fargsmm,fargsmp,fargspm,fargspp,flist;

gett(theta,tmms);
if(npoint == 0) goto line135;
xflll = zero;

for(int k = 1; k <= npoint; k++){
   
    fargsmm = Rcpp::List::create(Named("lk") = k,
                                 Named("lt") = tmms,
                                 Named("ln") = n);
    flist = func(fargsmm);
    fout = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
    weight = Rcpp::as<int>(Rcpp::as<List>(flist)["weight"]);
  
    tfunv.at(k - 1) = fout * two;
    xflll = xflll + weight * tfunv.at(k - 1);
   
}

xflll = xflll / two;

goto line136;

line135: fargsmm = Rcpp::List::create(Named("lt") = tmms,
                                      Named("ln") = n);

         flist = func(fargsmm);
         fout = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
         
         tfuns = fout * two;

line136: for(int i = 1; i <= n; i++){
   
             thetai = theta.at(i - 1);
             tmm.at(i - 1) = thetai;
             tmp.at(i - 1) = thetai;
             tpm.at(i - 1) = thetai;
             tpp.at(i - 1) = thetai;
   
         }

for(int j = 1; j <= n; j++){

    deltaj = delta.at(j - 1);
    thetaj = theta.at(j - 1);
    if(j == 1) goto line500;
    
    jm1 = j - 1;
    tmm.at(j - 1) = thetaj - deltaj;
    tmp.at(j - 1) = thetaj - deltaj;
    tpm.at(j - 1) = thetaj + deltaj;
    tpp.at(j - 1) = thetaj + deltaj;
    
    for(int i = 1; i <= jm1; i++){
       
        v.at(i - 1,j - 1) = zero;
        deltai = delta.at(i - 1);
        thetai = theta.at(i - 1);
        
        if((deltai < small) or (deltaj < small)) goto line126;
        
        d12 = four * deltai * deltaj;
        tmm.at(i - 1) = thetai - deltai;
        tmp.at(i - 1) = thetai + deltai;
        tpm.at(i - 1) = thetai - deltai;
        tpp.at(i - 1) = thetai + deltai;
        
        if(npoint == 0) goto line125;
        
        accum.at(0) = zero;
        accum.at(1) = zero;
        xmmlll = zero;
        xmplll = zero;
        xpmlll = zero;
        xpplll = zero;
        
        // Get untrans theta followed by xsave values
           gett(tmm,tmms);
           gett(tmp,tmps);
           gett(tpm,tpms);
           gett(tpp,tpps);
           
           for(int k = 1; k <= npoint; k++){
              
               // Set the xsave values in common for next func eval
                  sett(tmms);
                  fargsmm = Rcpp::List::create(Named("lk") = k,
                                               Named("lt") = tmms,
                                               Named("ln") = n);
                  flist  = func(fargsmm);
                  funcmm = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
                  weight = Rcpp::as<int>(Rcpp::as<List>(flist)["weight"]);
                  
                  sett(tmps);
                  fargsmp = Rcpp::List::create(Named("lk") = k,
                                               Named("lt") = tmps,
                                               Named("ln") = n);
                  flist  = func(fargsmp);
                  funcmp = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
                  weight = Rcpp::as<int>(Rcpp::as<List>(flist)["weight"]);
                  
                  sett(tpms);
                  fargspm = Rcpp::List::create(Named("lk") = k,
                                               Named("lt") = tpms,
                                               Named("ln") = n);
                  flist  = func(fargspm);
                  funcpm = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
                  weight = Rcpp::as<int>(Rcpp::as<List>(flist)["weight"]);
                  
                  sett(tpps);
                  fargspp = Rcpp::List::create(Named("lk") = k,
                                               Named("lt") = tpps,
                                               Named("ln") = n);
                  flist  = func(fargspp);
                  funcpp = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
                  weight = Rcpp::as<int>(Rcpp::as<List>(flist)["weight"]);
                  
                  exadd(-1 * weight * funcmm,accum);
                  exadd( weight * funcmp,accum);
                  exadd( weight * funcpm,accum);
                  exadd(-1 * weight * funcpp,accum);
                  xmmlll = xmmlll + weight * funcmm;
                  xmplll = xmplll + weight * funcmp;
                  xpmlll = xpmlll + weight * funcpm;
                  xpplll = xpplll + weight * funcpp;
   
           }
           
           v.at(i - 1,j - 1) = (accum.at(0) + accum.at(1)) / d12;
           
           if(debug::kprint >= 4){
             
              Rcpp::Rcout << "\nHMAT**4**+ TOP\n" << std::endl;
              Rcpp::Rcout << "     i = " << i - 1 << std::endl;
              Rcpp::Rcout << "     j = " << j - 1 << std::endl;
              Rcpp::Rcout << "v(i,j) = " << v.at(i - 1,j - 1) << std::endl;
              Rcpp::Rcout << "   d12 = " << d12 << std::endl;
              Rcpp::Rcout << "weight = " << weight << std::endl;
              Rcpp::Rcout << "xpplll = " << xpplll << std::endl;
              Rcpp::Rcout << "xmplll = " << xmplll << std::endl;
              Rcpp::Rcout << "xpmlll = " << xpmlll << std::endl;
              Rcpp::Rcout << "xmmlll = " << xmmlll << std::endl;
              Rcpp::Rcout << " accum = " << accum << std::endl;
              Rcpp::Rcout << "funcpp = " << funcpp << std::endl;
              Rcpp::Rcout << "funcmp = " << funcmp << std::endl;
              Rcpp::Rcout << "funcpm = " << funcpm << std::endl;
              Rcpp::Rcout << "funcmm = " << funcmm << std::endl;
             
           }     
           
           goto line126;
           
           line125: fargsmm = Rcpp::List::create(Named("lt") = tmm,
                                                 Named("ln") = n);
                    flist   = func(fargsmm);
                    foutmm  = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
        
                    fargsmp = Rcpp::List::create(Named("lt") = tmp,
                                                 Named("ln") = n);
                    flist   = func(fargsmp);
                    foutmp  = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
        
                    fargspm = Rcpp::List::create(Named("lt") = tpm,
                                                 Named("ln") = n);
                    flist   = func(fargspm);
                    foutpm  = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
        
                    fargspp = Rcpp::List::create(Named("lt") = tpp,
                                                 Named("ln") = n);
                    flist   = func(fargspp);
                    foutpp  = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
        
                    v.at(i - 1,j - 1) = -1 * (foutmm - foutmp - foutpm + foutpp) / d12;
                    
           line126: if(debug::kprint >= 4){
             
                       Rcpp::Rcout << "\nHMAT1**4** TOP\n" << std::endl;
                       Rcpp::Rcout << "i = " << i - 1 << std::endl;
                       Rcpp::Rcout << "j = " << j - 1 << std::endl;
                       Rcpp::Rcout << "v(i,j) = " << v.at(i - 1,j - 1) << std::endl;
                       Rcpp::Rcout << "npoint = " << npoint << std::endl;
                       Rcpp::Rcout << "deltai = " << deltai << std::endl;
                       Rcpp::Rcout << "deltaj = " << deltaj << std::endl;
                       Rcpp::Rcout << "thetai = " << thetai << std::endl;
                       Rcpp::Rcout << "thetaj = " << thetaj << std::endl;
                       Rcpp::Rcout << "foutpp = " << foutpp << std::endl;
                       Rcpp::Rcout << "foutmp = " << foutmp << std::endl;
                       Rcpp::Rcout << "foutpm = " << foutpm << std::endl;
                       Rcpp::Rcout << "foutmm = " << foutmm << std::endl;
             
                    }
           
           tmm.at(i - 1) = thetai;
           tmp.at(i - 1) = thetai;
           tpm.at(i - 1) = thetai;
           tpp.at(i - 1) = thetai;
           v.at(j - 1,i - 1) = v.at(i - 1,j - 1);
   
    }
    
    tmp.at(j - 1) = thetaj;
    tpm.at(j - 1) = thetaj;
    
    line500: v.at(j - 1,j - 1) = zero;
    
             if(deltaj < small) goto line1126;
             
             deltaj = two * delta.at(j - 1);
             d12 = deltaj * deltaj;
             tmm.at(j - 1) = thetaj - deltaj;
             tpp.at(j - 1) = thetaj + deltaj;
             
             if(npoint == 0) goto line1125;
             
             accum.at(0) = zero;
             accum.at(1) = zero;
             xmmlll = zero;
             xpplll = zero;
             gett(tmm,tmms);
             gett(tpp,tpps);
             
             for(int k = 1; k <= npoint; k++){
                
                 sett(tmms);
                 fargsmm = Rcpp::List::create(Named("lk") = k,
                                              Named("lt") = tmms,
                                              Named("ln") = n);
                 flist  = func(fargsmm);
                 fout   = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
                 weight = Rcpp::as<int>(Rcpp::as<List>(flist)["weight"]);
                 
                 funcmm = fout;
                 
                 sett(tpps);
                 fargspp = Rcpp::List::create(Named("lk") = k,
                                              Named("lt") = tpps,
                                              Named("ln") = n);
                 flist  = func(fargspp);
                 fout   = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
                 weight = Rcpp::as<int>(Rcpp::as<List>(flist)["weight"]);
                 
                 
                 funcpp = fout;
                 exadd(-weight * funcmm,accum);
                 exadd(-weight * funcpp,accum);
                 exadd( weight * tfunv.at(k - 1),accum);
                 xmmlll = xmmlll + weight * funcmm;
                 xpplll = xpplll + weight * funcpp;
                
             }
             
      v.at(j - 1,j - 1) = (accum.at(0) + accum.at(1)) / d12;
             
      if(debug::kprint >= 4){
            
         Rcpp::Rcout << "\nHMAT**4**+ BOTTOM\n" << std::endl;
         Rcpp::Rcout << "j = " << j - 1 << std::endl;
         Rcpp::Rcout << "v(j,j) = " << v.at(j - 1,j - 1) << std::endl;
         Rcpp::Rcout << "d12 = " << d12 << std::endl;
         Rcpp::Rcout << "xpplll = " << xpplll << std::endl;
         Rcpp::Rcout << "xflll = " << xflll << std::endl;
         Rcpp::Rcout << "xmmlll = " << xmmlll << std::endl;
         Rcpp::Rcout << "accum = " << accum << std::endl;
         Rcpp::Rcout << "funcpp = " << funcpp << std::endl;
         Rcpp::Rcout << "funcmp = " << funcmp << std::endl;
         Rcpp::Rcout << "funcpm = " << funcpm << std::endl;
         Rcpp::Rcout << "funcmm = " << funcmm << std::endl;
             
      }     

      goto line1126;
      
      line1125: fargsmm = Rcpp::List::create(Named("lt") = tmm,
                                             Named("ln") = n);
                flist   = func(fargsmm);
                foutmm  = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
                
                fargspp = Rcpp::List::create(Named("lt") = tpp,
                                             Named("ln") = n);
                flist   = func(fargspp);
                foutpp  = Rcpp::as<double>(Rcpp::as<List>(flist)["val"]);
          
                v.at(j - 1,j - 1) = -1 * (foutmm - tfuns + foutpp) / d12;
      
      line1126: if(debug::kprint >= 4){
             
                   Rcpp::Rcout << "\nHMAT1**4** TOP\n" << std::endl;
                   Rcpp::Rcout << "j = " << j - 1 << std::endl;
                   Rcpp::Rcout << "v(j,j) = " << v.at(j - 1,j - 1) << std::endl;
                   Rcpp::Rcout << "npoint = " << npoint << std::endl;
                   Rcpp::Rcout << "deltaj = " << deltaj << std::endl;
                   Rcpp::Rcout << "thetaj = " << thetaj << std::endl;
                   Rcpp::Rcout << "foutpp = " << foutpp << std::endl;
                   Rcpp::Rcout << "foutmm = " << foutmm << std::endl;
             
                }
      
                tmm.at(j - 1) = thetaj;
                tpp.at(j - 1) = thetaj;
   
}

return;

}