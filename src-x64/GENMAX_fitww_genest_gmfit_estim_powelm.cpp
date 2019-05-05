#include <base/base.hpp>
#include <utility/vmin.hpp>
#include <utility/dsign.hpp>

//' powell algorthim for minimizing a function of n variables
//' without the use of derivatives.
//' iprpow=5 for likelihood iterations and 0 for no iterations
//' kprint   gives level of printing for diagnostics
//' the vector x(1) is of length>=n
//' it may be greater than n if there are fixed parameter values
//' stored in the high part of the vector and controlled
//' externally with common communication
//' modified to return error code       11 may 1987

void powelm(Rcpp::NumericVector &x,
            Rcpp::NumericVector &e,
            int &n,
            double &f,
            double &escale,
            int &iprpow,
            int &icon,
            int &maxit,
            double (*func)(Rcpp::NumericVector, int),
            Rcpp::NumericVector &w,
            int &ier){

double pz3 = 0.03e00,pz5 = 0.05e00,p1 = 0.1e00;
double big = 1.0e30, epsilon = 1.0e-40;
double emin,dscale,aaa = 0.0e00,scer,a,b;
double sum,fkeep,fp,fa,fprev,fmval;
int maxtry = 5,iprxxx,ipline;
int j,jj,jjj,k,nfcc,inn,ind,iterc,isgrad,itone;
int krxp,is,jtl = 0,nm1,ntry,idirn,iline;
double dmax,dmag,ddmax,ddmag,dacc,d,dl,dd,ddhh,diff;
double db = 0.0e00,da = 0.0e00,dc = 0.0e00,di = 0.0e00;
double fc = 0.0e00,fi = 0.0e00,fb = 0.0e00,fhold = 0.0e00;


// find the minimum e value and use it to decide if we should print
// the noise warning
// dump to see if there is a w problem

if(debug::kprint >= 3){
  
   Rcpp::Rcout << "\nwlook = \n" << w << std::endl;
  
}

emin = vmin(e,n);
nfcc = 0;
dscale = escale;
ntry = 1;
dd = 1.0e32;

// powelm restart here
line1001:  nm1 = n - 1;
           iprxxx = iprpow;
           ipline = 0;
           if(iprpow >= 5) ipline = 1;
           if(iprpow >= 5) iprxxx = 1;
           ddmag = p1 * dscale;
           scer = pz5 / dscale;
           jj = n * n + n;
           jjj = jj + n;
           k = n + 1;
           nfcc = 1;
           ier = 0;
           ind = 1;
           inn = 1;
           
for(int i = 1; i <= n; i++){
  
    for(int j = 1; j <= n; j++){
      
        w.at(k - 1) = 0.0;
      
        if(i == j){
          
            w.at(k - 1) = std::abs(e.at(i - 1));
            w.at(i - 1) = dscale;
            
        }
          
           k = k + 1;
           
    }
}

       iterc = 1;
       isgrad = 2;
       f = func(x,n);
       fkeep = std::abs(f) + std::abs(f);
       
line5: itone = 1;
       fp = f;
       sum = 0.0;
       krxp = jj;

for(int i = 1; i <= n; i++){
  
    krxp = krxp + 1;
    w.at(krxp - 1) = x(i - 1);

}

idirn = n + 1;
iline = 1;

line7: dmax = w.at(iline - 1);
       dacc = dmax * scer;
       dmag = std::min(ddmag, p1 * dmax);
       dmag = std::max(dmag,20.0 * dacc);
       ddmax = 10.0 * dmag;

if(itone == 1) goto line70;
if(itone == 2) goto line70;
if(itone == 3) goto line71;

line70: dl = 0.0;
        d = dmag;
        fprev = f;
        is = 5;
        fa = f;
        da = dl;

line8: ddhh = dd;
       dd = d - dl;
       dl = d;

      if((is != 3) or (std::abs(ddhh + dd) > 1.0e-06)) goto line58;
      
//    dd=0.7d00*dd;
      diff = ddhh + dd;
      
      if(debug::kprint >= 3) {
        
         Rcpp::Rcout << " *****warning---dd changing sign in powell" << std::endl;
        
      }
      
line58: k = idirn;

for(int i = 1; i <= n; i++){

    x.at(i - 1) = x.at(i - 1) + dd * w.at(k - 1);
    k = k + 1;

}

f = func(x,n);

//     if(kprint.ge.6)write(6,4923)nfcc,is,itone,ind,inn,
//    &idirn,d,ddhh,dd,dmax
//     if(kprint.ge.6)write(6,4924)(w(iv+idirn-1),x(iv),iv=1,n)
//4924  format(1x,6d20.12)
//4923  format(' jjj',i4,5(1x,i2),4d20.12)

nfcc = nfcc + 1;

if(is == 1) goto line10;
if(is == 2) goto line11;
if(is == 3) goto line12;
if(is == 4) goto line13;
if(is == 5) goto line14;
if(is == 6) goto line96;

line14: if(std::abs(f - fa) < epsilon) goto line16;
        if(f < fa)       goto line15;
        if(f > fa)       goto line24;
        
line16: if(std::abs(std::abs(d) - dmax) < epsilon) goto line17;
        if(std::abs(d) > dmax) { ier = 1; goto line18; }
        
line17: d = d + d;
        goto line8;
        
line18: if(debug::kprint >= 2){
  
           Rcpp::Rcout << "0*****warning---maximum change does not alter function" << std::endl;
  
}

        ier = 2;
        goto line20;

line15: fb = f;
        db = d;
        goto line21;

line24: fb = fa;
        db = da;
        fa = f;
        da = d;

line21: if(isgrad == 1) goto line83;

line23: d = db + db - da;
        is = 1;
        goto line8;
        
line83: d = 0.5 * (da + db - (fa - fb) / (da - db));
        is = 4;

        if(std::abs((da - d) * (d - db)) < epsilon) goto line8;
        if(((da - d) * (d - db)) > 0.0) goto line8;
        
line25: is = 1;

        if(std::abs(std::abs(d - db) - ddmax) < epsilon) goto line8;
        if(std::abs(d - db) < ddmax) goto line8;
        
line26: d = db + dsign(ddmax,(db - da));
        is = 1;
        ddmax = ddmax + ddmax;
        ddmag = ddmag + ddmag;
        
        // Trap runaway problem and try restart
        if((std::abs(ddmag) < big) and (std::abs(ddmax) < big)) goto line265;
        
        // Check number of restarts
        if(ntry > maxtry) goto line993;
        
        // Try again
        Rcpp::Rcout << "\n*****warning--poweld restart number = " << ntry << std::endl;
        ntry = ntry + 1;
        dscale = dscale * 4.0; //factor
        goto line1001;
        
line265: if(std::abs(ddmax - dmax) < epsilon) goto line8;
         if(ddmax < dmax) goto line8;

line27: ddmax = dmax;
        goto line8;

line13: if(std::abs(f - fa) < epsilon) goto line23;
        if(f > fa) goto line23;

line28: fc = fb;
        dc = db;

line29: fb = f;
        db = d;
        goto line30;

line12: if(std::abs(f - fb) < epsilon) goto line28;
        if(f < fb) goto line28;

line31: fa = f;
        da = d;
        goto line30;

line11: if(std::abs(f - fb) < epsilon) goto line10;
        if(f > fb) goto line10;

line32: fa = fb;
        da = db;
        goto line29;

line71: dl = 1.0;
        ddmax = 5.0;
        fa = fp;
        da = -1.0;
        fb = fhold;
        db = 0.0;
        d = 1.0;

line10: fc = f;
        dc = d;

line30: a = (db - dc) * (fa - fc);
        b = (dc - da) * (fb - fc);

        if(std::abs((a + b) * (da - dc)) < epsilon) goto line33;
        if(((a + b) * (da - dc)) > 0.0) goto line34;

line33: fa = fb;
        da = db;
        fb = fc;
        db = dc;
        goto line26;

line34: d = 0.5 * (a * (db + dc) + b * (da + dc)) / (a + b);
        di = db;
        fi = fb;

        if(std::abs(fb - fc) < epsilon) goto line44;
        if(fb < fc) goto line44;

line43: di = dc;
        fi = fc;

line44: if(itone == 1) goto line86;
        if(itone == 2) goto line86;
        if(itone == 3) goto line85;

line85: itone = 2;
        goto line45;

line86: if(std::abs(std::abs(d - di) - dacc) < epsilon) goto line41;
        if(std::abs(d - di) < dacc) goto line41;

line93: if(std::abs(std::abs(d - di) - (pz3 * std::abs(d))) < epsilon) goto line41;
        if(std::abs(d - di) < (pz3 * std::abs(d))) goto line41;
        
line45: if(std::abs((da - dc) * (dc - d)) < epsilon) goto line46;
        if(((da - dc) * (dc - d)) < 0.0) goto line47;

line46: fa = fb;
        da = db;
        fb = fc;
        db = dc;
        goto line25;

line47: is = 2;
        if(std::abs((db - d) * (d - dc)) < epsilon) goto line8;
        if(((db - d) * (d - dc)) > 0.0) goto line8;

line48: is = 3;
        goto line8;

line41: f = fi;
        d = di - dl;
        dd = std::sqrt(std::abs((dc - db) * (dc - da) * (da - db) / (a + b)));
        
        // Fit to keep from dividing by 0 when dd ends up being 0;
        if(!(dd > 0.0)) dd = 1.0e-10;

        for(int i = 1; i <= n; i++){
        
            x.at(i - 1) = x.at(i - 1) + d * w.at(idirn - 1);
            w.at(idirn - 1) = dd * w.at(idirn - 1);
            idirn = idirn + 1;
        
        }

        w.at(iline - 1) = w.at(iline - 1) / dd;
        iline = iline + 1;

        if(iprxxx != 1) goto line51;
        
line50: if(ipline == 1) goto line556;
        if(debug::kprint > 0){
          
           Rcpp::Rcout << "\npoweld **50**\n" << std::endl;
           Rcpp::Rcout << "iterc = " << iterc << std::endl;
           Rcpp::Rcout << "nfcc = " << nfcc << std::endl;
           Rcpp::Rcout << "f = " << f << std::endl;
           Rcpp::Rcout << "x = " << x << std::endl;
          
        }
        
        goto line557;
        
line556: fmval = -f;
         // call powprn(iterc,nfcc,fmval,x,n);
         if(debug::kprint > 0){
          
           Rcpp::Rcout << "\npoweld **556**\n" << std::endl;
           Rcpp::Rcout << "iterc = " << iterc << std::endl;
           Rcpp::Rcout << "nfcc = " << nfcc << std::endl;
           Rcpp::Rcout << "f = " << f << std::endl;
           Rcpp::Rcout << "x = " << x << std::endl;
          
        }
         
line557: if(iprxxx == 1) goto line51;
         if(iprxxx == 2) goto line53;
         
line51:  if(itone == 1) goto line55;
         if(itone == 2) goto line38;

line55: if(std::abs((fprev - f) - sum) < epsilon) goto line95;
        if((fprev - f) < sum) goto line94;

line95: sum = fprev - f;
        jtl = iline;

line94: if(idirn <= jj) goto line7;

line84: if(ind == 1) goto line92;
        if(ind == 2) goto line72;

line92: fhold = f;
        is = 6;
        krxp = jj;

        for(int i = 1; i <= n; i++){
        
            krxp = krxp + 1;
            w.at(krxp - 1) = x.at(i - 1) - w.at(krxp - 1);
        
        }

        dd = 1.0;
        goto line58;

line96: if(ind == 1) goto line112;
        if(ind == 2) goto line87;

line112: if(std::abs(fp - f) < epsilon) goto line37;
         if(fp < f) goto line37;

line91: d = 2 * (fp + f - 2 * fhold) / std::pow((fp - f),2);

        if(std::abs((d * std::pow((fp - fhold - sum),2)) - sum) < epsilon) goto line37;
        if(((d * std::pow((fp - fhold - sum),2)) - sum) > 0.0) goto line37;

line87: j = jtl * n + 1;
        if(j > jj) goto line61;
        
line60: for(int i = j; i <= jj; i++){

            k = i - n;
            w.at(k - 1) = w.at(i - 1);

        }

        for(int i = jtl; i <= n; i++){
        
            w.at(i - 2) = w.at(i - 1);
        
        }

line61: idirn = idirn - n;
        itone = 3;
        k = idirn;
        krxp = jj;
        aaa = 0.0;

for(int i = 1; i <= n; i++){

    krxp = krxp + 1;
    w.at(k - 1) = w.at(krxp - 1);

    if(std::abs(aaa - std::abs(w.at(k - 1) / e.at(i - 1))) < epsilon) goto line67;
    if(aaa > std::abs(w.at(k - 1) / e.at(i - 1))) goto line67;

    line66: aaa = std::abs(w.at(k - 1) / e.at(i - 1));

    line67: k = k + 1;

}

    ddmag = 1;

if((aaa <= 0.0e00) and (debug::kprint >= 2)){
  
    Rcpp::Rcout << "\n ***pxxx-***error***\n" << std::endl;
    Rcpp::Rcout << "n = " << n << std::endl;
    Rcpp::Rcout << "k = " << k << std::endl;
    Rcpp::Rcout << "krxp = " << krxp << std::endl;
    Rcpp::Rcout << "w(k) = " << w.at(k - 1) << std::endl;
    Rcpp::Rcout << "e(n) = " << e.at(n - 1) << std::endl;
    Rcpp::Rcout << "aaa = " << aaa << std::endl;
  
}

    if(aaa <= 0.0e00) aaa = 1.0e-10;
    w.at(n - 1) = dscale / aaa;
    iline = n;
    goto line7;

line37: krxp = jj;
        aaa = 0.0;
        f = fhold;

for(int i = 1; i <= n; i++){

    krxp = krxp + 1;
    x.at(i - 1) = x.at(i - 1) - w.at(krxp - 1);
    if(std::abs((aaa * std::abs(e.at(i - 1))) - std::abs(w.at(krxp - 1))) < epsilon) continue;
    if((aaa * std::abs(e.at(i - 1))) > std::abs(w.at(krxp - 1))) continue;

    aaa = std::abs(w.at(krxp - 1) / e.at(i - 1));

}

goto line72;

line38: aaa = aaa * (1.0 + di);

        if(ind == 1) goto line72;
        if(ind == 2) goto line106;
        
line72: if(iprxxx >= 2) goto line50;

line53: if(ind == 1) goto line109;
        if(ind == 2) goto line88;
        
line109: if(std::abs(aaa - p1) < epsilon) goto line89;
         if(aaa > p1) goto line76;

line89: if(icon == 1) goto line20;
        if(icon == 2) goto line116;

line116: ind = 2;
         if(inn == 1) goto line100;
         if(inn == 2) goto line101;

line100: inn = 2;
         k = jjj;

for(int i = 1; i <= n; i++){

    k = k + 1;
    w.at(k - 1) = x.at(i - 1);
    x.at(i - 1) = x.at(i - 1) + 10.0 * e.at(i - 1);

}

fkeep = f;
f = func(x,n);

nfcc = nfcc + 1;
ddmag = 0.0;
goto line108;

line76: if(std::abs(f - fp) < epsilon) goto line78;
        if(f < fp) goto line35;
        
line78: if((emin > 1.0e-05) or (debug::kprint >= 3)) {
  
            Rcpp::warning("poweld: accuracy may be limited by noise in the log likelihood function");
  
        }

        goto line20;

line88: ind = 1;

line35: ddmag = 4.0 * std::sqrt(fp - f);
        isgrad = 1;
        
line108: iterc = iterc + 1;
         // check to see if the log likelihood is changing slowly
         if(iterc <= maxit) goto line5;
         
line81: Rcpp::Rcout << "maxit = " << maxit << std::endl;
        Rcpp::warning("poweld: max iterations completed without meeting specifications");
        if(std::abs(f - fkeep) < epsilon) goto line20;
        if(f < fkeep) goto line20;

line110: f = fkeep;

         for(int i = 1; i <= n; i++){
         
             jjj = jjj + 1;
             x.at(i - 1) = w.at(jjj - 1);
         
         }

         goto line20;

line101: jtl = 1;
         fp = fkeep;

         if(std::abs(f - fkeep) < epsilon) goto line78;
         if(f < fkeep)          goto line105;
         if(f > fkeep)          goto line104;

line104: jtl = 2;
         fp = f;
         f = fkeep;

line105: krxp = jj;

for(int i = 1; i <= n; i++){

    krxp = krxp + 1;
    k = krxp + n;
    if(jtl == 1) goto line114;
    if(jtl == 2) goto line115;
    line114: w.at(krxp - 1) = w.at(k - 1);
    continue;
    line115: w.at(krxp - 1) = x.at(i - 1);
    x.at(i - 1) = w.at(k - 1);

}

jtl = 2;
goto line92;

line106: if(std::abs(aaa - p1) < epsilon) goto line20;
         if(aaa > p1) goto line107;

line20:  return;

line107: inn = 1;
         goto line35;

line993: ier = 3;
         Rcpp::warning("poweld: warning---terminating because of too many restarts");
         
return;

}
