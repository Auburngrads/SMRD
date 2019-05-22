#include <base/base.hpp>
#include <wqmmlesss/wqm_rdesc.hpp>
#include <wqmmlesss/wqm_put.hpp>
#include <wqmmlesss/wqm_flike.hpp>
#include <wqmmlesss/wqm_piter.hpp>
#include <utility/dsign.hpp>
#include <utility/threecheck.hpp>

using namespace lstd;

inline bool logically_equal(float a, float b, float error_factor=1.0)
{
  return a==b ||
    std::abs(a-b)<std::abs(std::min(a,b))*std::numeric_limits<float>::epsilon()*error_factor;
}


int powsss(int test,
            std::string line,
            std::string str, 
            double val) {
  
     if(debug::kprint >= 10.0) {
       
        test = test + 1;
        Rcpp::Rcout << line << std::endl;
        Rcpp::Rcout << str << val << std::endl;
     
     }
     
     return test;
  
}

//' powell algorithim for minimizing a function of n variables;
//' without the use of derivatives. From Wayne Nelson 1973.;
//' code has been modified to eliminate side - step problems;

void wqm_powsss(Rcpp::NumericVector &thetg,
                Rcpp::NumericVector &e,
                int &nparm,
                double &upcen,
                double &xlike,
                double &escale,
                int &icon,
                bool &ltpp,
                int &maxit,
                Rcpp::NumericVector &diag,
                Rcpp::NumericMatrix &tmat,
                Rcpp::NumericVector &thetb,
                Rcpp::NumericVector &thetad,
                Rcpp::NumericVector &thetaf,
                Rcpp::LogicalVector &lfix,
                Rcpp::NumericVector &ed,
                Rcpp::NumericMatrix &xnew,
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
                Rcpp::NumericVector &w,
                int &iwdim,
                int &ier){
  
double pz3 = 0.03,pz5 = 0.05,p1 = 0.1;
Rcpp::List FL1, FL2, FL3, FL4;
Rcpp::LogicalVector CHK;

// Constants for poweld restart
double big = 1.0e30;
double ybar = 0.0, ysd = 0.0;
double epsilon = 1.0e-40;
double dscale = escale;
double fsavq, ddmag, scer;
double f, fkeep;
double fp, sum, dmax;
double dacc, dmag, ddmax;
double dl, d, fprev;
double fa, da, fb = 0.0e00;
double db = 0.0e00, dd, dc = 0.0e00;
double di, fi, a;
double b,  fc = 0.0e00, fhold = 0.0e00;
double aaa = 0.0e00;
int ntry = 1, n = 0, nfcc;
int idirn, iline, ind, inn, itone, iterc, isgrad;
int j, jj, jjj, jtl = 0, k;
int ixp = 0, is, maxtry = 100;
int test = 0, test_max = 1000;

ier = 0;

// Get standard dev of y for scaling
   wqm_rdesc(y,wt,nrow,ybar,ysd);

   
for(int i = 1; i <= nparm; i++){ //2222

    thetaf.at(i - 1) = thetg.at(i - 1);
    ed.at(i - 1) = e.at(i - 1) * ysd;
    if(lfix.at(i - 1)) continue;
    n = n + 1; 
    thetad.at(n - 1) = thetg.at(i - 1);

}    

if(n == 0) goto line997;
if(!lfix.at(nparm - 1)) ed.at(n - 1) = e.at(nparm - 1);

// #poweld restart here;
line1001: lstd::g_ltp = ltpp;
          fsavq = 1.0e35;
          ddmag = p1 * dscale;
          scer = pz5 / dscale;
          jj = n * n + n;
          jjj = jj + n;
          k = n + 1;
          nfcc = 1;
          ind = 1;
          inn = 1;
          
for(int i = 1; i <= n; i++){
  
    for(int j = 1; j <= n; j++){
      
        w.at(k - 1) = 0.0;
      
        if(i == j){
          
            w.at(k - 1) = std::abs(ed.at(i - 1));
            w.at(i - 1) = dscale;
            
        }
          
           k = k + 1;
           
    }
}

iterc = 1;
isgrad = 2;
thetaf = wqm_put(thetaf,thetad,thetg,lfix,nparm);

FL1 = wqm_flike(y,xnew,cen,wt,nty,ty,tcodes,nrow,nter,ny,
                  diag,tmat,thetb,kdist,thetaf,nparm,upcen);

f     = Rcpp::as<double>(Rcpp::as<List>(FL1)["flike"]);
//thetb = Rcpp::as<Rcpp::NumericVector>(Rcpp::as<List>(FLIKE)["thetb"]);

fkeep = std::abs(f) + std::abs(f);

line5: itone = 1;
       fp = f;
       sum = 0.0;
       ixp = jj;

for(int i = 1; i <= n; i++){
  
    ixp = ixp + 1;
    w.at(ixp - 1) = thetad.at(i - 1);

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

line8: dd = d - dl;
       dl = d;

line58: k = idirn;

for(int i = 1; i <= n; i++){

    thetad.at(i - 1) = thetad.at(i - 1) + dd * w.at(k - 1);
    k = k + 1;

}

thetaf = wqm_put(thetaf,thetad,thetg,lfix,nparm);
FL2 = wqm_flike(y,xnew,cen,wt,nty,ty,tcodes,nrow,nter,ny,
                  diag,tmat,thetb,kdist,thetaf,nparm,upcen);

f     = Rcpp::as<double>(Rcpp::as<List>(FL2)["flike"]);
//thetb = Rcpp::as<Rcpp::NumericVector>(Rcpp::as<List>(FLIKE)["thetb"]);


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
        if(std::abs(d) > dmax) { ier = 1; goto line20; }

line17: d = d + d;
        goto line8;

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


        if(std::abs((da - d) * (d - db)) < epsilon) goto line25;
        if(((da - d) * (d - db)) > 0.0) goto line8;

line25: is = 1;

        if(std::abs(std::abs(d - db) - ddmax) < epsilon) goto line8;
        if(std::abs(d - db) < ddmax) goto line8;

line26: d = db + dsign(ddmax,(db - da));
        is = 1;
        ddmax = ddmax + ddmax;
        ddmag = ddmag + ddmag;
        // #trap runaway problem and try restart;
        if((std::abs(ddmag) < big) and (std::abs(ddmax) < big)) goto line265;
        // #check number of restarts;
        if(ntry > maxtry) goto line995;
        // #try again;
        ntry = ntry + 1;
        dscale = dscale * 4.0; //factor
        goto line1001;

line265: if(std::abs(ddmax - dmax) < epsilon) goto line8;
         if(ddmax < dmax) goto line8;

         ddmax = dmax;
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

        fa = f;
        da = d;
        goto line30;

line11: if(std::abs(f - fb) < epsilon) goto line10;
        if(f > fb) goto line10;

        fa = fb;
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

        di = dc;
        fi = fc;

line44: if(itone == 1) goto line86;
        if(itone == 2) goto line86;
        if(itone == 3) goto line85;

line85: itone = 2;
        goto line45;

line86: if(std::abs(std::abs(d - di) - dacc) < epsilon) goto line41;
        if(std::abs(d - di) < dacc) goto line41;

        if(std::abs(std::abs(d - di) - (pz3 * std::abs(d))) < epsilon) goto line41;
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

        is = 3;
        goto line8;

line41: f = fi;
        d = di - dl;
        dd = std::sqrt(std::abs((dc - db) * (dc - da) * (da - db) / (a + b)));
        // #fit to keep from dividing by 0 when dd ends up being 0;
        if(!(dd > 0.0)) dd = 1.0e-10;

for(int i = 1; i <= n; i++){

    thetad.at(i - 1) = thetad.at(i - 1) + d * w.at(idirn - 1);
    w.at(idirn - 1) = dd * w.at(idirn - 1);
    idirn = idirn + 1;

}

w.at(iline - 1) = w.at(iline - 1) / dd;
iline = iline + 1;

 if(debug::kprint >= 1) {

     wqm_piter(f,thetg,thetad,thetaf,
               lfix,nparm,upcen,diag,tmat,thetb,
               nrow,nter,iterc,nfcc);

 }

if(itone == 1) goto line55;
if(itone == 2) goto line38;

line55: if(std::abs((fprev - f) - sum) < epsilon) goto line95;
        if((fprev - f) < sum) goto line94;

line95: sum = fprev - f;
        jtl = iline;

line94: if(idirn <= jj) goto line7;

        if(ind == 1) goto line92;
        if(ind == 2) goto line53;

line92: fhold = f;
        is = 6;
        ixp = jj;

for(int i = 1; i <= n; i++){

    ixp = ixp + 1;
    w.at(ixp - 1) = thetad.at(i - 1) - w.at(ixp - 1);

}

dd = 1.0;
goto line58;

line96: if(ind == 1) goto line112;
        if(ind == 2) goto line87;

line112: if(std::abs(fp - f) < epsilon) goto line37;
         if(fp < f) goto line37;

        d = 2 * (fp + f - 2 * fhold) / std::pow((fp - f),2);
        if(std::abs((d * std::pow((fp - fhold - sum),2)) - sum) < epsilon) goto line37;
        if(((d * std::pow((fp - fhold - sum),2)) - sum) > 0.0) goto line37;

line87: j = jtl * n + 1;
        if(j > jj) goto line61;

        for(int i = j; i <= jj; i++){

            k = i - n;
            w.at(k - 1) = w.at(i - 1);

        }

for(int i = jtl; i <= n; i++){

    w.at(i - 2) = w.at(i - 1);

}

line61: idirn = idirn - n;
        itone = 3;
        k = idirn;
        ixp = jj;
        aaa = 0.0;

for(int i = 1; i <= n; i++){

    ixp = ixp + 1;
    w.at(k - 1) = w.at(ixp - 1);
    if(std::abs(aaa - std::abs(w.at(k - 1) / ed.at(i - 1))) < epsilon) goto line67;
    if(aaa > std::abs(w.at(k - 1) / ed.at(i - 1))) goto line67;

    aaa = std::abs(w.at(k - 1) / ed.at(i - 1));

    line67: k = k + 1;

}

ddmag = 1;
// seemed to have a aaa = 0 here; patch it;
aaa = std::max(aaa,1.0e-10);
w.at(n - 1) = dscale / aaa;
iline = n;
goto line7;

line37: ixp = jj;
        aaa = 0.0;
        f = fhold;

for(int i = 1; i <= n; i++){

    ixp = ixp + 1;
    thetad.at(i - 1) = thetad.at(i - 1) - w.at(ixp - 1);
    if(std::abs((aaa * std::abs(ed.at(i - 1))) - std::abs(w.at(ixp - 1))) < epsilon) continue;
    if((aaa * std::abs(ed.at(i - 1))) > std::abs(w.at(ixp - 1))) continue;

    aaa = std::abs(w.at(ixp - 1) / ed.at(i - 1));

}

goto line53;

line38: aaa = aaa * (1.0 + di);
        if(ind == 1) goto line53;
        if(ind == 2) goto line106;

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
    w.at(k - 1) = thetad.at(i - 1);
    thetad.at(i - 1) = thetad.at(i - 1) + 10.0 * ed.at(i - 1);

}

fkeep = f;
thetaf = wqm_put(thetaf,thetad,thetg,lfix,nparm);
FL3 = wqm_flike(y,xnew,cen,wt,nty,ty,tcodes,nrow,nter,ny,
                  diag,tmat,thetb,kdist,thetaf,nparm,upcen);

f     = Rcpp::as<double>(Rcpp::as<List>(FL3)["flike"]);
//thetb = Rcpp::as<Rcpp::NumericVector>(Rcpp::as<List>(FLIKE)["thetb"]);

nfcc = nfcc + 1;
ddmag = 0.0;
goto line108;

line76: if(std::abs(f - fp) < epsilon) goto line78;
        if(f < fp) goto line35;

line78: ier = 0;
        goto line20;

line88: ind = 1;

line35: ddmag = 4.0 * std::sqrt(fp - f);
        isgrad = 1;

line108: iterc = iterc + 1;
         // #check to see if the likelihood is changing slowly;
         if(!lstd::g_ltp) goto line2002;
         if(std::abs(f - fsavq) > 1) goto line2001;
         lstd::g_ltp = false;
         ier = 0;

line2001: fsavq = f;

line2002: if(iterc <= maxit) goto line5;

          ier = 2;
          if(std::abs(f - fkeep) < epsilon) goto line20;
          if(f < fkeep) goto line20;

          f = fkeep;

for(int i = 1; i <= n; i++){

    jjj = jjj + 1;
    thetad.at(i - 1) = w.at(jjj - 1);

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

line105: ixp = jj;

for(int i = 1; i <= n; i++){

    ixp = ixp + 1;
    k = ixp + n;
    if(jtl == 1) goto line114;
    if(jtl == 2) goto line115;
    line114: w.at(ixp - 1) = w.at(k - 1);
    continue;
    line115: w.at(ixp - 1) = thetad.at(i - 1);
    thetad.at(i - 1) = w.at(k - 1);

}

jtl = 2;
goto line92;

line106: if(std::abs(aaa - p1) < epsilon) goto line20;
         if(aaa > p1){

            inn = 1;
            goto line35;

           } else {

             goto line20;

           }
           
line997: FL4 = wqm_flike(y,xnew,cen,wt,nty,ty,tcodes,nrow,nter,ny,
                           diag,tmat,thetb,kdist,thetaf,nparm,upcen);
           
         f = Rcpp::as<double>(Rcpp::as<List>(FL4)["flike"]);
         //thetb = Rcpp::as<Rcpp::NumericVector>(Rcpp::as<List>(FLIKE)["thetb"]);

goto line20;

line995: ier = 3;

line20: thetaf = wqm_put(thetaf,thetad,thetg,lfix,nparm); 

for(int i = 1; i <= nparm; i++){

    thetg.at(i - 1) = thetaf.at(i - 1);

}

xlike = -f;

return;

}