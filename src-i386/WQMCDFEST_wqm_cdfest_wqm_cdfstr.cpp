#include <base/base.hpp>
#include <utility/eps.hpp> // done
#include <utility/wqm_sortd.hpp>

//' @description Function to find the minimum data structure
//'              needed for the generalized k-m or turnbull
//'              algorithm

void wqm_cdfstr(Rcpp::NumericMatrix &y,
                int &ny,
                Rcpp::NumericVector &ys,
                Rcpp::NumericMatrix &ty,
                int &nty,
                Rcpp::IntegerVector &tcodes,
                Rcpp::IntegerVector &iorder,
                Rcpp::IntegerVector &ilcv,
                Rcpp::IntegerVector &iucv,
                Rcpp::IntegerVector &codes,
                Rcpp::IntegerVector &weight,
                int &n,
                Rcpp::NumericVector &p,
                Rcpp::NumericVector &q,
                int &m){

bool ll;
double ysip,ysi,ilcvi,iucvi;
int ittype, itype;
int n2 = 2 * n, ip;
int index, ilmark = 0;
// 2 mar 2004; bringing down the truncation info;
// to handle right/right and left/left;
// break ties according to the following conventions;

// # type x mapped to the interval endpoint ( or );

// # 1 failure (x);
// # 2 right censored x (----->;
// # 3 left censored <-----) x;
// # 4l lower int. endpoint x (----->;
// # 4u upper int. endpoint <-----) x;

for(int i = 1; i <= n; i++) {

    ip = i + n;
  
    itype = codes.at(i - 1);

    if(nty > 0) {

       ittype = tcodes.at(i - 1);

    } else {

       ittype = 1;

  }

double y1 = y.at(i - 1,0);
double y2 = y.at(i - 1,ny - 1);

// set up end-point vector extending censored obs
// to +-infinity and break ties
int iweig = weight.at(i - 1);

// skip dummy observations;
if((itype <= 0) or (itype > 5) or (iweig == 0)) {

  ys.at(i - 1)  = big31;
  ys.at(ip - 1) = big31;
  continue;

}

if((itype == 1) or (itype == 5)) {

  ys.at(i - 1)  = y1 - eps(y1, 0.00001);
  ys.at(ip - 1) = y2 + eps(y2, 0.00001);
  continue;

}

if(itype == 2) {

  ys.at(i - 1)  = y1 + eps(y1, 0.00002);
  ys.at(ip - 1) = big30;

  // right censoring and right or interval truncation
  if((ittype == 2) or (ittype == 4)) {

    ys.at(ip - 1) = ty.at(i - 1,ny - 1) - eps(y2, 0.00001);

  }
  
  continue;
  
}

if(itype == 3) {

  ys.at(i - 1)  = -1 * big30;
  ys.at(ip - 1) = y2 - eps(y2, 0.00002);

  // left censoring and left or interval truncation;

  if((ittype == 3) or (ittype == 4)) {

    ys.at(i - 1) = ty.at(i - 1,0) + eps(y2, 0.00001);

  }
  
  continue;
  
}

if(itype == 4) {

  ys.at(i - 1)  = y1 + eps(y1, 0.00003);
  ys.at(ip - 1) = y2 - eps(y2, 0.00003);
  
  continue;
  
}
}

// find the permutation vector to order the concatinated;
// upper and lower time limits;
   wqm_sortd(ys, n2, iorder);

// ll in reduce;
// t looking for upper;
// f looking for lower;
ll = false;
m = 0;

// go through observations, smallest to largest,
// to identify the p-q intervals;

for(int i = 1; i <= n2; i++){

    index = iorder.at(i - 1);

// skip dummy observations marked with big31
   if(ys.at(index - 1) >= big31) continue;

   if(debug::kprint >= 10){
     
      Rcpp::Rcout << "\nReduce\n"    << std::endl;
      Rcpp::Rcout << "i = "          << i - 1            << std::endl;
      Rcpp::Rcout << "index = "      << index - 1    << std::endl;
      Rcpp::Rcout << "ys(index) = "  << ys.at(index - 1) << std::endl;
      Rcpp::Rcout << "ll = "         << ll           << std::endl;
      Rcpp::Rcout << "n = "          << n            << std::endl;
     
   } 

if(ll) goto line125;

// looking for a new lower limit, skip if upper is found first
   if(index > n) continue;

// Just found a lower limit
   ll = true;
   ilmark = index;
   continue;

// We already have a lower limit; check current to see if lower
   line125: if(index <= n) goto line127;

   // new upper limit completes new q-p pair
      m = m + 1;
      p.at(m - 1) = ys.at(ilmark - 1);
      q.at(m - 1) = ys.at(index - 1);

   // save indces of actual p,q limits
      ilcv.at(m - 1) = ilmark;
      iucv.at(m - 1) = index - n;
   
   if(debug::kprint > 3){
     
      Rcpp::Rcout << "\np - q\n"    << std::endl;
      Rcpp::Rcout << "m = "          << m            << std::endl;
      Rcpp::Rcout << "ilmark = " << ilmark - 1 << std::endl;
      Rcpp::Rcout << "index = " << index - 1 << std::endl;
      Rcpp::Rcout << "p(m) = " << p.at(m - 1) << std::endl;
      Rcpp::Rcout << "q(m) = " << q.at(m - 1) << std::endl;
     
   }         
   
   //  start search for new lower limit
   ll = false;
   continue;


line127: ilmark = index;

}

// store the indicies needed later for restoration of endpoints

for(int j = 1; j <= m; j++){

    iorder.at(j - 1)     = ilcv.at(j - 1);
    iorder.at(j - 1 + m) = iucv.at(j - 1);

}

// find the limits defining the p-q intervals within
// each censoring interval (ai sets)

for(int i = 1; i <= n; i++){

    ip = i + n;
    ysi = ys.at(i - 1);
    iucv.at(i - 1) = 0;
    ilcv.at(i - 1) = 0;

   // skip over dummy obserations

   if(ysi >= big31) continue;

   ysip = ys.at(ip - 1);
     ll = false;

for(int j = 1; j <= m; j++){

    if(!ll) {

       if(p.at(j - 1) < ysi) continue;

       ilcv.at(i - 1) = j;
       ll = true;
       continue;

    }

    if(q.at(j - 1) <= ysip) continue;
    iucv.at(i - 1) = j - 1;
    goto line32;

}

  iucv.at(i - 1) = m;
  line32: ilcvi = ilcv.at(i - 1);
          iucvi = iucv.at(i - 1);

  
  if(debug::kprint >= 10){
    
     Rcpp::Rcout << "\nfind ilcv,iucv\n"    << std::endl;
     Rcpp::Rcout << "ysi = "      << ysi         << std::endl;
     Rcpp::Rcout << "ysip = "     << ysip        << std::endl;
     Rcpp::Rcout << "ilcv(i) = "  << ilcv.at(i - 1)  << std::endl;
     Rcpp::Rcout << "iucv(i) = "  << iucv.at(i - 1)  << std::endl;
     Rcpp::Rcout << "pcen = " << p.at(ilcvi - 1) << std::endl;
     Rcpp::Rcout << "qcen = " << q.at(iucvi - 1) << std::endl;
     
  }
         
}

return;

}
