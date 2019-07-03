#include <base/base.hpp>
#include <wqmmlesss/wqm_dpivot.hpp>

//' @description Invert an ntcxntc matrix (ntc < idim). The method 
//'              of double pivoting is used. \code{ir} and \code{jc} 
//'              are scratch arrays of size \code{idim}. \code{irank} 
//'              returns the rank of the matrix which will equal \code{ntc}
//'              if the matrix is nonsingular. \code{xtol} is a tolerance 
//'              level and should be set to 1.0e-8 for 32 or 36 bit words 
//'              (and smaller for larger words).

void dinvx(Rcpp::NumericMatrix &a,
           int &ntc,
           double xtol,
           Rcpp::IntegerVector &ir,
           Rcpp::IntegerVector &jc,
           int &irank,
           int &idim){
  
int ix = 0,imax = 0,jmax = 0,it,jx;
int nr,is,ntp1;
double temp,tol,tx,ty,t;

nr = ntc;
is = 0;

ntp1 = ntc + 1;
irank = 0;

for(int i = 1; i <= nr; i++){

    jc.at(i - 1) = i;
    ir.at(i - 1) = i;

}

line20: temp = zero;

if(is == nr) goto line500;

is = is + 1;

for(int i = is; i <= nr; i++){

    for(int j = is; j <= nr; j++){

        if(std::abs(a.at(i - 1,j - 1)) > temp) {

           imax = i;
           jmax = j;
           temp = std::fabs(a.at(i - 1,j - 1));
           
        }
    }
}

if(ix == 0) tol = xtol * temp;

if(debug::kprint >= 9) {

   Rcpp::Rcout << "\nDINVX**1**\n"   << std::endl;
   Rcpp::Rcout << "imax = " << imax  << std::endl;
   Rcpp::Rcout << "jmax = " << jmax  << std::endl;
   Rcpp::Rcout << "  jc = "   << ir    << std::endl;
   Rcpp::Rcout << "  ir = "   << jc    << std::endl;
   Rcpp::Rcout << "temp = " << temp  << std::endl;
   Rcpp::Rcout << " tol = "  << tol   << std::endl;
   Rcpp::Rcout << "  nr = "   << nr    << std::endl;
   Rcpp::Rcout << "ntp1 = " << ntp1  << std::endl;

}

ix = 1;

if(temp <= tol) goto line610;

for(int j = 1; j <= ntc; j++){

    tx = a.at(is - 1,j - 1);
    a.at(is - 1,j - 1) = a.at(imax - 1,j - 1);
    a.at(imax - 1,j - 1) = tx;

}

if(debug::kprint >= 9) {

   Rcpp::Rcout << "\nDINVX**2**\n"   << std::endl;
   Rcpp::Rcout << "vcv = " << a  << std::endl;

}

for(int i = 1; i <= nr; i++){

    ty = a.at(i - 1,is - 1);
    a.at(i - 1,is - 1) = a.at(i - 1,jmax - 1);
    a.at(i - 1,jmax - 1) = ty;

}

if(debug::kprint >= 9) {

   Rcpp::Rcout << "\nDINVX**3**\n"   << std::endl;
   Rcpp::Rcout << "vcv = \n" << a      << std::endl;

}

it = ir.at(imax - 1);
ir.at(imax - 1) = ir.at(is - 1);
ir.at(is - 1) = it;
it = jc.at(jmax - 1);
jc.at(jmax - 1) = jc.at(is - 1);
jc.at(is - 1) = it;

if(debug::kprint >= 9) {

   Rcpp::Rcout << "\nDINVX**4**\n"   << std::endl;
   Rcpp::Rcout << "jc = "   << ir    << std::endl;
   Rcpp::Rcout << "ir = "   << jc    << std::endl;

}

for(int i = 1; i <= nr; i++){

    a.at(i - 1,ntp1 - 1) = zero;

}

a.at(is - 1,ntp1 - 1) = one;

if(debug::kprint >= 9) {

   Rcpp::Rcout << "\nDINVX**4**\n"   << std::endl;
   Rcpp::Rcout << "vcv = \n" << a      << std::endl;

}

wqm_dpivot(a,is,nr,ntp1,idim);

if(debug::kprint >= 9) {

   Rcpp::Rcout << "\nDINVX**5**\n"   << std::endl;
   Rcpp::Rcout << "vcv = \n" << a      << std::endl;

}

for(int i = 1; i <= nr; i++){

    a.at(i - 1,is - 1) = a.at(i - 1,ntp1 - 1);

}

goto line20;

line500: if(is == nr) irank = nr;

line600: if(irank == 0) return;

goto line700;

line610: irank = is - 1;

goto line600;

line700: if(irank != nr) return;

for(int i = 1; i <= nr; i++){

    for(int iq = 1; iq <= nr; iq++){

        if(jc.at(iq - 1) != i) continue;
        ix = jc.at(iq - 1);
        jc.at(iq - 1) = jc.at(i - 1);
        jc.at(i - 1) = ix;

        for(int k = 1; k <= ntc; k++){

            t = a.at(iq - 1,k - 1);
            a.at(iq - 1,k - 1) = a.at(i - 1,k - 1);
            a.at(i - 1,k - 1) = t;

        }
    }
}

if(debug::kprint >= 9) {

   Rcpp::Rcout << "\nDINVX**6**\n"   << std::endl;
   Rcpp::Rcout << "vcv = \n" << a      << std::endl;

}

for(int j = 1; j <= nr; j++){

    for(int jq = 1; jq <= nr; jq++){

        if(ir.at(jq - 1) != j) continue;
        jx = ir.at(jq - 1);
        ir.at(jq - 1) = ir.at(j - 1);
        ir.at(j - 1) = jx;

        for(int k = 1; k <= nr; k++){

            t = a.at(k - 1,jq - 1);
            a.at(k - 1,jq - 1) = a.at(k - 1,j - 1);
            a.at(k - 1,j - 1) = t;

        }
    }
}

if(debug::kprint >= 9) {

   Rcpp::Rcout << "\nDINVX**7**\n" << std::endl;
   Rcpp::Rcout << "vcv = \n" << a  << std::endl;
   Rcpp::Rcout << " ir = "   << ir  << std::endl;
   Rcpp::Rcout << " jc = "   << jc  << std::endl;

}

return;

}
