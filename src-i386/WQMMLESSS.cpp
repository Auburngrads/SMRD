#include <base/base.hpp>
#include <wqmmlesss/wqm_mlboth.hpp>

// [[Rcpp::export]]
Rcpp::List WQMMLESSS(Rcpp::IntegerVector ivec,
                     Rcpp::NumericVector rvec,
                     int nrow,
                     int nparm,
                     Rcpp::NumericMatrix x,
                     Rcpp::NumericMatrix y,
                     Rcpp::IntegerVector cen,
                     Rcpp::IntegerVector wt,
                     Rcpp::NumericVector msftgm,
                     Rcpp::NumericMatrix ty,
                     Rcpp::IntegerVector tcodes,
                     Rcpp::LogicalVector lfix,
                     Rcpp::NumericVector e,
                     Rcpp::NumericVector dscrat,
                     Rcpp::IntegerVector iscrat,
                     Rcpp::NumericVector theta,
                     Rcpp::NumericVector fsder,
                     Rcpp::NumericMatrix vcv,
                     Rcpp::NumericMatrix r,
                     Rcpp::NumericMatrix res,
                     Rcpp::NumericVector fv,
                     Rcpp::NumericMatrix dev,
                     Rcpp::NumericMatrix ipxnew,
                     Rcpp::NumericVector iprv1,
                     Rcpp::NumericVector ipdiag,
                     Rcpp::NumericMatrix iptmat,
                     Rcpp::NumericVector ipthb,
                     Rcpp::NumericVector ipthg,
                     Rcpp::NumericVector ipfsd,
                     Rcpp::NumericMatrix ipvcvb,
                     Rcpp::NumericMatrix ipvcvg,
                     Rcpp::NumericVector ipnext,
                     Rcpp::NumericVector itd,
                     Rcpp::NumericVector itf,
                     Rcpp::NumericVector ied,
                     Rcpp::NumericVector iw,
                     Rcpp::NumericVector ivd,
                     Rcpp::NumericMatrix ivcvd,
                     Rcpp::NumericMatrix ivcvdd,
                     Rcpp::IntegerVector iir,
                     Rcpp::IntegerVector ijc){
  
int ierfit = 0;
int iervcv = 0;
int nter   = ivec.at(1);
int ny     = ivec.at(2);
int nty    = ivec.at(3);
int kdist  = ivec.at(4);
int intcpt = ivec.at(7);
int maxit  = ivec.at(8);

double xlike = 0;
double gamthr = rvec.at(0);
double escale = rvec.at(1);

bool lcheck = false;

debug::kprint = ivec.at(9);

if(debug::kprint >= 2){
  
   Rcpp::Rcout << "\nWQMMLESSS - before MLBOTH\n" << std::endl;
   Rcpp::Rcout << "x = "      << x << std::endl;
   Rcpp::Rcout << "y = "      << y << std::endl;
   Rcpp::Rcout << "cen = "    << cen << std::endl;
   Rcpp::Rcout << "wt = "     << wt << std::endl;
   Rcpp::Rcout << "gamthr = " << msftgm << std::endl;
   Rcpp::Rcout << "nrow = "   << nrow << std::endl;
   Rcpp::Rcout << "nter = "   << nter << std::endl;
   Rcpp::Rcout << "ny = "     << ny << std::endl;
   Rcpp::Rcout << "nty = "    << nty << std::endl;
   Rcpp::Rcout << "ty = "     << ty << std::endl;
   Rcpp::Rcout << "tcodes = " << tcodes << std::endl;
   Rcpp::Rcout << "kdist = "  << kdist << std::endl;
   Rcpp::Rcout << "nparm = "  << nparm << std::endl;
   Rcpp::Rcout << "lfix = "   << lfix << std::endl;
   Rcpp::Rcout << "lcheck = " << lcheck << std::endl;
   Rcpp::Rcout << "nparm = "  << nparm << std::endl;
   Rcpp::Rcout << "intcpt = " << intcpt << std::endl;
   Rcpp::Rcout << "escale = " << escale << std::endl;
   Rcpp::Rcout << "e = "      << e << std::endl;
   Rcpp::Rcout << "maxit = "  << maxit << std::endl;
   Rcpp::Rcout << "xlike = "  << xlike << std::endl;
   Rcpp::Rcout << "fv = "     << fv << std::endl;
   Rcpp::Rcout << "dev = "    << dev << std::endl;
   Rcpp::Rcout << "theta = "  << theta << std::endl;
   Rcpp::Rcout << "fsder = "  << fsder << std::endl;
   Rcpp::Rcout << "vcv = "    << vcv << std::endl;
   Rcpp::Rcout << "r = "      << r << std::endl;
   Rcpp::Rcout << "res = "    << res << std::endl;
   Rcpp::Rcout << "ierfit = " << ierfit << std::endl;
   Rcpp::Rcout << "iervcv = " << iervcv << std::endl;

}

 wqm_mlboth(x, y, cen, wt, nrow, nter, ny, nty, ty, tcodes,
            kdist, msftgm, lfix, lcheck, nparm, intcpt,
            escale, e, maxit, dscrat, iscrat,
            xlike, dev, theta, fsder, vcv, r,
            res, fv, ierfit, iervcv,ipxnew,iprv1,ipdiag,iptmat,
            ipthb,ipthg,ipfsd,ipvcvb,ipvcvg,ipnext,itd,itf,
            ied,iw,ivd,ivcvd,ivcvdd,iir,ijc);

if(debug::kprint >= 2){
  
   Rcpp::Rcout << "\nWQMMLESSS - after MLBOTH\n" << std::endl;
   Rcpp::Rcout << "x = "      << x << std::endl;
   Rcpp::Rcout << "y = "      << y << std::endl;
   Rcpp::Rcout << "cen = "    << cen << std::endl;
   Rcpp::Rcout << "wt = "     << wt << std::endl;
   Rcpp::Rcout << "gamthr = " << msftgm << std::endl;
   Rcpp::Rcout << "nrow = "   << nrow << std::endl;
   Rcpp::Rcout << "nter = "   << nter << std::endl;
   Rcpp::Rcout << "ny = "     << ny << std::endl;
   Rcpp::Rcout << "nty = "    << nty << std::endl;
   Rcpp::Rcout << "ty = "     << ty << std::endl;
   Rcpp::Rcout << "tcodes = " << tcodes << std::endl;
   Rcpp::Rcout << "kdist = "  << kdist << std::endl;
   Rcpp::Rcout << "nparm = "  << nparm << std::endl;
   Rcpp::Rcout << "lfix = "   << lfix << std::endl;
   Rcpp::Rcout << "lcheck = " << lcheck << std::endl;
   Rcpp::Rcout << "nparm = "  << nparm << std::endl;
   Rcpp::Rcout << "intcpt = " << intcpt << std::endl;
   Rcpp::Rcout << "escale = " << escale << std::endl;
   Rcpp::Rcout << "e = "      << e << std::endl;
   Rcpp::Rcout << "maxit = "  << maxit << std::endl;
   Rcpp::Rcout << "xlike = "  << xlike << std::endl;
   Rcpp::Rcout << "fv = "     << fv << std::endl;
   Rcpp::Rcout << "dev = "    << dev << std::endl;
   Rcpp::Rcout << "theta = "  << theta << std::endl;
   Rcpp::Rcout << "fsder = "  << fsder << std::endl;
   Rcpp::Rcout << "vcv = "    << vcv << std::endl;
   Rcpp::Rcout << "r = "      << r << std::endl;
   Rcpp::Rcout << "res = "    << res << std::endl;
   Rcpp::Rcout << "ierfit = " << ierfit << std::endl;
   Rcpp::Rcout << "iervcv = " << iervcv << std::endl;

}

if(debug::kprint >= 1) {
  
  Rcpp::Rcout << "\nFitted Values and Deviances\n" << std::endl;
  Rcpp::Rcout << "fv = "     << fv << std::endl;
  Rcpp::Rcout << "dev = "    << dev << std::endl;
  
}

Rcpp::List ints = Rcpp::List::create(Named("ierfit") = ierfit,
                                     Named("iervcv") = iervcv,
                                     Named("nter") = nter,
                                     Named("ny") = ny,
                                     Named("nty") =  ty,
                                     Named("kdist") =  kdist,
                                     Named("intcpt") = intcpt,
                                     Named("maxit") =  maxit);

Rcpp::List doubs = Rcpp::List::create(Named("xlike") = xlike,
                                      Named("gamthr") = gamthr,
                                      Named("escale") = escale);

Rcpp::List bools = Rcpp::List::create(Named("lcheck") = lcheck);

Rcpp::List intvec = Rcpp::List::create(Named("cen") = cen,
                                       Named("wt") = wt,
                                       Named("tcodes") = tcodes,
                                       Named("iscrat") = iscrat,
                                       Named("iir") = iir,
                                       Named("ijc") = ijc);

Rcpp::List numvec = Rcpp::List::create(Named("theta") = theta,
                                       Named("fsder") = fsder,
                                       Named("fv") = fv,
                                       Named("msftgm") = msftgm,
                                       Named("e") = e,
                                       Named("dscrat") = dscrat,
                                       Named("ipthb") = ipthb,
                                       Named("ipthg") = ipthg,
                                       Named("ipfsd") = ipfsd,
                                       Named("iprv1") = iprv1,
                                       Named("ipdiag") = ipdiag,
                                       Named("ipnext") = ipnext,
                                       Named("itd") = itd,
                                       Named("itf") = itf,
                                       Named("ied") = ied,
                                       Named("iw") = iw,
                                       Named("ivd") = ivd);

Rcpp::List nummat = Rcpp::List::create(Named("vcv") = vcv,
                                       Named("ivcvd") = ivcvd,
                                       Named("ivcvdd") = ivcvdd,
                                       Named("r") = r,
                                       Named("res") = res,
                                       Named("x") = x,
                                       Named("y") = y,
                                       Named("ty") = ty,
                                       Named("dev") = dev,
                                       Named("ipvcvb") = ipvcvb,
                                       Named("ipvcvg") = ipvcvg,
                                       Named("ipxnew") = ipxnew,
                                       Named("iptmat") = iptmat);

Rcpp::List logvec = Rcpp::List::create(Named("lfix") = lfix);

return Rcpp::List::create(Named("ints")   = ints,
                          Named("doubs")  = doubs,
                          Named("intvec") = intvec,
                          Named("numvec") = numvec,
                          Named("logvec") = logvec,
                          Named("nummat") = nummat,
                          Named("bools")  = bools);
}