#include <base/base.hpp>

//' This file is used to define all of the global variables
//' used throughout the SMRD package.  Each of these globals 
//' have been defined as \code{extern} in the src/heads/globals.h
//' file.
//' 
//' In FORTRAN these variables were stored in COMMON structures
//' with various names throughout the program.  Here these 
//' COMMONS have been replaced by NAMESPACES of the same 
//' name.
//' 
//' In C++ global variables can only be defined once in a 
//' single .cpp file. That's the purpose of this file.

Rcpp::NumericVector cstak::g_ds(50000);

int debug::kprint;

Rcpp::IntegerVector explan_g::mu_cols;
Rcpp::IntegerVector explan_g::si_cols;
Rcpp::IntegerVector explan_g::p1_cols;
Rcpp::IntegerVector explan_g::p2_cols;
Rcpp::IntegerVector explan_g::p3_cols;

Rcpp::IntegerVector fdnprd_g::ipgty;
Rcpp::IntegerVector fdnprd_g::ipmark;

Rcpp::IntegerVector astww_g::iipcod;
Rcpp::IntegerVector astww_g::iipwei;

Rcpp::NumericMatrix ast0xx_g::iresid;
Rcpp::NumericMatrix ast0xx_g::iyhat;
Rcpp::NumericMatrix ast0xx_g::itimes;
Rcpp::NumericVector ast0xx_g::ip;
Rcpp::NumericVector ast0xx_g::iq;
Rcpp::NumericVector ast0xx_g::iprob;
Rcpp::NumericVector ast0xx_g::ithtmp;
Rcpp::NumericVector ast0xx_g::iypoin;
Rcpp::NumericVector ast0xx_g::ippoin;
Rcpp::NumericVector ast0xx_g::ipsd;
Rcpp::NumericVector ast0xx_g::ippsd;

int genx00::g_nrownw;
int genx00::g_ncoly;
int genx00::g_ncolty;
int genx00::g_npoint;

Rcpp::NumericMatrix genx01::g_ipy;
Rcpp::IntegerVector genx01::g_ipcode;
Rcpp::IntegerVector genx01::g_ipweig;
Rcpp::IntegerVector genx01::g_ipinow;
Rcpp::NumericMatrix genx01::g_ipty;
Rcpp::IntegerVector genx01::g_iptc;

Rcpp::IntegerVector genx03::g_ipkode;
Rcpp::IntegerVector genx03::g_ipplab;
int genx03::g_ngame;

bool genx04::g_ltp; 

int genx05::g_ncolx;
Rcpp::NumericVector genx05::g_ipxbar;
Rcpp::NumericVector genx05::g_ipxbru;
Rcpp::NumericVector genx05::g_ipsd;
Rcpp::IntegerVector genx05::g_ipiscd;
Rcpp::NumericMatrix genx05::g_ipx;

int genx07::g_kdist;
int genx07::g_kmod;
int genx07::g_kmccde;
int genx07::g_kpopu;
int genx07::g_llog;
int genx07::g_nparm;

double genx08::g_pest;
double genx08::g_upest;
int    genx08::g_kparm;
int    genx08::g_klmode;
int    genx08::g_kpoint;

int genx09::g_kscloc;
int genx09::g_kprloc;
int genx09::g_kpwloc;
int genx09::g_kthloc;

double genx14::g_funarg;
int    genx14::g_kfuncf;

int genx15::g_ithetn;
int genx15::g_ithett;
int genx15::g_ithold;
int genx15::g_maxit;
int genx15::g_kodeh;

double genx16::g_xlogmh;
double genx16::g_thetah;
double genx16::g_sigmah;

Rcpp::IntegerVector genx20::g_nxg(20);
Rcpp::IntegerVector genx20::g_nterg(20);
Rcpp::IntegerVector genx20::g_intg(20);
Rcpp::List genx20::g_ipxcg(5);

Rcpp::NumericVector genx21::g_gamms(20);
Rcpp::IntegerVector genx21::g_ipthet(20);
Rcpp::IntegerVector genx21::g_irelag(20);
Rcpp::IntegerVector genx21::g_igtyg(20);

int gdump::g_kprint;
Rcpp::IntegerVector gdump::g_ihead(76);
Rcpp::IntegerVector gdump::g_iunit(76);

int cendum::g_kprint;

bool lstd::g_ltp;

Rcpp::NumericVector rantab::g_t(33);

double passer1::g_beta0p;
double passer1::g_beta1p;
double passer1::g_sigmap;
double passer1::g_ugammap;
double passer1::g_sgammap;
double passer1::g_xlogp;
double passer1::g_wp;

int passer2::g_ndist1p;
int passer2::g_ndist2p;

double passer3::g_beta11p;
double passer3::g_ugamma1p;
double passer3::g_sdgammap;
double passer3::g_w1p;

int passer4::g_ndist1p;
int passer4::g_ndist2p;

double passer5::g_beta0p;
double passer5::g_beta1p;
double passer5::g_sigmap;
double passer5::g_ugammap;
double passer5::g_sdgammap;
double passer5::g_stressp;
double passer5::g_alphap;
int    passer5::g_ndist1p;
int    passer5::g_ndist2p;

double pass2::g_a;
double pass2::g_b1;
double pass2::g_b2;
double pass2::g_thet1;
double pass2::g_pval;
double pass2::g_pifix;
double pass2::g_pmlim;
double pass2::g_zlhold;
Rcpp::NumericVector pass2::g_dlimu(4);
Rcpp::NumericVector pass2::g_dliml(4);

Rcpp::NumericVector pass1::g_z(3);
Rcpp::NumericVector pass1::g_pi(3);
Rcpp::NumericVector pass1::g_fp(3);
Rcpp::NumericVector pass1::g_pq(3);

int pass3::g_knownt;
int pass3::g_idist;
int pass3::g_iopts;
int pass3::g_iopta;
int pass3::g_ioptm;
int pass3::g_iprin;

int trap::g_iercc;

double passersft2gr1::g_tlogp;
double passersft2gr1::g_mut2p;
double passersft2gr1::g_sigmat2p;
double passersft2gr1::g_mur2gr1p;
double passersft2gr1::g_sigmar2gr1p;

double passerurlike::g_tlogp;
double passerurlike::g_mut1p;
double passerurlike::g_sigmat1p;
double passerurlike::g_mut2p;
double passerurlike::g_sigmat2p;
double passerurlike::g_mur1p;
double passerurlike::g_sigmar1p;
double passerurlike::g_mur2p;
double passerurlike::g_sigmar2p;
double passerurlike::g_rhop;

double passer::g_xmu1p;
double passer::g_sig1p;
double passer::g_xmu2p;
double passer::g_sig2p;
double passer::g_rhop;
double passer::g_rootr;
double passer::g_dfp;
double passer::g_tfp;
double passer::g_d0p;
double passer::g_sfactp;
double passer::g_kdmodp;
