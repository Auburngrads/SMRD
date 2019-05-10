#ifndef SMRD_globals_H
#define SMRD_globals_H

namespace debug{
    extern int kprint;
};
namespace fdb001{
    extern double g_pbinp;
    extern double g_pquanp;
    extern int g_np;
    extern double g_xlamp;
};
namespace genx00{
    extern int g_nrownw,g_ncoly,g_ncolty,g_npoint;
};
namespace genx01{
    extern Rcpp::NumericMatrix g_ipy;
    extern Rcpp::IntegerVector g_ipcode;
    extern Rcpp::IntegerVector g_ipweig;
    extern Rcpp::IntegerVector g_ipinow;
    extern Rcpp::NumericMatrix g_ipty;
    extern Rcpp::IntegerVector g_iptc;
};
namespace genx03{
    extern Rcpp::IntegerVector g_ipkode;
    extern Rcpp::IntegerVector g_ipplab;
    extern int g_ngame;
};
namespace genx04{ 
    extern bool g_ltp; 
};
namespace genx05{ 
    extern int g_ncolx;
    extern Rcpp::NumericVector g_ipxbar;
    extern Rcpp::NumericVector g_ipxbru;
    extern Rcpp::NumericVector g_ipsd;
    extern Rcpp::IntegerVector g_ipiscd;
    extern Rcpp::NumericMatrix g_ipx;
};
namespace genx07{
    extern int g_kdist,g_kmod,g_kmccde,g_kpopu,g_llog,g_nparm;
};
namespace genx08{
    extern double g_pest,g_upest;
    extern int g_kparm,g_klmode,g_kpoint;
};
namespace genx09{
    extern int g_kscloc,g_kprloc,g_kpwloc,g_kthloc;
};
namespace genx14{
    extern double g_funarg;
    extern int g_kfuncf;
};
namespace genx15{
    extern int g_ithetn,g_ithett,g_ithold,g_maxit,g_kodeh;
};
namespace genx16{ 
    extern double g_xlogmh,g_thetah,g_sigmah;
};
namespace genx20{ 
    extern Rcpp::IntegerVector g_nxg,g_nterg,g_intg; //all length 20
    extern Rcpp::List g_ipxcg;  // length 5
};
namespace genx21{
    extern Rcpp::NumericVector g_gamms;
    extern Rcpp::IntegerVector g_ipthet,g_irelag,g_igtyg; //all length 20
};
namespace gdump{
    extern int g_kprint;
    extern Rcpp::IntegerVector g_ihead,g_iunit; //all length 76
};
namespace cendum{
    extern int g_kprint;
};
namespace lstd{
    extern bool g_ltp;
};
namespace rantab{
    extern Rcpp::NumericVector g_t; //length 33
};
namespace passer1{
    extern double g_beta0p,g_beta1p,g_sigmap,g_ugammap,g_sgammap,g_xlogp,g_wp;
};
namespace passer2{
    extern int g_ndist1p,g_ndist2p;
};
namespace passer3{
    extern double g_beta11p,g_ugamma1p,g_sdgammap,g_w1p;
};
namespace passer4{
    extern int g_ndist1p,g_ndist2p;
};
namespace passer5{
    extern double g_beta0p,g_beta1p,g_sigmap,g_ugammap,g_sdgammap,g_stressp,g_alphap;
    extern int    g_ndist1p,g_ndist2p;
};
namespace pass2{
    extern double g_a,g_b1,g_b2,g_thet1,g_pval,g_pifix,g_pmlim,g_zlhold;
    extern Rcpp::NumericVector g_dlimu,g_dliml; // all length 4
};
namespace pass1{
    extern Rcpp::NumericVector g_z,g_pi,g_fp,g_pq; // all length 3
};
namespace pass3{
    extern int g_knownt,g_idist,g_iopts,g_iopta,g_ioptm,g_iprin;
};
namespace trap{
    extern int g_iercc;
};
namespace passersft2gr1{
    extern double g_tlogp,g_mut2p,g_sigmat2p,g_mur2gr1p,g_sigmar2gr1p;
};
namespace passerurlike{
    extern double g_tlogp,g_mut1p,g_sigmat1p,g_mut2p,g_sigmat2p;
    extern double g_mur1p,g_sigmar1p,g_mur2p,g_sigmar2p,g_rhop;
};
namespace passer{
    extern double g_xmu1p,g_sig1p,g_xmu2p,g_sig2p,g_rhop,g_rootr;
    extern double g_dfp,g_tfp,g_d0p,g_sfactp,g_kdmodp;
};
#endif