#include <base/base.hpp>
#include <genmax/setup.hpp>
#include <genmax/fxest.hpp>
#include <genmax/gmfit.hpp>
#include <genmax/mlrsd.hpp>
#include <genmax/unscpx.hpp>
#include <genmax/unscvx.hpp>

//'   kmod             model code (usual list 1-5)
//'   -------------------------------------------------------------
//'     0  distribution kdist with one or more parameters related to
//'        explanatory variables
//'     1  same as 0 with an lfp parameter
//'     2  same as 0 with a doa parameter
//'     3  steady-state model with distribution kdist plus exponential
//'        competing risk
//'     4  proportional hazards model for distribution kdist plus
//'        explanatory variables for the power (or other) parameters
//' 
//'   kdist            distribution number (usual list 1-12)
//'                    >100 is a user specified distribution
//' 
//'   intd(npard)      vector of 0 or 1 to indicate if an intercept in to
//'                    be included in the regression relationship
//'                    for the ith distribution parameter
//'                    send down 1's where no regression
//' 
//'   nxd(npard)       vector giving number of explanatory variables for the
//'                    ith distribution parameter (not including intercept)
//'                    send down zero's if no regression
//' 
//'   ipxcd(npard)     vector of pointers to the stack, pointing to the
//'                    beginning of integer vectors of
//'                    length  nxd(iparm)+intd(iparm), giving the cols of x
//'                    for the ith relationship (pointer to zero required
//'                    for intercept)
//' 
//'   irelad(npard)    vector of codes giving the chosen relationship
//'                    1 is linear default, and >100 is user specified
//' 
//'   npard            number of 'distribution parameters' (each with
//'                    or without an explanatory variable relationship)
//' 
//'   theta(nparm)     double precision vector returning ml estimates
//'                    of model parameters and values of any fixed parameters
//' 
//'   thetas(nparm)    double precision vector returning ml scaled estimates
//'                    of model parameters and (scaled) values of
//'                    any fixed parameters
//'                    data scaled according to centered and scaled x
//' 
//'   kodet(nparm)     integer vector of parameter codes indicating range
//'                         0 fixed
//'                         1 unrestricted
//'                         2 positive
//'                         3 0-1
//' 
//' 
//'   ifix(nparm)      an integer vector of 0's and 1's to indicate which
//'                    parameters are to be fixed (1 for fixed)
//' 
//'   nparm            number of model parameters
//'                    nparm is the sum of the parameters in all of the
//'                    regression relationships plus any model parameters
//'                    that do not depend on explanatory variables
//' 
//'   ipy              pointer to a real matrix of times.
//'                    transformed times may be returned
//' 
//'   ncoly            number of columns in the observation matrix (1 or 2)
//' 
//'   nrownw             number of rows in the data matrix
//' 
//'   ipx              pointer to real matrix x of explanatory variables which
//'                    must have a column of ones in the first column (even if
//'                    it is not used).
//' 
//'                    note that upon return, x may not be in its original
//'                    form due to scaling or other standardization.
//' 
//'   ncolx            number of columns in x, not including the required col
//'                    of ones (because we think of the ones as column zero)
//' 
//'   ipcode           pointer to a real vector of censor codes
//' 
//'   ipweig           pointer to a real vector of observation weights
//' 
//'   ipty             pointer to a real matrix of truncation times
//' 
//'   ncolty           number of columns in the matrix of truncation times
//'                    (0, 1, or 2)
//' 
//'   iptc             pointer to a real vector of truncation codes
//' 
//'   kcentr           code to indicate how or if the x matrix is to be scaled
//'      0             no centering
//'      1             center and give centered results
//'      2             center and give uncentered results (if possible).
//' 
//'                    for some models, option 2 may not be availabple, in which
//'                    case the default becomes 1.
//' 
//' 
//' 
//'    ifit                  code to indicate type or run
//'  -----------------------------------------------------
//'     0                    use theta input without maximization
//'                            (assumes use of recovered estimates)
//'     1                    use only one pass with powell
//'     2                    use two passes with powell (usually needed)
//' 
//'   pest             quantile to be used to define a new internal
//'                    location parameter for estimation
//' 
//'              pest < 0.0    use usual location parameter only in estimation
//'        0.0 < pest < 1.0    use the inputted value
//'              pest >= 1.0   use automatic value computed from censoring
//'           suggest pest=1 as a default
//' 
//' 
//'   ipplab           pointer to a vector of parameter labels, 8 words for
//'                    each parameter.
//' 
//'                    this pointer is not used if kprint=0 (no output below)
//' 
//'   kprint           print code 0-none  1-minimal 2-usual 3-light debug
//'                    4 and higher provides more debug output
//' 
//'   maxit            maximum number of iterations allowed in powell alg.
//' 
//'   eps              double precision epsilon used in the finite differences
//'                    if zero is sent down, a reasonable default value
//'                    will be used.
//' 
//'   fstder(nparm)    double precision vector returning first derivatives
//'                    of the likelihood with respect to thetas
//' 
//'   xlogl            double precision scaler returning value of the
//'                    log likelihood
//' 
//'   vcv(nparm,nparm) double precision matrix returning estimated matrix of
//'                    variances and covariances of the ml estimates
//' 
//'   vcvs(nparm,nparm) double precision matrix returning estimated matrix of
//'                     variances and covariances of the ml scaled estimates
//' 
//'   r(nparm,nparm)   double precision matrix returning estimated correlation
//'                    matrix from the vcv matrix
//' 
//'   yhat(nrownw)       real vector of fitted values
//' 
//'   resid(nrownw)      real vector of residuals
//' 
//'   ier            code       meaning
//'      -------------------------------------------------------------------
//'      third digit:  0 no optimization errors detected
//'                    1 likelihood shape caused problems with the powell alg
//'                    2 convergence criterion not met after maximum number
//'                      or iterations
//' 
//'    second digit:   0 first derivatives of likelihood small
//'                    1 first derivatives of the loglikelihood too large
//' 
//'     first digit:   0 estimated fisher info matrix inverted successfully
//'                    1 estimated fisher info matrix appears to be singular
//' 
//' note: all pointers sent down in the argument list are pointing to the
//'      rs(.) stack in labeled common.
//' 
//'       fortran typing conventions are generally followed for typing
//'       except that double precision is used except when data is to
//'       be taken from the worksheet.

void genest(int &kmod,
            int &kdist,
            Rcpp::IntegerVector &intd,
            Rcpp::IntegerVector &nxd,
            Rcpp::List &ipxcd,
            Rcpp::IntegerVector &irelad,
            int &npard,
            Rcpp::NumericVector &theta,
            Rcpp::NumericVector &thetas,
            Rcpp::IntegerVector &kodet,
            Rcpp::IntegerVector &ifix,
            int &nparm,
            Rcpp::NumericMatrix &ipy,
            int &ncoly,
            int &nrownw,
            Rcpp::NumericMatrix &ipx,
            int &ncolx,
            Rcpp::IntegerVector &ipcode,
            Rcpp::IntegerVector &ipweig,
            Rcpp::NumericMatrix &ipty,
            int &ncolty,
            Rcpp::IntegerVector &iptc,
            int &kcentr,
            Rcpp::IntegerVector &ipplab,
            int &ifit,
            double &pest,
            int &maxit,
            double &eps,
            Rcpp::NumericVector &fstder,
            double &xlogl,
            Rcpp::NumericMatrix &vcvs,
            Rcpp::NumericMatrix &vcv,
            Rcpp::NumericMatrix &r,
            Rcpp::NumericVector &yhat,
            Rcpp::NumericVector &resid,
            int &ier){
   
double escale = 1000.0e00,epsx,pfail;
int kmccde,kmodp,llog,nregr;

// call setup to center and standardize data
// set likilihood communication variables below
// transform theta values for standardized and centered variables

   setup(kmod,kdist,intd,nxd,ipxcd,irelad,npard,
         theta,thetas,kodet,ifix,nparm,ipy,ncoly,nrownw,ipx,ncolx,
         ipcode,ipweig,ipty,ncolty,iptc,kcentr,ipplab,
         kmodp,pfail,kmccde,llog,nregr,ier);
   
   if(ier > 0) return;

// Call fxest to fix internal constants for constrained or
// reparameterized constrained estimation
   fxest(pfail,pest,kdist,kmccde,kmod);
   epsx = eps;
   
   if(epsx <= zero) epsx = 1.0e-08;

// Call gmfit to find the ml estimates and the covariance matrix
//   if(debug::kprint >= 5) getdum(1);
   
   gmfit(ifit,thetas,kodet,maxit,fstder,ipplab,xlogl,
         nparm,vcvs,r,escale,epsx,nrownw,ier);

// Find residuals and yhat
   mlrsd(ipy,ncoly,nrownw,thetas,kdist,resid,yhat);

// Now find the parameter estimates in terms of the uncentered variables (if possible)
   if(kcentr != 2){
      
      unscpx(thetas,theta);
      unscvx(vcvs,vcv,nparm);
      
   }
   
return;
   
}
