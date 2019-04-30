#include <base/base.hpp>
#include <gensiz/explan.hpp>
#include <gensiz/fdnprd.hpp>

using namespace explan_g;

//' Compute the sizes of parameter vectors
//' 
//' @description Given model information compute nparm and 
//'              npard giving the sizes of parameter vectors.
//'              
//' @param kmod Integer model code (see Details)
//' @param kdist Integer code representing distribution number 
//'              usually in 1-12. \code{kdist > 100}> signifies
//'              a user specified distribution.
//' @param kparv Integer vector giving parameter numbers 
//'              that have regression relationships.
//' @param nrvar Integer vector. \code{nrvar[i]} gives the number of 
//'              columns in the x-matrix that are in the relationship 
//'              for the parameter specified in \code{kparv[i]}.
//' @param mrelat Integer matrix with dimensions [mnrvar,nrelat]  
//'               in which \code{mrelat[i,j]} gives the col of 
//'               the x-matrix for the ith term of the jth relationship.  
//'               \code{i = 1,nrvar[j], j = 1,nrelat} (see Details).
//' @param nrelat Integer giving the number of regression relationships
//' @param mnrvar Integer giving the number of columns in the x-matrix
//'               \code{mnrvar = max(nrvar)}.
//' @param ncolx  Integer giving the number of columns in the x-matrix. This does not include
//'               the required column of ones (we think of the ones as column zero).
//' @param kprint Integer giving the print code for debugging output (see Details).
//' @param nparm  Integer giving the sum of the parameters in all of the regression 
//'               relationships plus any model parameters that do not depend on 
//'               explanatory variables.
//' @param npard  Integer giving the number of parameters in the distributional
//'               model (i.e., for a particular combination of the explanatory 
//'               variables.
//' @param ier Three-digit integer giving error codes upon return (see Details)
//' @param nxd Integer vector (length = 5) giving the number of dimensions in the x-matrix for each relationship
//' @param intd Integer vector (length = 5) giving the number of relationships with an intercept term
//' @param ipxcd Integer vector (length = 5) giving the columns in the x-matrix that are involved with each relationship
//' @param irelad Integer vector (length = 5) giving the number of things (don't know yet). 
//' @param ilabp Integer vector of parameter labels (length 8*nparm) in this new 
//'        version we copy over to stack and pass pointers below.
//' @param ilabd Integer vector of distribution parmameters (length 8*nparm)
//'              in this new version we copy over to stack and pass 
//'              pointers below.
//' @details See Rcpp function \code{genmax} for more information 
//'          on parameters than what is shown here.
//'          
//'          \code{kmod} values:
//'          \describe{
//'           \item{0}{Distribution \code{kdist} with one or more parameters related to
//'                    explanatory variables.}
//'           \item{1}{Same as \code{kmod = 0} with an lfp parameter.}
//'           \item{2}{Same as \code{kmod = 0} with a doa parameter.}
//'           \item{3}{Steady-state model with distribution \code{kdist} plus
//'                    exponential competing risk.}
//'           \item{4}{Proportional hazards model for distribution \code{kdist} 
//'                    plus explanatory variables for the power (or other) parameters.}
//'          }
//'          
//'          Columns in the x-matrix may have a functional relationship
//'          with the location parameter, the scale parameter, or another model
//'          parameter. Each column in the matrix \code{mrelat} gives which columns
//'          in the x-matrix are related to a specific parameter.  If all columns
//'          in the x-matrix are related to the location parameter only one column
//'          in \code{mrelat} will contain non-zero elements.
//'          
//'          \code{kprint} values:
//'          \describe{
//'           \item{0}{no printing at all}
//'           \item{1}{minimal printing}
//'           \item{2}{usual printing - no debugging}
//'           \item{3}{light debugging}
//'           \item{4+}{heavy debugging}
//'          }
//'          
//'          \code{ier} error code values:
//'          \itemize{
//'           \item{third digit:
//'             \describe{
//'              \item{0}{No optimization errors detected}
//'              \item{1}{Likelihood shape caused problems with the powell alg}
//'              \item{2}{Convergence criterion not met after maximum number of iterations}
//'             }
//'             }
//'             \item{second digit:
//'              \describe{
//'               \item{0}{first derivatives of likelihood small}
//'               \item{1}{first derivatives of the loglikelihood too large}
//'              }
//'              }
//'              \item{
//'               \describe{
//'               \item{0}{estimated fisher info matrix inverted successfully}
//'               \item{1}{estimated fisher info matrix appears to be singular}
//'               }
//'              }
//'          }
// [[Rcpp::export]]
Rcpp::List GENSIZ(int &kmod,
                  int &kdist,
                  Rcpp::IntegerVector &kparv,
                  Rcpp::IntegerVector &nrvar,
                  Rcpp::IntegerMatrix &mrelat,
                  int &nrelat,
                  int &mnrvar,
                  int &ncolx,
                  int &kprint,
                  int &nparm,
                  int &npard,
                  int &ier,
                  Rcpp::IntegerVector &nxd,
                  Rcpp::IntegerVector &intd,
                  Rcpp::List &ipxcd,
                  Rcpp::IntegerVector &irelad,
                  Rcpp::IntegerVector &ilabp,
                  Rcpp::IntegerVector &ilabd,
                  int &nregr,
                  int &kgtall,
                  int &llog,
                  int &kmodp,
                  int &npardm,
                  int &nnum, 
                  int &kparm, 
                  int &iup, 
                  int &nterd,
                  int &maxpd){

  debug::kprint = kprint;
  
// explan set up explanatory variables;
  explan(kparv,nrvar,mrelat,nrelat,mnrvar,
         nxd,intd,ipxcd,irelad,ncolx,
         ier,npardm,nnum,kparm,iup,nterd);
  
if(ier == 0) {
  
// set up parameter vector;
  fdnprd(kmod,kdist,intd,nxd,nregr,
         npard,kgtall,nparm,ilabp,
         ilabd,llog,kmodp,ier,maxpd);
  
}

List ints1 = Rcpp::List::create(Named("kmod") = kmod,
                                Named("kdist") = kdist,
                                Named("nrelat") = nrelat,
                                Named("mnrvar") = mnrvar,
                                Named("ncolx") = ncolx,
                                Named("kprint") = kprint,
                                Named("nparm") = nparm,
                                Named("npard") = npard,
                                Named("ier") = ier);

List ints2 = Rcpp::List::create(Named("nregr") = nregr,
                                Named("kgtall") = kgtall,
                                Named("llog") = llog,
                                Named("kmodp") = kmodp,
                                Named("npardm") = npardm,
                                Named("nnum") = nnum,
                                Named("kparm") = kparm,
                                Named("iup") = iup,
                                Named("nterd") = nterd,
                                Named("maxpd") = maxpd);
  
List vecs = Rcpp::List::create(Named("kparv") = kparv,
                               Named("nrvar") = nrvar,
                               Named("mrelat") = mrelat,
                               Named("nxd") = nxd,
                               Named("intd") = intd,
                               Named("irelad") = irelad,
                               Named("ilabp") = ilabp,
                               Named("ilabd") = ilabd);

 return Rcpp::List::create(Named("ints1")   = ints1,
                           Named("ints2")   = ints2,
                           Named("vecs" )   = vecs,
                           Named("ipxcd")   = ipxcd);
 
}