#include <base/base.hpp>
#include <genmax/gmfit1.hpp>

//' Perform ml estimation and find vcvs and correlation matrices
void gmfit(int &ifit,
           Rcpp::NumericVector &thetas,
           Rcpp::IntegerVector &kodet,
           int &maxit,
           Rcpp::NumericVector &fstder,
           Rcpp::IntegerVector &ipplab,
           double &xlogl,
           int &nparm,
           Rcpp::NumericMatrix &vcvs,
           Rcpp::NumericMatrix &r,
           double &escale,
           double &epsx,
           int &nrownw,
           int &ier){
   
Rcpp::NumericVector idelta = Rcpp::NumericVector(nparm);
Rcpp::NumericVector itt    = Rcpp::NumericVector(nparm);
Rcpp::NumericMatrix ivcvss = Rcpp::NumericMatrix(nparm,nparm);

  gmfit1(ifit,itt,thetas,kodet,maxit,idelta,
         fstder,ipplab,xlogl,nparm,vcvs,r,ivcvss,
         escale,epsx,nrownw,ier);

return;
   
}

#include <base/base.hpp>
#include <genmax/flkttx.hpp>
#include <genmax/flkt.hpp>
#include <genmax/flktt.hpp>
#include <genmax/estim.hpp>
#include <genmax/hmat.hpp>
#include <genfun/fdel.hpp>
#include <genfun/gvec.hpp>
#include <genmax/fixp.hpp>
#include <genfun/unfixp.hpp>
#include <utility/wqm_filld.hpp>
#include <genmax/matsqu.hpp>
#include <genmax/unfixv.hpp>
#include <genmax/usloc.hpp>
#include <utility/vmax.hpp>
#include <utility/vmin.hpp>
#include <genmax/preign.hpp>
#include <genmax/dinvv.hpp>
#include <genmax/matexp.hpp>

using namespace genx04;

// Perform ml estimation and find vcvs and correlation matrices
void gmfit1(int &ifit,
            Rcpp::NumericVector &thetat,
            Rcpp::NumericVector &thetas,
            Rcpp::IntegerVector &kodet,
            int &maxit,
            Rcpp::NumericVector &delta,
            Rcpp::NumericVector &fstder,
            Rcpp::IntegerVector &ilabp,
            double &xlogl,
            int &nparm,
            Rcpp::NumericMatrix &vcvs,
            Rcpp::NumericMatrix &vcvss,
            Rcpp::NumericMatrix &r,
            double &escale,
            double &epsx,
            int &nrownw,
            int &ier){
   
double dersm = 1.0e-2,tol1 = 1.0e-05,tol2 = 1.0e-08;
double stepmx,eps,dermx,vvtol = 1.0e-12;
int ierv = 0, ierd = 0, ierp = 0;
int iprpow,ktrcde,nparmm,irank;
Rcpp::List FLKTT,fargs;
double ktt_out;

// Transform parameter input parameters values as necessary
   genx04::g_ltp = 1;
   fixp(thetas,kodet,nparm,thetat);
   fargs = Rcpp::List::create(Named("lt") = thetat,
                              Named("ln") = nparm);
   FLKTT = flktt(fargs);
   ktt_out = Rcpp::as<double>(Rcpp::as<List>(FLKTT)["val"]);
   
   xlogl = -1 * ktt_out;
   
   if(debug::kprint >= 3){
      
      Rcpp::Rcout << "\nGMFIT1**1**\n" << std::endl;
      Rcpp::Rcout << "ilabp = " << ilabp << std::endl;
      Rcpp::Rcout << "thetas = " << thetas << std::endl;
      Rcpp::Rcout << "thetat = " << thetat << std::endl;
      Rcpp::Rcout << "  ifit = " << ifit << std::endl;
      
   }
   
   genx04::g_ltp = 0;
   iprpow = 0;
   if(debug::kprint >= 2) iprpow = 5;
   if(ifit != 0) {
      
      stepmx = 2.0e00;
      
      if(ifit != 1){
         
         // Maximize the log likelihood turn on ltp for first pass
            genx04::g_ltp = 1;
         
         // First estimation uses rough tolerance for large initial steps
            estim(thetat,kodet,nparm,nparmm,tol1,stepmx,maxit,xlogl,
                  flkttx,iprpow,ilabp,ierp);
            
            stepmx = tol1;
   
      }
      
      // Second pass estimation now turn off ltp and reset stepmx and tol for fine tuning
         genx04::g_ltp = 0;
         estim(thetat,kodet,nparm,nparmm,tol2,stepmx,maxit,xlogl,
               flkttx,iprpow,ilabp,ierp);
         goto line102;

   }
   
if(debug::kprint >= 1){
   
   Rcpp::Rcout << "\nGMFIT1: evaluation without optimization\n" << std::endl;
   
}

fargs = Rcpp::List::create(Named("lt") = thetat,
                           Named("ln") = nparm);
FLKTT = flktt(fargs);
ktt_out = Rcpp::as<double>(Rcpp::as<List>(FLKTT)["val"]);

xlogl = -1 * ktt_out;

line102: if(debug::kprint >= 3){
   
            Rcpp::Rcout << "\nGMFIT1**2**\n" << std::endl;
            Rcpp::Rcout << " ilabp = " << ilabp << std::endl;
            Rcpp::Rcout << "thetat = " << thetat << std::endl;
            
         }

// Compute the untransformed values of the parameter  estimates
   unfixp(thetat,kodet,nparm,thetas);

// Transform parameter input parameters values as necessary
   fixp(thetas,kodet,nparm,thetat);

// Get a dump of computed likelihood
   fargs = Rcpp::List::create(Named("lt") = thetat,
                              Named("ln") = nparm);
   FLKTT = flktt(fargs);
   ktt_out = Rcpp::as<double>(Rcpp::as<List>(FLKTT)["val"]);

   xlogl = -1 * ktt_out;
   if(debug::kprint >= 1){
      
      Rcpp::Rcout << "\nGMFIT1: log likelihood = " << xlogl << std::endl;
      
   }
   
   if(debug::kprint >= 3){
      
      Rcpp::Rcout << "\nGMFIT1**3**\n" << std::endl;
      Rcpp::Rcout << " ilabp = " << ilabp << std::endl;
      Rcpp::Rcout << "thetas = " << thetas << std::endl;
               
   }

// Compute delta values to be used in computing the derivatives,
// eps is the desired change in the function
   eps = std::abs(epsx * xlogl);
   if(debug::kprint >= 3){
      
      Rcpp::Rcout << "\nGMFIT1: values of epsx/eps for finite differences\n" << std::endl;
      Rcpp::Rcout << "   eps = "  << eps  << std::endl;
      Rcpp::Rcout << "  epsx = " << epsx << std::endl;
      Rcpp::Rcout << " ilabp = " << ilabp << std::endl;
      Rcpp::Rcout << "thetas = " << thetas << std::endl;
      Rcpp::Rcout << "thetat = " << thetat << std::endl;
               
   }
   
   ktrcde = 0;
// ktrcde will=0 when we are working with thetat (with transformations)
   fdel(flktt,thetat,ktrcde,kodet,nparm,eps,delta);
   if(debug::kprint >= 3){
      
      Rcpp::Rcout << "\nGMFIT1**4**\n" << std::endl;
      Rcpp::Rcout << " ilabp = " << ilabp << std::endl;
      Rcpp::Rcout << "thetas = " << thetas << std::endl;
      Rcpp::Rcout << "thetat = " << thetat << std::endl;
      Rcpp::Rcout << " delta = " << delta << std::endl;
               
   }

// Compute and print the first derivatives
// In finding the derivatives, use the thetas routine with unfix conv in gett
   for(int i = 1; i <= nparm; i++){
      
       fstder.at(i - 1) = 0.0e00;
      
   }
   
   gvec(flkt,nrownw,thetat,delta,nparm,ktrcde,kodet,fstder);
   
// Get the maximum of the absolute values of the first derivatives
   dermx = std::max(std::abs(vmax(fstder,nparm)),std::abs(vmin(fstder,nparm)));
   if((debug::kprint >= 2) or (dermx >= dersm)){
      
       Rcpp::Rcout << "\nGMFIT1: first derivatives of the log likelihood" << std::endl;
       Rcpp::Rcout << "fstder = " << fstder << std::endl;
       
   }
   
   for(int i = 1; i <= nparm; i++){
      
       for(int j = 1; j <= nparm; j++){
          
           vcvs.at(i - 1, j - 1) = 0.0e00;
          
       }
       
   }
   
// Check the size of the first derivatives
   if(dermx > dersm) {
      
      ierd = 1;
      Rcpp::warning("\nDerivatives of the loglikelihood are not close to 0\nvariance-covariance matrix and normal theory confidence limits have no meaning\n");
      
   }
   
   if(ifit <= 1) goto line99;
   
// Compute the fisher information  matrix
// hmat looks at delta to internally skip fixed pareameters
   hmat(flkt,nrownw,thetat,delta,nparm,vcvs,nparm);
   
   if(debug::kprint >= 3){
      
      Rcpp::Rcout << "\nGMFIT1: fisher matrix of thetat\n" << std::endl;
      Rcpp::Rcout << "vcvs = " << vcvs << std::endl;
      Rcpp::Rcout << "thetas = " << thetas << std::endl;
      Rcpp::Rcout << "thetat = " << thetat << std::endl;
      
   }
   
// Copy over fisher matrix, squish, and print eigen info
   vcvss = clone(vcvs);
   matsqu(vcvss,kodet,nparm,nparm);
   if(debug::kprint >= 3) preign(vcvss,nparmm,nparm);
   
   if(debug::kprint >= 3){
      
      Rcpp::Rcout << "\nGMFIT1: after matsqu 1\n" << std::endl;
      Rcpp::Rcout << "vcvss = " << vcvss << std::endl;
      Rcpp::Rcout << " vcvs = " << vcvs << std::endl;
      
   }
// Compute the hessian in terms of the untrans param
// looks at kodet to get info on fixed parameters
   unfixv(vcvs,fstder,thetas,kodet,nparm,nparm);
   
   if(debug::kprint >= 3){
      
      Rcpp::Rcout << "\nGMFIT1: fisher matrix of thetas without percentile fix\n" << std::endl;
      Rcpp::Rcout << "vcvs = " << vcvs << std::endl;
      Rcpp::Rcout << "thetas = " << thetas << std::endl;
      Rcpp::Rcout << "thetat = " << thetat << std::endl;
      
   }
   
// Copy over fisher matrix, squish, and print eigen info
   vcvss = clone(vcvs);
   matsqu(vcvss,kodet,nparm,nparm);
   if(debug::kprint >= 3) preign(vcvss,nparmm,nparm);
   
   if(debug::kprint >= 3){
      
      Rcpp::Rcout << "\nGMFIT1: after matsqu 2\n" << std::endl;
      Rcpp::Rcout << "vcvss = " << vcvss << std::endl;
      Rcpp::Rcout << " vcvs = " << vcvs << std::endl;
      
   }
   
// Invert the squished fisher information matrix
   dinvv(vcvss,nparmm,vvtol,irank,nparm);
   
// Copy over the squished inverse and then expand
   vcvs = clone(vcvss);
   matexp(vcvs,kodet,nparm,nparm);
   if(debug::kprint >= 3){
      
      Rcpp::Rcout << "\nGMFIT1: vcv of thetas without percentile fix\n" << std::endl;
      Rcpp::Rcout << "vcvs = " << vcvs << std::endl;
      
   }
   
   if(irank != nparmm){
      
      ierv = 1;
      Rcpp::warning("\nGMFIT1: irank (%i) != nparmm (%i) (error code: -8080)",irank,nparmm);
      
   }
   
// Put zeros in expanded vcvs where parameters were fixed
   if(nparm != nparmm) {
      
      if(debug::kprint >= 3){
         
         Rcpp::Rcout << "\nGMFIT1:  expanded vcvs without percentile fix\n" << std::endl;
         Rcpp::Rcout << "vcvs = " << vcvs << std::endl;
         
      }
      
   }
   
// Adjust the vcvs for our change to the usual location parameter
   usloc(thetas,vcvs,nparm);
   if(debug::kprint >= 3){
         
      Rcpp::Rcout << "\nGMFIT1:  expanded vcvs percentile fix\n" << std::endl;
      Rcpp::Rcout << "vcvs = " << vcvs << std::endl;
      Rcpp::Rcout << "thetas = " << thetas << std::endl;
      Rcpp::Rcout << "thetat = " << thetat << std::endl;
         
   }

// Copy over fisher matrix, squish, and print eigen info
   vcvss = clone(vcvs);
   matsqu(vcvss,kodet,nparm,nparm);
   if(debug::kprint >= 3) preign(vcvss,nparmm,nparm);
   
// Compute 3-digit error code
   line99: ier = 100 * ierv + 10 * ierd + ierp;
   
   return;
   
}
