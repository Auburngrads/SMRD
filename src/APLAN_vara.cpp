#include <base/base.hpp>
#include <vvar1/zfcdf.hpp>
#include <aplan/cefail.hpp>
#include <aplan/cefail.hpp>
#include <aplan/var2.hpp>
#include <aplan/optfi.hpp>

using namespace pass1;
using namespace pass2;
using namespace pass3;

double vara(Rcpp::NumericVector delta, 
            int nopt){
  
Rcpp::NumericVector ratio(3);
  
double vsmall = -1.0e-10,det,VARA,pi2c;
int iopt = 0,ioptmp,iplan,k;

// iopts = 1: No optimization of stress
   if(pass3::g_iopts == 1){
     
      // z(1)=(pass2::g_a - wqm_quant(pass2::g_pmlim,idist))/b
      Rcpp::stop("\nVARA: No optimization of stress -- Execution stopped");
     
   }

// iopts = 2: Lowest stress level optimized
   if(pass3::g_iopts == 2){
     
      iopt = iopt + 1;
      pass1::g_z.at(0) = optfi(delta.at(iopt - 1),pass2::g_dliml.at(iopt - 1),pass2::g_dlimu.at(iopt - 1));
     
   }

// iopts = 3: 
   if(pass3::g_iopts == 3){
     
      pass1::g_z.at(0) = pass2::g_zlhold;
     
   }

// iopts = 4: 
   if(pass3::g_iopts == 4){
     
      iopt = iopt + 1;
      pass1::g_z.at(0) = optfi(delta.at(iopt - 1),pass2::g_dliml.at(iopt - 1),pass2::g_dlimu.at(iopt - 1));
      iopt = iopt + 1;
      pass1::g_z.at(1) = optfi(delta.at(iopt - 1),pass2::g_dliml.at(iopt - 1),pass2::g_dlimu.at(iopt - 1));
     
   }

pass1::g_z.at(2) = one;
   
pass1::g_fp.at(0) = zfcdf((pass2::g_a - pass2::g_b1 * pass1::g_z.at(0) - pass2::g_b2 * (std::pow(pass1::g_z.at(0), 2))) / (std::pow(pass2::g_thet1, pass1::g_z.at(0))),pass3::g_idist);
pass1::g_fp.at(2) = zfcdf((pass2::g_a - pass2::g_b1 * pass1::g_z.at(2) - pass2::g_b2 * (std::pow(pass1::g_z.at(2), 2))) / (std::pow(pass2::g_thet1, pass1::g_z.at(2))),pass3::g_idist);

// Choose the middle stress according to the ioptm rule
   ioptmp = pass3::g_ioptm + 1;
   
if(ioptmp == 1){
   
   pass1::g_z.at(1) = (pass1::g_z.at(0) + pass1::g_z.at(2)) / two;
   pass1::g_fp.at(1) = zfcdf((pass2::g_a - pass2::g_b1 * pass1::g_z.at(1) - pass2::g_b2 * (std::pow(pass1::g_z.at(1), 2))) / (std::pow(pass2::g_thet1, pass1::g_z.at(1))),pass3::g_idist);
  
}

if(ioptmp == 2){
   
   pass1::g_fp.at(1) = (pass1::g_fp.at(0) + pass1::g_fp.at(2)) / two;
   //call xerr(2000+ioptm)
   // z(2) = (pass2::g_a-wqm_quant(fp(2),idist))/b
  
}

if(ioptmp == 3){
   
   pass1::g_z.at(1) = (pass1::g_z.at(0) + pass1::g_z.at(2)) / two;
   pass1::g_fp.at(1) = zfcdf((pass2::g_a - pass2::g_b1 * pass1::g_z.at(1) - pass2::g_b2 * (std::pow(pass1::g_z.at(1), 2))) / (std::pow(pass2::g_thet1, pass1::g_z.at(1))),pass3::g_idist);
   
   if(pass1::g_fp.at(1) <= pass2::g_pmlim){
     
      pass1::g_fp.at(1) = pass2::g_pmlim;
      //call xerr(3000+ioptm)
      // z(2)=(pass2::g_a-wqm_quant(fp(2),idist))/b
   
   }

}

if(ioptmp == 4){
   
   pass1::g_fp.at(1) = zfcdf((pass2::g_a - pass2::g_b1 * pass1::g_z.at(1) - pass2::g_b2 * (std::pow(pass1::g_z.at(1), 2))) / (std::pow(pass2::g_thet1, pass1::g_z.at(1))),pass3::g_idist);
  
}

if(debug::kprint > 5){
  
   Rcpp::Rcout << "\nVARA IOPTM\n"                 << std::endl;
   Rcpp::Rcout << " iopta = " << pass3::g_iopta    << std::endl;
   Rcpp::Rcout << " ioptm = " << pass3::g_ioptm    << std::endl;
   Rcpp::Rcout << "ioptmp = " << ioptmp            << std::endl;
   Rcpp::Rcout << "  z(1) = " << pass1::g_z.at(0)  << std::endl;
   Rcpp::Rcout << "  z(2) = " << pass1::g_z.at(1)  << std::endl;
   Rcpp::Rcout << "  z(3) = " << pass1::g_z.at(2)  << std::endl;
   Rcpp::Rcout << " fp(1) = " << pass1::g_fp.at(0) << std::endl;
   Rcpp::Rcout << " fp(2) = " << pass1::g_fp.at(1) << std::endl;
   Rcpp::Rcout << " fp(3) = " << pass1::g_fp.at(2) << std::endl;
   Rcpp::Rcout << " pmlim = " << pass2::g_pmlim    << std::endl;
   
}

// iopta = 1: equal allocation
if(pass3::g_iopta == 1){
  
   pass1::g_pi = Rcpp::NumericVector(3,0.33333e00);
  
}

// iopta = 2: equal expected failing at all sub-experiments
if(pass3::g_iopta == 2){
  
   det = pass1::g_fp.at(0) * pass1::g_fp.at(1) + pass1::g_fp.at(1) * pass1::g_fp.at(2) + pass1::g_fp.at(0) * pass1::g_fp.at(2);
   pass1::g_pi.at(0) = pass1::g_fp.at(1) * pass1::g_fp.at(2) / det;
   pass1::g_pi.at(1) = pass1::g_fp.at(0) * pass1::g_fp.at(2) / det;
  
}

// iopta = 3: fix middle at pifix and optimize
if(pass3::g_iopta == 3){
  
   iopt = iopt + 1;
   pass1::g_pi.at(0) = optfi(delta.at(iopt - 1),pass2::g_dliml.at(iopt - 1),pass2::g_dlimu.at(iopt - 1));
   pass1::g_pi.at(1) = pass2::g_pifix;
  
}

// iopta = 4: optimize with e(rl) = e(rm)
if(pass3::g_iopta == 4){
  
   iopt = iopt + 1;
   pass1::g_pi.at(0) = optfi(delta.at(iopt - 1),pass2::g_dliml.at(iopt - 1),pass2::g_dlimu.at(iopt - 1));
   pass1::g_pi.at(1) = (pass1::g_fp.at(0) / pass1::g_fp.at(1)) * pass1::g_pi.at(0);
  
}
  
// iopta = 5: optimize with e(rm) = e(rh)
if(pass3::g_iopta == 5){
  
   iopt = iopt + 1;
   pass1::g_pi.at(0) = optfi(delta.at(iopt - 1),pass2::g_dliml.at(iopt - 1),pass2::g_dlimu.at(iopt - 1));
   pass1::g_pi.at(1) = (one - pass1::g_pi.at(0)) / (pass1::g_fp.at(1) / pass1::g_fp.at(2) + one);
  
}

// iopta = 6: 4  2  1   relative allocation
if(pass3::g_iopta == 6){
  
   pass1::g_pi.at(0) = 4.0e00 / 7.0e00;
   pass1::g_pi.at(1) = 2.0e00 / 7.0e00;
  
}

// iopta = 7: 
if(pass3::g_iopta == 7){
  
   ratio.at(0) = 1.0e00;
   ratio.at(1) = 2.0e00;
   ratio.at(2) = 2.0e00;
   cefail(pass1::g_fp,ratio,pass1::g_pi);
  
}

// iopta = 8: 
if(pass3::g_iopta == 8){
  
   ratio.at(0) = 1.0e00;
   ratio.at(1) = 2.0e00;
   ratio.at(2) = 3.0e00;
   cefail(pass1::g_fp,ratio,pass1::g_pi);
  
}

// iopta = 9: optimize with e(rl) = e(rm)
if(pass3::g_iopta == 9){
  
   iopt = iopt + 1;
   pass1::g_pi.at(0) = optfi(delta.at(iopt - 1),pass2::g_dliml.at(iopt - 1),pass2::g_dlimu.at(iopt - 1));
   iopt = iopt + 1;
   pi2c = optfi(delta.at(iopt - 1),pass2::g_dliml.at(iopt - 1),pass2::g_dlimu.at(iopt - 1));
   pass1::g_pi.at(1) = pi2c * (one - pass1::g_pi.at(0));
  
}

// iopta = 10: Fix constrain middle and high allocation to be same and optimize
if(pass3::g_iopta == 10){
  
   iopt = iopt + 1;
   pass1::g_pi.at(0) = optfi(delta.at(iopt - 1),pass2::g_dliml.at(iopt - 1),pass2::g_dlimu.at(iopt - 1));
   pass1::g_pi.at(1) = (1.0 - pass1::g_pi.at(0)) / 2.0;
  
}

pass1::g_pi.at(2) = one - pass1::g_pi.at(0) - pass1::g_pi.at(1);
if(pass1::g_pi.at(2) < vsmall) goto line999;
//iprint = 0;
if(debug::kprint >= 10) debug::kprint = 1;
k = 3;

VARA = var2(pass2::g_a,
            pass2::g_b1,
            pass2::g_b2,
            pass2::g_thet1,
            pass1::g_z,
            pass1::g_pi,
            0.0e0,k,
            pass2::g_pval,
            pass3::g_idist,
            pass3::g_knownt,
            pass1::g_fp,
            pass1::g_pq);
      
      iplan = 10 * (pass3::g_iopts - 1) + pass3::g_iopta;
      
if(debug::kprint >= 4){
  
   Rcpp::Rcout << "\nEND OF VARA\n"            << std::endl;
   Rcpp::Rcout << "    a = " << pass2::g_a     << std::endl;
   Rcpp::Rcout << "   b1 = " << pass2::g_b1    << std::endl;
   Rcpp::Rcout << " pval = " << pass2::g_pval  << std::endl;
   Rcpp::Rcout << "pmlim = " << pass2::g_pmlim << std::endl;
   Rcpp::Rcout << "iplan = " << iplan          << std::endl;
   Rcpp::Rcout << "    z = " << pass1::g_z     << std::endl;
   Rcpp::Rcout << "   pi = " << pass1::g_pi    << std::endl;
   Rcpp::Rcout << "   fp = " << pass1::g_fp    << std::endl;
   Rcpp::Rcout << " vara = " << VARA           << std::endl;
  
}
  
return VARA;

line999:   VARA = 1.0e35;
           pass1::g_z  = Rcpp::NumericVector(3,zero);
           pass1::g_pi = Rcpp::NumericVector(3,zero);
           pass1::g_pq = Rcpp::NumericVector(3,zero);
           pass1::g_fp = Rcpp::NumericVector(3,zero);
           
return VARA;
        
}
