#include <base/base.hpp>

//****************************************************************************80

double D1mach(int i)
{
	switch(i){
	
	  case 1: return DBL_MIN;
	  case 2: return DBL_MAX;
	  case 3: return DBL_EPSILON/FLT_RADIX;
	  case 4: return DBL_EPSILON;
	  case 5: return std::log10((double)FLT_RADIX);
	    
	  }
  
	Rcpp::stop("\ninvalid argument: D1mach(%i)", i);
	return 0; /* some compilers demand return values */
}

//****************************************************************************80

long I1mach(int i)
{
	switch(i){
	
	  case 1:  return 5;	/* standard input */
	  case 2:  return 6;	/* standard output */
	  case 3:  return 7;	/* standard punch */
	  case 4:  return 0;	/* standard error */
	  case 5:  return 32;	/* bits per integer */
	  case 6:  return sizeof(int);
	  case 7:  return 2;	/* base for integers */
	  case 8:  return 31;	/* digits of integer base */
	  case 9:  return LONG_MAX;
	  case 10: return FLT_RADIX;
	  case 11: return FLT_MANT_DIG;
	  case 12: return FLT_MIN_EXP;
	  case 13: return FLT_MAX_EXP;
	  case 14: return DBL_MANT_DIG;
	  case 15: return DBL_MIN_EXP;
	  case 16: return DBL_MAX_EXP;
	    
	  }
	
	Rcpp::stop("\ninvalid argument: i1mach(%i)", i);
  
	return 0; /* some compilers demand return values */

}
/***************************************************************************/

float R1mach(int i)
{
	switch(i){
	
	  case 1: return FLT_MIN;
	  case 2: return FLT_MAX;
	  case 3: return FLT_EPSILON/FLT_RADIX;
	  case 4: return FLT_EPSILON;
	  case 5: return std::log10((double)FLT_RADIX);
	    
	  }
  
	Rcpp::stop("\ninvalid argument: r1mach(%i)", i);
	return 0; /* else complaint of missing return value */
}
