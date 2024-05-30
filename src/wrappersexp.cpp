//
//sexp wrapper for fmtools for callbacks
 



#include <cstdlib>
#include <vector> 
 
#include "generaldefs.h" 
 
 
typedef unsigned long long int_64; 
 
 
#ifdef __cplusplus 
 
#include "fuzzymeasuretools.h" 
#include "fuzzymeasurefit.h" 
//#include "fmrandom.h" 
 
#endif 
 
#include <Rcpp.h> 
#include <Rdefines.h> 
#include <stdint.h> 
 
 
 
struct  fm_env { 
    int n; 
    int m; 
    int* card; 
    int* cardpos; 
    double* bit2card;     
    double* card2bit; 
    double* factorials; 
 
}; 
 
 
 
 
using namespace Rcpp; 
 
// forward declarations (instead of .h file) 
 int generate_fm_kinteractivedualconvex(int_64 num, int n, int kadd, int markov, int_64* length, double noise, double* vv,   void* extrachecks); 
 int generate_fm_kinteractivedualconcave(int_64 num, int n, int kadd, int markov, int_64* length, double noise, double* vv, void* extrachecks); 
  
  int generate_fm_randomwalk(int_64 num, int n, int kadd, int markov, int option, double step , double* vv, int* len, void* extrachecks); 
  int generate_fm_2additive_randomwalk2(int_64 num, int n, int markov, int option, double noise, double* vv, void* extrachecks); 
  
 // wrapper callback  
SEXP* fm_fn2; 
int myfun2( int* n, double* x) 
{ 
    int f1=*n; 
    SEXP fval; 
//    Rprintf("\n%s  %d %d %f\n", "Back to myf-C: 1 ", f1,*n , x[0]); 
     
	Rcpp::Function func(PROTECT( *fm_fn2));
    NumericVector X=as<NumericVector>(wrap(std::vector<double>(x,x+ *n)));
    PROTECT(fval=func(wrap(f1), X));
 
    f1=as<int>(PROTECT(fval));
    UNPROTECT(3);
 //  Rprintf("\n%s  %d %d %f\n", "Back to myf-C: 2 ", f1,*n , x[0]);
    return f1;
} 
/* Various tests with macro versions in the internal headers
Rboolean (Rf_isNull)(SEXP s);
Rboolean (Rf_isSymbol)(SEXP s);
Rboolean (Rf_isLogical)(SEXP s);
Rboolean (Rf_isReal)(SEXP s);
Rboolean (Rf_isComplex)(SEXP s);
Rboolean (Rf_isExpression)(SEXP s);
Rboolean (Rf_isEnvironment)(SEXP s);
Rboolean (Rf_isString)(SEXP s);
Rboolean (Rf_isObject)(SEXP s);
 */

	//	option==0 normal, with the first element V 
	// 0 - normal, 1 convex, 2 antibuoyant, 3 kadditive ,   4 belief,  5 kadditive convex,  
//option =0x0100 standars slower mon/convexiy check, 0x1000 or 0x0010 - avoid belief meaures to start walk  
 
  SEXP generate_fm_randomwalkCall(SEXP num, SEXP n, SEXP kadd, SEXP markov, SEXP option, SEXP step , SEXP extrachecks,  
                                  SEXP scard, SEXP scardpos, SEXP sbit2card, SEXP scard2bit, SEXP sfactorials, SEXP Nu) 
  { 
	  int N = as<int>(n); 
 
      int NU = as<int>(Nu); 

      if(0) { 
          double x[2]={1,2}; 
           
          Rprintf("\n%s  %d %d %f\n", "here ", N, N, x[0]); 
          Rcpp::Function func( extrachecks); 
          N=2; 
          NumericVector X=as<NumericVector>(wrap(std::vector<double>(x,x+2))); 
          SEXP fval=func(wrap(N), X); 
      } 
       
 
 
       N = as<int>(n); 
       
       
	  int_64 Num = as<int>(num); 
	  int Kadd= as<int>(kadd); 
	  int Markov= as<int>(markov); 
	  int Option= as<int>(option); 
	  double Step=as<double>(step); 
       
 //     Rprintf("\n %s %d %d  %x ", "before myf-C: ", N, Num, extrachecks); 
       
       
      int* Rcard=INTEGER(scard); 
      int* Rcardpos=INTEGER(scardpos); 
      double* Rbit2card=REAL(sbit2card); 
      double* Rcard2bit=REAL(scard2bit); 
      double* Rfactorials=REAL(sfactorials); 
 
      card = Rcard; 
      cardpos = Rcardpos; 
      bit2card = (int_64*)Rbit2card; 
      card2bit = (int_64*)Rcard2bit; 
      m_factorials = Rfactorials; 
		 
 
		 
		int TNUM = Num; 
		 
		switch(Option& 0xFF ) { 
			case 3: 
			case 5: 
			TNUM *= fm_arraysize_kadd(N, Kadd); 
				break; 
			case 1: 
			case 2: 
			case 4: 
			default: 
			TNUM *= (0x1UL << N); 
				 
		} 
	NumericVector VV(TNUM); 
	double* VVP=NUMERIC_POINTER(VV); 
	int len; 
	int out; 
       
	if(NU==0) out = generate_fm_randomwalk(Num, N, Kadd, Markov, Option, Step, VVP, &len, NULL); 
    else { 
//        Rprintf("\n IN \n"); 
         
        fm_fn2= &extrachecks;
        PROTECT(*fm_fn2);
//        Rprintf("\n %s %d %d  %x ", "before myf-C: ", N, Num, fm_fn2);
        out = generate_fm_randomwalk(Num, N, Kadd, Markov, Option, Step, VVP, &len, (void*)(&myfun2)); 
        UNPROTECT(1);
    }
		 
	return  
			List::create(Named("V") = VV,  
                        Named("length") = len); 
	  //return wrap(out); 
  } 
   
   
    SEXP generate_fm_kinteractivedualconvexCall(SEXP num, SEXP n, SEXP kadd, SEXP markov,  SEXP noise , SEXP extrachecks, 
                                                SEXP scard, SEXP scardpos, SEXP sbit2card, SEXP scard2bit, SEXP sfactorials, SEXP Nu) 
	  // returns length 
  { 
      int_64 Num = as<int>(num); 
      int N = as<int>(n); 
      int NU = as<int>(Nu); 
//	  fm_fn2=(void*) extrachecks; 
 
       
       
      int Kadd= as<int>(kadd); 
      int Markov= as<int>(markov); 
	  //int_64 Option; 
      double Step=as<double>(noise); 
       
      int* Rcard=INTEGER(scard); 
      int* Rcardpos=INTEGER(scardpos); 
      double* Rbit2card=REAL(sbit2card); 
      double* Rcard2bit=REAL(scard2bit); 
      double* Rfactorials=REAL(sfactorials); 
 
      card = Rcard; 
      cardpos = Rcardpos; 
      bit2card = (int_64*)Rbit2card; 
      card2bit = (int_64*)Rcard2bit; 
      m_factorials = Rfactorials; 
       
	   
 //     Rprintf("\n %s %d %d %d %d %f %x ", "before myf-C: ", N, Num, Kadd, Markov,Step,fm_fn2);
       
		 
	  int TNUM = Num; 
	  int_64 Len=(fm_arraysize_kadd(N, Kadd) +N); 
	  TNUM *=Len; 
       
  //    fm_fn2=NULL; 
       
//      Rprintf("\n %s %d %d %d %d %d", "in myf-C: ", TNUM, Len,N, card[0],card[1]); 
	   
	  NumericVector VV(TNUM); 
      double* VVP=NUMERIC_POINTER(VV); 
	   
       
      int out; 
      if(NU!=0) { 
          fm_fn2=&extrachecks; 
          out= generate_fm_kinteractivedualconvex(Num, N, Kadd, Markov, &Len, Step, VVP,     (void*)(&myfun2)); 
      } 
      else 
          out= generate_fm_kinteractivedualconvex(Num, N, Kadd, Markov, &Len, Step, VVP,    NULL); 
       
       
		out=(int)Len; 
		 
		return  
			List::create(Named("V") = VV,  
                        Named("length") = out); 
	  //return wrap(out); 
  } 
   
      SEXP generate_fm_kinteractivedualconcaveCall(SEXP num, SEXP n, SEXP kadd, SEXP markov,  SEXP noise ,  SEXP extrachecks, 
                                                   SEXP scard, SEXP scardpos, SEXP sbit2card, SEXP scard2bit, SEXP sfactorials, SEXP Nu) 
	  // returns length 
  { 
	  int N = as<int>(n); 
      int NU = as<int>(Nu); 
//	  fm_fn2=(void*) extrachecks; 
 
       
	  int_64 Num = as<int>(num); 
	  int Kadd= as<int>(kadd); 
	  int Markov= as<int>(markov); 
	  //int_64 Option; 
	  double Step=as<double>(noise); 
	   
      int* Rcard=INTEGER(scard); 
      int* Rcardpos=INTEGER(scardpos); 
      double* Rbit2card=REAL(sbit2card); 
      double* Rcard2bit=REAL(scard2bit); 
      double* Rfactorials=REAL(sfactorials); 
 
      card = Rcard; 
      cardpos = Rcardpos; 
      bit2card = (int_64*)Rbit2card; 
      card2bit = (int_64*)Rcard2bit; 
      m_factorials = Rfactorials; 
	   
	  int TNUM = Num; 
      int_64 Len=(fm_arraysize_kadd(N, Kadd) +N); 
      TNUM *=Len; 
	   
	  NumericVector VV(TNUM); 
      double* VVP=NUMERIC_POINTER(VV); 
	  	 
       
      int out; 
      if(NU!=0){ 
          fm_fn2= &extrachecks; 
          out= generate_fm_kinteractivedualconcave(Num, N, Kadd, Markov, &Len, Step, VVP,     (void*)(&myfun2)); 
      } 
      else 
          out= generate_fm_kinteractivedualconcave(Num, N, Kadd, Markov, &Len, Step, VVP,    NULL); 
       
		out=(int)Len; 
		 
		return  
			List::create(Named("V") = VV,  
                        Named("length") = out);		 
	  //return wrap(out); 
  } 
   
   
   
    SEXP generate_fm_2additive_randomwalk2Call(SEXP num, SEXP n,  SEXP markov, SEXP option, SEXP step ,  SEXP extrachecks, SEXP Nu) 
  { 
	  int N = as<int>(n); 
//	  fm_fn2=(void*) extrachecks; 
        int NU = as<int>(Nu); 
         
	  int_64 Num = as<int>(num); 
 
	  int Markov= as<int>(markov); 
	  int Option= as<int>(option); 
	  double Step=as<double>(step); 
	   
	  int TNUM = Num; 
	  int length=fm_arraysize_2add(N); 
	  TNUM *=length; 
	   
	  NumericVector VV(TNUM); 
      double* VVP=NUMERIC_POINTER(VV); 
 
        if(NU!=0){ 
            fm_fn2= &extrachecks; 
            generate_fm_2additive_randomwalk2(Num, N, Markov, Option, Step, VVP, (void*)(&myfun2)); 
        } 
        else 
            generate_fm_2additive_randomwalk2(Num, N, Markov, Option, Step, VVP, NULL); 
		 
		return  
			List::create(Named("V") = VV,  
                        Named("length") = length);			 
	  //return wrap(out); 
  } 
   
   
   
