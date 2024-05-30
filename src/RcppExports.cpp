#include <Rcpp.h>

using namespace Rcpp;


/*
parameters description:


 */


// fm


#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>



SEXP generate_fm_randomwalkCall(SEXP num, SEXP n, SEXP kadd, SEXP markov, SEXP option, SEXP step , SEXP extrachecks, SEXP scard, SEXP scardpos, SEXP sbit2card, SEXP scard2bit, SEXP sfactorials, SEXP NU);

RcppExport SEXP fm_generate_fm_randomwalkCall(SEXP numS, SEXP nS, SEXP kaddS, SEXP markovS, SEXP optionS, SEXP stepS , SEXP extrachecksS, SEXP scard, SEXP scardpos, SEXP sbit2card, SEXP scard2bit, SEXP sfactorials, SEXP NU) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< SEXP >::type num(numS);
    Rcpp::traits::input_parameter< SEXP >::type n(nS);
    Rcpp::traits::input_parameter< SEXP >::type kadd(kaddS);
    Rcpp::traits::input_parameter< SEXP >::type markov(markovS);
    Rcpp::traits::input_parameter< SEXP >::type option(optionS);
    Rcpp::traits::input_parameter< SEXP >::type step(stepS);
    Rcpp::traits::input_parameter< SEXP >::type extrachecks(extrachecksS);
    Rcpp::traits::input_parameter< SEXP >::type card(scard);
    Rcpp::traits::input_parameter< SEXP >::type cardpos(scardpos);
    Rcpp::traits::input_parameter< SEXP >::type bit2card(sbit2card);
    Rcpp::traits::input_parameter< SEXP >::type card2bit(scard2bit);
    Rcpp::traits::input_parameter< SEXP >::type factorials(sfactorials);
    Rcpp::traits::input_parameter< SEXP >::type Nu(NU);
//    Rcpp::traits::input_parameter< SEXP >::type b(bSEXP);
    __result = Rcpp::wrap(generate_fm_randomwalkCall(num, n, kadd, markov, option, step, extrachecks, card, cardpos, bit2card, card2bit, factorials,Nu));
    return __result;
END_RCPP
}

static const R_CallMethodDef callMethods_generate_fm_randomwalk[]  = {
  {NULL, NULL, 0}
};

static R_NativePrimitiveArgType myC_t_generate_fm_randomwalk[] = {
    INTSXP,  INTSXP,  INTSXP,  INTSXP,  INTSXP, REALSXP, INTSXP, INTSXP, INTSXP,REALSXP,REALSXP,REALSXP,INTSXP
};

static const R_CMethodDef cMethods_generate_fm_randomwalk[] = {
    {"generate_fm_randomwalkCall", (DL_FUNC) &generate_fm_randomwalkCall, 13, myC_t_generate_fm_randomwalk},
    {NULL, NULL, 0, NULL}};
    
    
    

SEXP generate_fm_kinteractivedualconvexCall(SEXP num, SEXP n, SEXP kadd, SEXP markov,  SEXP noise , SEXP extrachecks, SEXP scard, SEXP scardpos, SEXP sbit2card, SEXP scard2bit, SEXP sfactorials, SEXP Nu);
    
    RcppExport SEXP fm_generate_fm_kinteractivedualconvexCall
    (SEXP numSEXP, SEXP nSEXP, SEXP kaddSEXP, SEXP markovSEXP, SEXP noiseSEXP,  SEXP extrachecksSEXP, 
     SEXP scard, SEXP scardpos, SEXP sbit2card, SEXP scard2bit, SEXP sfactorials, SEXP NU) {
    BEGIN_RCPP
        Rcpp::RObject __result;
        Rcpp::traits::input_parameter< SEXP >::type num(numSEXP);
        Rcpp::traits::input_parameter< SEXP >::type n(nSEXP);
        Rcpp::traits::input_parameter< SEXP >::type kadd(kaddSEXP);
        Rcpp::traits::input_parameter< SEXP >::type markov(markovSEXP);
        Rcpp::traits::input_parameter< SEXP >::type noise(noiseSEXP);
        Rcpp::traits::input_parameter< SEXP >::type extrachecks(extrachecksSEXP);
        Rcpp::traits::input_parameter< SEXP >::type card(scard);
        Rcpp::traits::input_parameter< SEXP >::type cardpos(scardpos);
        Rcpp::traits::input_parameter< SEXP >::type bit2card(sbit2card);
        Rcpp::traits::input_parameter< SEXP >::type card2bit(scard2bit);
        Rcpp::traits::input_parameter< SEXP >::type factorials(sfactorials);
        Rcpp::traits::input_parameter< SEXP >::type Nu(NU);
 //       Rprintf("\n %s %x %x %d %f %x %x", "before myf-C: ", n, num, kadd, noise,extrachecks, env0);
        
    //    Rcpp::traits::input_parameter< SEXP >::type b(bSEXP);
        __result = Rcpp::wrap(generate_fm_kinteractivedualconvexCall(num, n, kadd, markov, noise,  extrachecks, card, cardpos, bit2card, card2bit, factorials,Nu));
        return __result;
    END_RCPP
    }
    

    SEXP generate_fm_kinteractivedualconcaveCall(SEXP num, SEXP n, SEXP kadd, SEXP markov,  SEXP noise , SEXP extrachecks, SEXP scard, SEXP scardpos, SEXP sbit2card, SEXP scard2bit, SEXP sfactorials, SEXP Nu);
        
        RcppExport SEXP fm_generate_fm_kinteractivedualconcaveCall
        (SEXP numS, SEXP nS, SEXP kaddS, SEXP markovS, SEXP noiseS,  SEXP extrachecksS,
         SEXP scard, SEXP scardpos, SEXP sbit2card, SEXP scard2bit, SEXP sfactorials, SEXP NU) {
        BEGIN_RCPP
            Rcpp::RObject __result;
            Rcpp::traits::input_parameter< SEXP >::type num(numS);
            Rcpp::traits::input_parameter< SEXP >::type n(nS);
            Rcpp::traits::input_parameter< SEXP >::type kadd(kaddS);
            Rcpp::traits::input_parameter< SEXP >::type markov(markovS);
            Rcpp::traits::input_parameter< SEXP >::type noise(noiseS);
            Rcpp::traits::input_parameter< SEXP >::type extrachecks(extrachecksS);
            Rcpp::traits::input_parameter< SEXP >::type card(scard);
            Rcpp::traits::input_parameter< SEXP >::type cardpos(scardpos);
            Rcpp::traits::input_parameter< SEXP >::type bit2card(sbit2card);
            Rcpp::traits::input_parameter< SEXP >::type card2bit(scard2bit);
            Rcpp::traits::input_parameter< SEXP >::type factorials(sfactorials);
            Rcpp::traits::input_parameter< SEXP >::type Nu(NU);
        //    Rcpp::traits::input_parameter< SEXP >::type b(bSEXP);
            __result = Rcpp::wrap(generate_fm_kinteractivedualconcaveCall(num, n, kadd, markov, noise,  extrachecks, card, cardpos, bit2card, card2bit, factorials,Nu));
            return __result;
        END_RCPP
        }
    

    
    static const R_CallMethodDef callMethods_generate_fm_kinteractivedualconvex[]  = {
      {NULL, NULL, 0}
    };

    static R_NativePrimitiveArgType myC_t_generate_fm_kinteractivedualconvex[] = {
        INTSXP,  INTSXP,  INTSXP,  INTSXP,  REALSXP, INTSXP,  INTSXP, INTSXP,REALSXP,REALSXP,REALSXP,INTSXP
    };

static const R_CMethodDef cMethods_generate_fm_kinteractivedualconvex[] = {
    {"generate_fm_kinteractivedualconvexCall", (DL_FUNC) &generate_fm_kinteractivedualconvexCall, 12, myC_t_generate_fm_kinteractivedualconvex},
    {NULL, NULL, 0, NULL} };
        
static const R_CMethodDef cMethods_generate_fm_kinteractivedualconcave[] = {
            {"generate_fm_kinteractivedualconcaveCall", (DL_FUNC) &generate_fm_kinteractivedualconcaveCall, 12, myC_t_generate_fm_kinteractivedualconvex},
            {NULL, NULL, 0, NULL} };
    
    

SEXP generate_fm_2additive_randomwalk2Call(SEXP num, SEXP n,  SEXP markov, SEXP option, SEXP step ,  SEXP extrachecks, SEXP Nu);
RcppExport SEXP fm_generate_fm_2additive_randomwalk2Call(SEXP numS, SEXP nS, SEXP markovS, SEXP optionS, SEXP stepS,  SEXP extrachecksS, SEXP NU) {

BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< SEXP >::type num(numS);
    Rcpp::traits::input_parameter< SEXP >::type n(nS);
    Rcpp::traits::input_parameter< SEXP >::type markov(markovS);
    Rcpp::traits::input_parameter< SEXP >::type option(optionS);
    Rcpp::traits::input_parameter< SEXP >::type step(stepS);
    Rcpp::traits::input_parameter< SEXP >::type extrachecks(extrachecksS);
    Rcpp::traits::input_parameter< SEXP >::type Nu(NU);
    __result = Rcpp::wrap(generate_fm_2additive_randomwalk2Call(num,n,markov,option,step,extrachecks,Nu));
	return __result;
END_RCPP
}
                                              
static const R_CallMethodDef callMethods_generate_fm_2additive_randomwalk[]  = {
  {NULL, NULL, 0}
 };

 static R_NativePrimitiveArgType myC_t_generate_fm_2additive_randomwalk[] = {
    INTSXP, INTSXP, INTSXP, INTSXP, REALSXP, INTSXP,INTSXP // Check type of  SEXP Fn
 };

  static const R_CMethodDef cMethods_generate_fm_2additive_randomwalk[] = {
  {"generate_fm_2additive_randomwalk2Call", (DL_FUNC) &generate_fm_2additive_randomwalk2Call, 7, myC_t_generate_fm_2additive_randomwalk},
     {NULL, NULL, 0, NULL}
   };

                                              




void
R_init_fm(DllInfo *info)
{
   R_registerRoutines(info, cMethods_generate_fm_2additive_randomwalk, callMethods_generate_fm_2additive_randomwalk, NULL, NULL);
   R_registerRoutines(info, cMethods_generate_fm_kinteractivedualconvex, callMethods_generate_fm_kinteractivedualconvex, NULL, NULL);
   R_registerRoutines(info, cMethods_generate_fm_kinteractivedualconcave, callMethods_generate_fm_kinteractivedualconvex, NULL, NULL);
    
   R_registerRoutines(info, cMethods_generate_fm_randomwalk, callMethods_generate_fm_randomwalk, NULL, NULL);
   
   R_useDynamicSymbols(info, TRUE);
};


