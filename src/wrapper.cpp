#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "fuzzymeasuretools.h"
#include "fuzzymeasurefit.h"
extern "C" {



int log2int(const unsigned int u) {
	int l=0;
	unsigned m=u;
	while (m >>= 1) { ++l; }
	return l;
}

void BanzhafCall(double* v, double* x, int* n,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	int nn = *n;
	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;
	Banzhaf(v, x, nn, *m);	

}


void ChoquetCall(double* x, double* v, int* n, double& cho,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)
{
	int nn = *n;

	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;;
	cho  = Choquet(x, v, nn, *m);

}


void ChoquetMobCall(double*x, double* Mob, int *n, double& choMob,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	int nn = *n;

	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials; 
    choMob = ChoquetMob(x, Mob, nn, *m);
}


void ConstructLambdaMeasureCall(double* singletons, double* lambda, double* v, int &n,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)
{
	int nn = n;

	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials; 

    ConstructLambdaMeasure(singletons, lambda, v, nn, *m);

}


void dualmCall(double* v, double* w, int &n,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)
{
	int nn = log2int(n);

	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;
       dualm(v, w, nn, *m);
}


void EntropyChoquetCall(double* v, int& n, double& cho,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	int nn = n;
 
	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;
	cho  = Entropy(v, nn, *m);
}


int  fittingCall(int *n, int* datanum, int* Kadd, double *v, double *Dataset)
{
	double orness[2];
	orness[0]=0; 
	orness[1]=1;
	int res;
	int nn = *n;
	unsigned int m;
	int datanums = *datanum;
	int additive = *Kadd;

	Preparations_FM(nn,&m);

	double *w = new double[m];

	res = FuzzyMeasureFitLP(nn,  m,  datanums,  additive, w,  Dataset, 0, NULL , NULL, 0, orness);

	for(unsigned int i=0; i<m ; i++)  {
			v[card2bit[i]] = w[i];
	}

	Cleanup_FM();
	delete [] w;
	return res;
}
int  fittingCallKtolerant(int *n, int* datanum, int* Kadd, double *v, double *Dataset)
{
	double orness[2];
	orness[0]=0; 
	orness[1]=1;
	int res;
	int nn = *n;
	unsigned int m;
	int datanums = *datanum;
	int additive = *Kadd;

	Preparations_FM(nn,&m);

	double *w = new double[m];

	res = FuzzyMeasureFitLPStandard(nn,  m,  datanums,  additive, w,  Dataset, 0, NULL , NULL, 0, orness);

	for(unsigned int i=0; i<m ; i++)  {
			v[card2bit[i]] = w[i];
	}

	Cleanup_FM();
	delete [] w;
	return res;
}
int  fittingCallKmaxitive(int *n, int* datanum, int* Kadd, double *v, double *Dataset)
{
	int res;
	int nn = *n;
	unsigned int m;
	int datanums = *datanum;
	int additive = *Kadd;

	Preparations_FM(nn,&m);

	double *w = new double[m];
	if (nn<6 || nn - additive<3)
	res = FuzzyMeasureFitLPMIP(nn,  m,  datanums,  additive, w,  Dataset);
	else 
		res = FuzzyMeasureFitLP_relaxation(nn, m, datanums, additive, w, Dataset);
	for(unsigned int i=0; i<m ; i++)  {
			v[card2bit[i]] = w[i];
	}

	Cleanup_FM();
	delete [] w;
	return res;
}

int FuzzyMeasureFitLPCall(int *n, int* datanum, int* Kadd, double *v, double *Dataset,
    int *options=0, double* indexlow=NULL, double* indexhigh=NULL , int *option1=0, double* orness=NULL)
{
    // int FuzzyMeasureFitLP(int n, int m, int K, int Kadd, double *v, double* XYData, int options=0, 
    //    double* indexlow=NULL, double* indexhigh=NULL , int option1=0, double* orness=NULL);
    // Input parameters: 
    // n - the dimension of inputs, m = 2^n - the number of fuzzy measure values
    // K - the number of empirical data
    // Kadd - k in k-additive f. measures, 1 < Kadd < n+1. Kdd=n - f.m. is unrestricted
    // XYData - an array of size K x (n+1), where each row is the pair (x,y), K data altogether
    // options (default value is 0)
    //    1 - lower bounds on Shapley values supplied in indexlow
    //    2 - upper bounds on Shapley values supplied in indexhigh
    //    3 - lower and upper bounds on Shapley values supplied in indexlow and indexhigh
    //    4 - lower bounds on all interaction indices supplied in indexlow
    //    5 - upper bounds on all interaction indices supplied in indexhigh
    //    6 - lower and upper bounds on all interaction indices supplied inindexlow and indexhigh
    //    all these value will be treated as additional constraints in the LP
    // indexlow, indexhigh - array of size n (options =1,2,3) or m (options=4,5,6)
    // containing the lower and upper bounds on the Shapley values or interaction indices

	// double orness[2];
	// orness[0]=0; 
	// orness[1]=1;
	int res;
	int nn = *n;
	unsigned int m;
	int datanums = *datanum;
	int additive = *Kadd;

	Preparations_FM(nn,&m);

	double *w = new double[m];
//Rprintf("%d,%d,%d %d\n",m,additive,options,option1);
//Rprintf("%f %f\n",orness[0],orness[1]);



	// res = FuzzyMeasureFitLP(nn,  m,  datanums,  additive, w,  Dataset, 0, NULL , NULL, 0, orness);
	res = FuzzyMeasureFitLP(nn,  m,  datanums,  additive, w,  Dataset, 
                  *options, indexlow, indexhigh, *option1, orness);

	for(unsigned int i=0; i<m ; i++)  {
			v[card2bit[i]] = w[i];
	}
//Rprintf("output %d\n",res);

	Cleanup_FM();
	delete [] w;
return res;
}


int fittingOWACall(int *n, int* datanum, double *v, double *Dataset)
{
	double orness[2];
	orness[0]=0; 
	orness[1]=1;
	int res;
	int nn = *n;
	int datanums = *datanum;

	double *w = new double[nn];

	res = FuzzyMeasureFitLPsymmetric(nn,  datanums, w, Dataset, 0, NULL, NULL, 0, orness);
	
	for(int i=0; i<nn ; i++)  {
			v[i] = w[i];
	}
	
	delete [] w;
	return res;
}	


int fittingWAMCall(int *n, int* datanum, double *v, double *Dataset)
{
	double orness[2];
	orness[0]=0; 
	orness[1]=1;
	int res;
	int nn = *n;
	int datanums = *datanum;

	double *w=new double[nn];

	res = FuzzyMeasureFitLPsymmetric(nn,  datanums, w,  Dataset, 1, NULL , NULL, 0, orness);
	
	for(int i=0; i<nn ; i++)  {
			v[i] = w[i];
	}
	
	delete [] w;
	return res;
}	


void InteractionCall(double* Mob, double* w, int *n, int* coalition,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{

	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;

	Interaction(Mob, w, *m);	

	for(int i=0; i<*m; i++)
	{
		 coalition[i] = ShowValue(i);
	}

}	


void InteractionBCall(double* Mob, double* w, int *n, int* coalition,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;

	InteractionB(Mob, w, *m);	

	for(int i=0; i<*m; i++)
	{
		 coalition[i] = ShowValue(i);
	}
}	


int IsMeasureAdditiveCall(double* v, unsigned int &n, int& result,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	// Returns 1 if yes, 0 if no;
    // v is a fuzzy measure in standard representation.
    //unsigned int m;
	int nn = log2int(n);

//	Preparations_FM(nn,&m);

	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;

	result= IsMeasureAdditive(v, nn, *m);
	
//	Cleanup_FM();
	return(result);
}	


int IsMeasureBalancedCall(double* v, unsigned int &n, int& result,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	// Returns 1 if yes, 0 if no;
    // v is a fuzzy measure in standard representation.
    unsigned int mm=*m;
   
	//	Preparations_FM(nn,&m);

	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;

	result= IsMeasureBalanced(v, mm);
	
//	Cleanup_FM();
	return(result);
}	


int IsMeasureSelfdualCall(double* v, unsigned int &n, int& result,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	// Returns 1 if yes, 0 if no;
    // v is a fuzzy measure in standard representation.
 //   unsigned int m;
//	int nn = log2int(n);
//Rprintf("%d  %d \n",n,nn);
 
	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;

	result= IsMeasureSelfdual(v, *m);
//Rprintf("%d %d %d ",result,m,nn);
//Rprintf("%f %f %f %f\n",v[0],v[1],v[2],v[3]);
	
//	Cleanup_FM();
	return(result);
}	


int IsMeasureSubadditiveCall(double* v, unsigned int &n, int& result,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	// Returns 1 if yes, 0 if no;
    // v is a fuzzy measure in standard representation.
  //  unsigned int m;
//	int nn = log2int(n);
	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;

//	Preparations_FM(nn,&m);

	result= IsMeasureSubadditive(v, *m);
	
//	Cleanup_FM();
	return(result);
}	


int IsMeasureSubmodularCall(double* v, unsigned int &n, int& result,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	// Returns 1 if yes, 0 if no;
    // v is a fuzzy measure in standard representation.
   // unsigned int m;
//	int nn = log2int(n);

//	Preparations_FM(nn,&m);
	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;

	result= IsMeasureSubmodular(v, *m);
	
//	Cleanup_FM();
	return(result);
}	


int IsMeasureSuperadditiveCall(double* v, unsigned int &n, int& result,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	// Returns 1 if yes, 0 if no;
    // v is a fuzzy measure in standard representation.
//    unsigned int m;
//	int nn = log2int(n);

//	Preparations_FM(nn,&m);
	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;
	result= IsMeasureSuperadditive(v, *m);
	
//	Cleanup_FM();
	return(result);
}	


int IsMeasureSupermodularCall(double* v, unsigned int &n, int& result,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)
{
	// Returns 1 if yes, 0 if no;
    // v is a fuzzy measure in standard representation.
    //unsigned int m;
//	int nn = log2int(n);

//	Preparations_FM(nn,&m);
	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;
	result= IsMeasureSupermodular(v, *m);
	
//	Cleanup_FM();
	return(result);
}	


int IsMeasureSymmetricCall(double* v, unsigned int &n, int& result, 
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)
{
	// Returns 1 if yes, 0 if no;
    // v is a fuzzy measure in standard representation.
    //unsigned int m;
	int nn = log2int(n);

//	Preparations_FM(nn,&m);

	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;

//Rprintf("%d %d %d %d %d %d\n", nn,*m, card[0], cardpos[1],bit2card[2], card2bit[2]);

	result=IsMeasureSymmetric(v, nn, *m);
	
//	Cleanup_FM();
	return(result);
}	

int IsMeasureKmaxitiveCall(double* v, unsigned int &n, int& result,
	int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	// Returns 1 if yes, 0 if no;
	// v is a fuzzy measure in standard representation.
	// unsigned int m;
	int nn = log2int(n);

	//	Preparations_FM(nn,&m);
	card = Rcard;
	cardpos = Rcardpos;
	bit2card = Rbit2card;
	card2bit = Rcard2bit;
	m_factorials = Rfactorials;

	result = IsMeasureKMaxitive(v, nn, *m);

	//	Cleanup_FM();
	return(result);
}

void MobiusCall(double* v, double* MobVal, int *n,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;
	Mobius(v, MobVal,*n,*m);	
}


void OrnessChoquetMobCall(double* Mob, int *n, double& choMob,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)
{
	unsigned int mm=*m;
	int nn = *n;

//	Preparations_FM(nn,&m);

	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;
    choMob = Orness(Mob, nn, mm);
//	Cleanup_FM();
}


void OWACall(double* x, double* v, int* n, double& owaval)
{
	int nn = *n;
	owaval  =  OWA(x,v,nn);
}


void ShapleyCall(double* v, double* x, int *n,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)
{
	unsigned int mm=*m;
	int nn = *n;
//	Preparations_FM(nn,&m);

	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;
	Shapley(v, x,nn,mm);	
//	Cleanup_FM();
}


void SugenoCall(double* x, double* v, int* n, double& cho,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	unsigned int mm=*m;
	int nn = *n;

	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;
	cho  = Sugeno(x, v, nn, mm);
}


void WAMCall(double* x, double* v, int* n, double& wamval)
{
	int nn = *n;
	wamval  =  WAM(x,v,nn);
}


void ZetaCall(double* Mob, double* v, int &n,
int* m, int* Rcard, int* Rcardpos, unsigned int* Rbit2card, unsigned int* Rcard2bit, double* Rfactorials)

{
	card=Rcard;
 	cardpos=Rcardpos;
	bit2card=Rbit2card;
	card2bit=Rcard2bit;
	m_factorials=Rfactorials;
	Zeta(Mob,v,n,*m);
}



// this is a recursive procedure which helps build all subsets of a given cardinality, and 
// set up conversion arrays
void recursive_card(unsigned int* k, unsigned int level, unsigned int maxlevel, 
                                        unsigned int start, unsigned int finish,
                                        int* b2c, int* c2b, unsigned int *s, int n)
{
        unsigned int i1;
        for(i1=start; i1 <= finish; i1++) { AddToSet(s,i1);
                if(level == maxlevel) {
                        b2c[*s]=*k;
                        c2b[*k]=*s;
                        (*k)++;
                } else {
                        recursive_card(k,level+1,maxlevel,i1+1,finish+1,b2c,c2b,s,n);
                }
                RemoveFromSet(s,i1);
        }
}
void main_card(unsigned int* k, unsigned int level, int* b2c, int* c2b, int n)
{
        // we recursively construct all subsets of cardinality "level"
        unsigned int s=0;
        recursive_card(k,1,level,0, n-level, b2c,c2b, &s,n);
}

SEXP Preparations_FMCall(int* Rn, int* Rm, int* Rcard, int*  Rcardpos, int*  Rbit2card, int*  Rcard2bit, double* m_factorials)
{
        int i;
        unsigned int j;


	int   *cardpos, *bit2card, *card2bit;
int* card;
//	double*  m_factorials;
int n; unsigned int m;
  n=*(Rn);
  m=*(Rm);
  card=Rcard;
  cardpos=Rcardpos;
  bit2card=Rbit2card;
  card2bit=Rcard2bit;
  

//Rprintf("%d %d %d\n",n,m, card[1]);



   //     *m= 1<<(n);

    // calculate the array containing factorials of i! (faster than calculating them every time)
 //   m_factorials=new double[n+1];
        m_factorials[0]=1;
        for(i=1;i<=n;i++) m_factorials[i] = m_factorials[i-1]*i;

    // this array will contains cardinailities of subsets (coded as binaries), i.e. the number of bits in i.
    //    card=new int[(int) *m];
    //    cardpos=new int[n+1];


        card[0]=0; card[1]=0;
        for(j=1;j<m;j++) card[j] = cardf(j);

// these two arrays are used to pass from binary to cardinality ordering
// they are precomputed 
// in binary ordering the subsets are ordered as
// 0 1 2 12 3 13 23 123 4 14 24 124 34 134 234 1234,...
// (which corresponds to the order 0,1,2,3,... in binary form)
// in cardinality ordering they are ordered as
// 0 1 2 3 4 5 6 12 13 14 15 16 23 24 25 26 34 35 36 45 46 56 123 124,...
// (empty, singletons, pairs,triples, etc.)
// for a given subset s in cardinality ordering, to find its binary code use  card2bit[s]
// and vice versa
// cardpos[i] is the index at which subsets with cardinality i+1 start in the cardinality ordering
// i.e. cardpos[0]..cardpos[1]-1 - singletons, cardpos[1]..cardpos[2]-1 - pairs, etc.

   //     bit2card=new unsigned int[*m];
   //     card2bit=new unsigned int[*m];

        unsigned int k; int l;
        bit2card[0]=card2bit[0]=0;

        cardpos[0]=1; // positions where singletons start, the 0th element is empyset

        k=1;
        for(l=1;l<=n-1;l++) {
                main_card(&k, l, bit2card, card2bit,  n);
                cardpos[l]=int(k);
        }
        cardpos[n]=cardpos[n-1]+1;
        
        bit2card[m-1]=card2bit[m-1]=m-1;
return 0;
}


}

#include <R_ext/Rdynload.h>    
#include <R_ext/Visibility.h>

static const R_CallMethodDef callMethods[]  = {
  {NULL, NULL, 0}
};

static R_NativePrimitiveArgType myC_t[] = {
    INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static const R_CMethodDef cMethods[] = {
   {"Preparations_FMCall", (DL_FUNC) &Preparations_FMCall, 7, myC_t},
   {NULL, NULL, 0, NULL}
};

//Rfmtool
void
R_init_Rfmtool(DllInfo *info)
{
   R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
   R_useDynamicSymbols(info, TRUE); 
}

