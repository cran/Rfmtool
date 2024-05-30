\name{fm.FuzzyMeasureFitLPMob}
\alias{fm.FuzzyMeasureFitLPMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mobius Fuzzy Measure Fitting function, R wrapper for FuzzyMeasureFitLP() in fuzzymeasurefit.cpp

}
\description{
Estimate values of the Mobius fuzzy measures from empirical data.
The result is an array containing the values of the fuzzy measure in Mobius, ordered according to set cardinalities.
kadd defines the complexity of fuzzy measure. if kadd is not provided, its default value is equal to the number of inputs.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.FuzzyMeasureFitLPMob(data, env=NULL, kadd="NA", 
        options=0, indexlow=(NULL), indexhigh=(NULL) , option1=0, orness=(NULL))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Empirical data set in pairs (x_1,y_1),(x_2,y_2),...,(x_d,y_d)
where x_i in [0,1]^n is a vector contains utility values of n input criteria
x_i1,x_i2,...,x_in, y_i in [0,1] is a single aggregated value given by decision makers. 
The data is stored as a matrix of M by n+1 elements, where M is the number of data instances, 
and n is the number of input criteria, the column n + 1 store the observed aggregating value y.
%%     ~~Describe \code{} here~~
}
  \item{env}{Environment variable obtained from fm.Init(n).}
  \item{kadd}{Value of k-additivity, which is used for 
reducing the complexity of fuzzy measures.  
kadd is defined as an optional argument, its default value is kadd = n. kadd is k in k-additive f-measure, 1 < kadd < n+1; if kdd=n - f.m. is unrestricted
%%     ~~Describe \code{} here~~
}
  \item{options}{
Options default value is 0.
   1 - lower bounds on Shapley values supplied in indexlow,
   2 - upper bounds on Shapley values supplied in indexhigh,
   3 - lower and upper bounds on Shapley values supplied in indexlow and indexhigh,
   4 - lower bounds on all interaction indices supplied in indexlow,
   5 - upper bounds on all interaction indices supplied in indexhigh,
   6 - lower and upper bounds on all interaction indices supplied inindexlow and indexhigh.
   All these value will be treated as additional constraints in the LP.
%%     ~~Describe \code{} here~~
}
  \item{indexlow}{
Array of size n (options =1,2,3) or m (options=4,5,6)
containing the lower bounds on the Shapley values or interaction indices
%%     ~~Describe \code{} here~~
}
  \item{indexhigh}{
Array of size n (options =1,2,3) or m (options=4,5,6)
containing the upper bounds on the Shapley values or interaction indices
%%     ~~Describe \code{} here~~
}
  \item{option1}{If the value is 1, the interval of orness values will be fitted (and the desired low and high orness values should be provided). 
If 0, no additional orness constraints. 
%%     ~~Describe \code{} here~~
}

  \item{orness}{
Array of size 2, for example c(0.1,1)
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The output is an array of size 2^n containing estimated Mobius fuzzy measure in binary ordering.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}
\note{
%%  ~~further notes~~
The fit might not be perfect, and not all the constraints can be fully met.
}
\examples{

d <-  matrix( c( 0.00125122, 0.563568, 0.193298, 0.164338, 
            0.808716, 0.584991, 0.479858, 0.544309, 
            0.350281, 0.895935, 0.822815, 0.625868, 
            0.746582, 0.174103, 0.858917, 0.480347, 
            0.71048, 0.513519, 0.303986, 0.387631, 
            0.0149841, 0.0914001, 0.364441, 0.134229, 
            0.147308, 0.165894, 0.988495, 0.388044, 
            0.445679, 0.11908, 0.00466919, 0.0897714, 
            0.00891113, 0.377869, 0.531647, 0.258585, 
            0.571167, 0.601746, 0.607147, 0.589803, 
            0.166229, 0.663025, 0.450775, 0.357412, 
            0.352112, 0.0570374, 0.607666, 0.270228, 
            0.783295, 0.802582, 0.519867, 0.583348, 
            0.301941, 0.875946, 0.726654, 0.562174, 
            0.955872, 0.92569, 0.539337, 0.633631, 
            0.142334, 0.462067, 0.235321, 0.228419, 
            0.862213, 0.209595, 0.779633, 0.498077, 
            0.843628, 0.996765, 0.999664, 0.930197, 
            0.611481, 0.92426, 0.266205, 0.334666, 
            0.297272, 0.840118, 0.0237427, 0.168081), 
       nrow=20, 
       ncol=4,byrow=TRUE);
env<-fm.Init(3)
fm.FuzzyMeasureFitLPMob(d,env)
indexlow=c(0.1,0.1,0.2);
indexhigh=c(0.9,0.9,0.5);
fm.FuzzyMeasureFitLPMob(d,env, kadd=2, indexlow, indexhigh, 
  options=3, option1=1, orness=c(0.1,0.7))
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ FuzzyMeasureFitLP }
\keyword{ FuzzyMeasureFitLPMob }% __ONLY ONE__ keyword per line

