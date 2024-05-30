\name{fm.fittingKinteractiveMC}
\alias{fm.fittingKinteractiveMC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fuzzy Measure Fitting function of the k-interactive using maximal chains method

}
\description{
Estimate values of the k-interacive fuzzy measures from empirical data using maximal chains method.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.fittingKinteractiveMC(data, env=NULL, kadd="NA", K="NA")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Empirical data set in pairs (x_1,y_1),(x_2,y_2),...,(x_d,y_d)
where x_i in [0,1]^n is a vector containing utility values of n input criteria
x_i1,x(i2,...,x_in), y_i in [0,1] is a single aggregated value given by decision makers. 
The data is stored as a matrix of M by n+1 elements, where M is the number of data instances, and n is the number of input criteria, the column n + 1 stores the observed aggregated value y.
%%     ~~Describe \code{data} here~~
}
  \item{env}{Environment variable obtained from fm.Init(n).}
  \item{kadd}{Value of k-interactivity, which is used for reducing the complexity of fuzzy measures.  
kadd is defined as an optional argument, its default value is kadd = 2. 
%%     ~~Describe \code{kadd} here~~
}
\item{K}{The constant K the value of FM value for sets of cardinality kadd+1 is computed from data, default 0.5.}
%%  \item{...}{other arguments}
}

\value{
\item{output}{The output is an array of size 2^n containing estimated standard fuzzy measure in binary ordering.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}
\examples{
env<-fm.Init(3)
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
fm.fittingKinteractiveMC(d,env,2,0.6)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ fitting }
\keyword{ fittingKinteractiveMC }% __ONLY ONE__ keyword per line
