\name{fm.Choquet}
\alias{fm.Choquet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Choquet integral computation function 

}
\description{
Calculates the value of a discrete Choquet integral of input x, with fuzzy measure in general representation.  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.Choquet(x, v, env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Input vector of size n, containing utility value of input criteria.
  x is in [0,1].
}
  \item{v}{The general fuzzy measure of size m=2^n. Its values can be provided by users,
  or by estimating from empirical data.
}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The ouput is a single value of the computed Choquet integral.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}
\examples{
env<-fm.Init(3)
fm.Choquet(c(0.6, 0.3, 0.8), c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env)
env<-fm.Free(env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Choquet }

