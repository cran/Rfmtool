\name{fm.Mobius}
\alias{fm.Mobius}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mobius transform function

}
\description{Calculates Mobius representation of general fuzzy measure,
the input and output is an array of size 2^n=m in binary ordering.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.Mobius(v,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{Fuzzy measure value in standard representation.}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The output is the fuzzy measure in Mobius representation.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}
\examples{

env<-fm.Init(3)
fm.Mobius(c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Mobius }

