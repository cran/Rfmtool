\name{fm.Zeta}
\alias{fm.Zeta}
\title{Zeta transform function}  
%%  ~~function to do ... ~~
\description{Calculate the general fuzzy measure from Mobius representation. 
The input and output is an array of size 2^n=m in binary ordering. This is the inverse of the Mobius function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{fm.Zeta(Mob,env)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{Fuzzy measure value in Mobius representation.}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
\item{output}{The output is the fuzzy measure in general representation.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

env<-fm.Init(3)
fm.Zeta(c(0.0,0.3,0.5,-0.2,0.4,0.1,-0.2,0.1),env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Zeta }
