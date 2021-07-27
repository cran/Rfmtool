\name{fm.Sugeno}
\alias{fm.Sugeno}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sugeno computation function 

}
\description{
Calculate the value of a Sugeno integral of input x, with fuzzy measure in standard representation  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.Sugeno(x, v,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Input vector of size n, containing utility value of input criteria.
  x is in [0,1].
%%     ~~Describe \code{x} here~~
}
  \item{v}{General fuzzy measure of size m=2^n. Its values can be provided by users,
  or by estimating from empirical data.}
  \item{env}{Environment variable obtained from fm.Init(n).}
%%     ~~Describe \code{v} here~~
}

\value{
\item{output}{The output is a single value of the computed Sugeno integral.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

env<-fm.Init(3)
fm.Sugeno(c(0.6, 0.3, 0.8), c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sugeno }
