\name{fm.Shapley}
\alias{fm.Shapley}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Shapley value computation function 
}
\description{Calculates the Shapley values of input criteria from general fuzzy measure,
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.Shapley(v,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{Fuzzy measure in general representation.}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
\item{output}{The output is an array of size n, which contain Shapley values of input criteria.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}
\examples{

env<-fm.Init(3)
fm.Shapley(c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Shapley }

