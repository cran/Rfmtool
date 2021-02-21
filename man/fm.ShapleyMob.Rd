\name{fm.ShapleyMob}
\alias{fm.ShapleyMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Shapley value computation function in Mobius representation 
}
\description{Calculate the Shapley indices of input criteria from general fuzzy measure in Mobius representation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.ShapleyMob(Mob,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{Fuzzy measure in Mobius representation.}
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
fm.ShapleyMob(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Shapley }

