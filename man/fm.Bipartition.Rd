\name{fm.Bipartition}
\alias{fm.Bipartition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bipartition interaction index computation function 
}
\description{Calculates the Bipartition interaction indices of input criteria from general fuzzy measure.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.Bipartition(v,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{Fuzzy measure in general representation.}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The output is an array of size 2^n, which contain bipartition interaction indices of input criteria coalitions.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
env<-fm.Init(3)
fm.Bipartition(c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Bipartition  }
\keyword{ NonadditivityIndex }% __ONLY ONE__ keyword per line
