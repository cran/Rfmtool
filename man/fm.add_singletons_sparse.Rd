\name{fm.add_singletons_sparse}
\alias{fm.add_singletons_sparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for adding singletons to the sparse fuzzy measure
}
\description{This is used for adding singletons to the structure.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.add_singletons_sparse(v, envsp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{The vector of singletons of size n.}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is added singletons to the structure.}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
  n <- 3
  tups<-vector()
  tupsidx<-vector()
  envsp <- fm.PrepareSparseFM(n, tups,tupsidx) 

 envsp <- fm.add_singletons_sparse(c(0, 0.3, 0.5),envsp)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{add_singletons_sparse}
\keyword{add_singletons_sparse}% __ONLY ONE__ keyword per line