\name{fm.min_subset_sparse}
\alias{fm.min_subset_sparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Minimun of x computation function in sparse representation 
}
\description{Calculates minimum of x with the indices belonging to tuple indexed as S
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.min_subset_sparse(x, S, cardS, envsp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Input vector of size n, containing utility value of input criteria.
  x is in [0,1].}
  \item{S}{Indices belonging to tuple indexed}
  \item{cardS}{Cardinality cardS}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is the minimum of x with the indices belonging to tuple indexed as S}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
  n <- 3
  envsp <- fm.PrepareSparseFM(n, vector(), vector()) 
  envsp <- fm.add_singletons_sparse(c(0.2,0.1,0.2),envsp) 
  envsp <- fm.add_tuple_sparse(c(1,2,3),0.4,envsp);

  fm.min_subset_sparse(c(0.1,0.05,0.2),0,3,envsp)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ min_subset_sparse  }
\keyword{min_subset }% __ONLY ONE__ keyword per line