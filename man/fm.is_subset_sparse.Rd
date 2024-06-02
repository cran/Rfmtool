\name{fm.is_subset_sparse }
\alias{fm.is_subset_sparse }
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for checking if tuple B is subset of tuple A 
}
\description{Checks if tuple B is a subset of tuple A, The cardinalities of both tuples need to be supplied.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.is_subset_sparse(A, cardA, B, cardB, envsp = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{Tuple}
  \item{cardA}{Whose cardinality can be 1,2, other (automatically determined)}
  \item{B}{Tuple, tup=0}
  \item{cardB}{Whose cardinality can be 1,2, other (automatically determined)}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is a logical value.}
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
  envsp <- fm.add_pair_sparse(1,2,0.2,envsp);
  envsp <- fm.add_pair_sparse(1,3,0.3,envsp);

  fm.is_subset_sparse(0,3,0,2,envsp) #is 0th pair a subset of the 0th tuple?
  fm.is_subset_sparse(0,3,1,2,envsp) #is 1th pair a subset of the 0th tuple?

  envsp<-fm.FreeSparseFM(envsp)	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ is_subset_sparse  }
\keyword{ is_subset }% __ONLY ONE__ keyword per line