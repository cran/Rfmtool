\name{fm.is_inset_sparse }
\alias{fm.is_inset_sparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for checking if i belongs to the tuple A
}
\description{Checks if element i (1-based) belongs to the tuple indexed A (whose cardinality can be 1,2, other (automatically determined)).
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.is_inset_sparse(A, card, i, envsp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{Tuple indexed.}
  \item{card}{Whose cardinality can be 1,2, other (automatically determined)}
  \item{i}{Element (1-based)}
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

  fm.is_inset_sparse(0,3,1,envsp)
  fm.is_inset_sparse(0,3,4,envsp)

  envsp <- fm.FreeSparseFM(envsp)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ is_inset_sparse  }
\keyword{ is_inset }% __ONLY ONE__ keyword per line