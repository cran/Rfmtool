\name{fm.sparse_get_singletons}
\alias{fm.sparse_get_singletons}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get singletons of sparse fuzzy measure
}
\description{Export the internal arrays of the sparse capacity as arrays of singletons, pairs and tuples. 
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.sparse_get_singletons(envsp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
}


\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is the numbers of pairs and tuples.}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
  n <- 3
  envsp <- fm.PrepareSparseFM(n)
  envsp <- fm.add_singletons_sparse(c(0, 0.3, 0.5),envsp)
  singletons <- fm.sparse_get_singletons(envsp)
  singletons
       envsp <- fm.FreeSparseFM(envsp) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sparse_get_singletons }
\keyword{sparse_get_singletons}% __ONLY ONE__ keyword per line