\name{fm.populate_fm_2add_sparse}
\alias{fm.populate_fm_2add_sparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for populating 2-additive sparse capacity
}
\description{Populate 2-additive sparse capacity with nonzero values using the singletons and two arrays of indices (of size numpairs).
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.populate_fm_2add_sparse(singletons, numpairs, pairs, indicesp1, indicesp2, envsp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{singletons}{Singletons 0-based.}
  \item{numpairs}{Size numpairs.}
  \item{pairs}{Array 0-based.}
  \item{indicesp1}{Array of indices of Size numpairs.need to be 1-based.}
  \item{indicesp2}{Array of indices of Size numpairs.need to be 1-based.}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
}


\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is Populate 2-additive sparse capacity with nonzero values using the singletons and two arrays of indices (of size numpairs)}
}
\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
    n <- 3
    envsp <- fm.PrepareSparseFM(n, vector(), vector()) 

   envsp <- fm.populate_fm_2add_sparse(c(0.1,0.2,0.3), 3, 
              c(0.4,0.5,0.6), c(1,1,2), c(2,3,3), envsp)
    envsp
    envsp <- fm.FreeSparseFM(envsp) 
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ populate_fm_2add_sparse}
\keyword{populate_fm_2add_sparse}% __ONLY ONE__ keyword per line
