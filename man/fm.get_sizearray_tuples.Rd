\name{fm.get_sizearray_tuples}
\alias{fm.get_sizearray_tuples}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for exporting the size of the array of tuples 
}
\description{Returns the length of the array of tuples.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.get_sizearray_tuples(envsp=NULL)
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
  \item{output}{The output is the length of the array of tuples.}
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
  fm.get_sizearray_tuples(envsp)

  envsp <- fm.FreeSparseFM(envsp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ get_sizearray_tuples }
\keyword{ tuples }% __ONLY ONE__ keyword per line