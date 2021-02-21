\name{fm.sparse_get_pairs}
\alias{fm.sparse_get_pairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get pairs computation function in sparse representation
}
\description{Export the internal arrays of the sparse capacity as arrays of singletons, pairs and tuples.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.sparse_get_pairs( envsp=NULL)
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
    \item{output}{The output is the array of pairs and their number.}
  }

  \author{
    %%  ~~who you are~~
      Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
  }
  
  \examples{
    n <- 3
    envsp <- fm.PrepareSparseFM(n)
    envsp <-fm.add_pair_sparse(1,2, 0.4, envsp)
    envsp <-fm.add_pair_sparse(1,3, 0.3, envsp)

    pairs <- fm.sparse_get_pairs(envsp)
    pairs
         envsp <- fm.FreeSparseFM(envsp) 
  }
  % Add one or more standard keywords, see file 'KEYWORDS' in the
  % R documentation directory.
  \keyword{ sparse_get_pairs }
  \keyword{sparse_get_pairs}% __ONLY ONE__ keyword per line