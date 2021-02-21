\name{fm.ShapleyMob_sparse}
\alias{fm.ShapleyMob_sparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Shapley values computation function in sparse representation
}
\description{Calculate Shapley values vectors of size n of a sparse fuzzy measure
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.ShapleyMob_sparse(n, envsp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Size of values vectors}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
  }
  
  \value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    \item{output}{The output is Shapley values vectors of size n of a sparse fuzzy measure.}
  }

  \author{
    %%  ~~who you are~~
      Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
  }
  
  \examples{
    n <- 3
    envsp <- fm.PrepareSparseFM(n, c(0.2,0.4,0.1), c(2,1,2,2,1,3,3,1,2,3)) 

   fm.ShapleyMob_sparse(3, envsp)

   envsp <- fm.FreeSparseFM(envsp) 
    
  }
  % Add one or more standard keywords, see file 'KEYWORDS' in the
  % R documentation directory.
  \keyword{ ShapleyMob_sparse }
  \keyword{ShapleyMob}% __ONLY ONE__ keyword per line