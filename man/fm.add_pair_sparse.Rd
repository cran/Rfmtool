\name{fm.add_pair_sparse}
\alias{fm.add_pair_sparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for adding a pair to the sparse fuzzy measure
}
\description{This is used for populating capacities which Add a pair v_ij to the structure, their Indices are 1-based.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.add_pair_sparse( i, j, v, envsp = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{One of the indices which are 1-based}
  \item{j}{One of the indices which are 1-based}  
\item{v}{The value to be added.}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n)}
  }
  
  \value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    \item{output}{The output is an added pair v_ij to the structure.}
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
   envsp <-fm.add_pair_sparse(1,2, 0.4, envsp)
   envsp <-fm.add_pair_sparse(1,3, 0.3, envsp)
envsp
     envsp <- fm.FreeSparseFM(envsp)    
  }
  % Add one or more standard keywords, see file 'KEYWORDS' in the
  % R documentation directory.
  \keyword{ add_pair_sparse}
  \keyword{add_pair_sparse}% __ONLY ONE__ keyword per line
