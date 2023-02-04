\name{fm.populate_fm_2add_sparse_from2add}
\alias{fm.populate_fm_2add_sparse_from2add}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for populating 2-additive sparse capacity from 2-additive capacity 
}
\description{Given 2-additive capacity singletons+pairs in one array v , selects nonzero pairs and populates sparse capacity envsp
}
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~

\usage{
  fm.populate_fm_2add_sparse_from2add(n, v, envsp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of inputs}
  \item{v}{Pairs in one array v}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
  }
  
  \value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    \item{output}{The output is a nonzero pairs and populates sparse capacity envsp}
  }
 
  \author{
    %%  ~~who you are~~
      Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
  }
  
  \examples{
      n <- 3
      envsp <- fm.PrepareSparseFM(n, vector(), vector()) 
      envsp <- fm.populate_fm_2add_sparse_from2add(3,c(0.4,0.5,0.6, 0, 0, 0.1),envsp)
envsp
     envsp <- fm.FreeSparseFM(envsp) 	
  }
  % Add one or more standard keywords, see file 'KEYWORDS' in the
  % R documentation directory.
  \keyword{ populate_fm_2add_sparse_from2add  }
  \keyword{populate_fm_2add_sparse_from2add}% __ONLY ONE__ keyword per line