\name{fm.NonmodularityIndex_sparse}
\alias{fm.NonmodularityIndex_sparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Nonmodularity index computation function in sparse representation
}
\description{Calculate all 2^n nonmodularity indices using Mobius transform of a fuzzy measure of lenght 2^n=m, using sparse representation
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.NonmodularityIndex_sparse( n, envsp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of inputs}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
  }
  
  \value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    \item{output}{The output is all 2^n nonmodularity indice.}
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
    envsp <- fm.add_singletons_sparse(c(0.2,0.1,0.2),envsp) 
    envsp <- fm.add_pair_sparse(1,2,0.4,envsp);

    fm.NonmodularityIndex_sparse(3,envsp)
    envsp <- fm.FreeSparseFM(envsp)
  }
  % Add one or more standard keywords, see file 'KEYWORDS' in the
  % R documentation directory.
  \keyword{NonmodularityIndex_sparse}
  \keyword{NonmodularityIndex}% __ONLY ONE__ keyword per line