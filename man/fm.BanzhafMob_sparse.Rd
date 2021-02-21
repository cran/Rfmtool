\name{fm.BanzhafMob_sparse}
\alias{fm.BanzhafMob_sparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Banzhaf values computation function in sparse representation
}
\description{Calculates Banzhaf values vectors of size n of a sparse fuzzy measure
                                               %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.BanzhafMob_sparse(n, envsp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{The size of values vectors}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
  }
  
  \value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    \item{output}{The output is Banzhaf values vectors of size n of a sparse fuzzy measure.}
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

    fm.BanzhafMob_sparse(3, envsp)

    envsp <- fm.FreeSparseFM(envsp)
  }
  % Add one or more standard keywords, see file 'KEYWORDS' in the
  % R documentation directory.
  \keyword{ BanzhafMob_sparse }
  \keyword{BanzhafMob}% __ONLY ONE__ keyword per line