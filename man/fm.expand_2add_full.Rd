\name{fm.expand_2add_full}
\alias{fm.expand_2add_full}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for exporting full representation of 2-additive capacity
}
\description{From sparse to full representation of 2-additive capacity (singletons and pairs, augmented with 0s). 
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.expand_2add_full(n, envsp=NULL)
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
  \item{output}{The output is a sparse to full representation of 2-additive capacity (singletons and pairs, augmented with 0s)}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
  n <- 3
  envsp <- fm.PrepareSparseFM(n, vector(), vector()) 
  envsp <- fm.add_singletons_sparse(c(0.2,0.1,0.2),envsp) 
  envsp <- fm.add_pair_sparse(1,2,0.4,envsp);

  cap2add <- fm.expand_2add_full(n,envsp)
  cap2add 

  envsp <- fm.FreeSparseFM(envsp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ expand_2add_full  }
\keyword{expand_2add_full}% __ONLY ONE__ keyword per line