\name{fm.ChoquetMob_sparse}
\alias{fm.ChoquetMob_sparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Choquet integral computation function in sparse representation 
}
\description{Calculates the Choquet integral in Mobius sparse representation.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.ChoquetMob_sparse(x, envsp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Input vector of size n, containing utility value of input criteria. x is in [0,1].}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
  }

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is the Choquet integral in Mobius sparse representation.}
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

   ChoquetMobsparse <- fm.ChoquetMob_sparse(c(0.1,0.05,0.2),envsp)
   ChoquetMobsparse 
   envsp <- fm.FreeSparseFM(envsp)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ChoquetMob_sparse  }
\keyword{Choquet}% __ONLY ONE__ keyword per line