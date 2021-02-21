\name{fm.generate_fm_kadditive_convex_sparse}
\alias{fm.generate_fm_kadditive_convex_sparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate kadditive convex sparse fuzzy measures 
}
\description{Generates a random k-additive Belief fuzzy measure in sparse representation
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.generate_fm_kadditive_convex_sparse(n, kadd, nonzero, envsp = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Inputs length. (n inputs)}
  \item{kadd}{kadd is the value of k-additivity, which is used for reducing the complexity of fuzzy measures. default value is kadd = n. 1 < kadd < n+1; if kdd=n - f.m. is unrestricted}
  \item{nonzero}{Values stored and indexed in the respective arrays which are part of the structure}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
  }

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is k-additive Belief fuzzy measure in sparse representation}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
    n <- 5
  tups<-vector()
  tupsidx<-vector()
  envsp <- fm.PrepareSparseFM(n, tups,tupsidx) 
  envsp <- fm.generate_fm_kadditive_convex_sparse(n,4,10, envsp)
  envsp
     envsp <- fm.FreeSparseFM(envsp) 
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ generate_fm_kadditive_convex_sparse }
\keyword{generate_fm_kadditive_convex_sparse}% __ONLY ONE__ keyword per line