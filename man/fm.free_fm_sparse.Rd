\name{fm.FreeSparseFM}
\alias{fm.FreeSparseFM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{FreeSparseFM function 
}
\description{Frees the memory previously allocated in envsp.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.FreeSparseFM(envsp)
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
  \item{output}{Frees the memory previously allocated in envsp.}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
     n<-3
    envsp <- fm.PrepareSparseFM(n, vector(), vector()) 
    envsp <- fm.FreeSparseFM(envsp)
    envsp <- fm.PrepareSparseFM(n, c(0.2,0.4,0.1), c(2,1,2,2,1,3,3,1,2,3)) 
     envsp <- fm.FreeSparseFM(envsp) 
envsp
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{FreeSparseFM}
\keyword{FreeSparseFM}% __ONLY ONE__ keyword per line