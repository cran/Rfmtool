\name{fm.Free}
\alias{fm.Free}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{FreeSparseFM function 
}
\description{Frees the memory previously allocated in env.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.Free(env)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{env}{Structure required for auxiliary data. It is obtained from fm.Init(n).}
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
    env <- fm.Init(n)
    env<-fm.Free(env)
    env

  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Free}
\keyword{FreeSparseFM}% __ONLY ONE__ keyword per line
