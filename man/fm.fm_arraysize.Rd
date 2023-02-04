\name{fm.fm_arraysize}
\alias{fm.fm_arraysize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for returning the length of the array 
}
\description{Returns the length of the array of values of k-interactive fuzzy measures. Useful for reserving memory. 
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.fm_arraysize(env = NULL, kint = "NA")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{env}{Environment variable obtained from fm.Init(n).}
  \item{kint}{Interactive fuzzy measure. 0 < kint <= n}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The outputs is the length of the array of values of k-interactive fuzzy measures}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  env<-fm.Init(3)
  arraylength <- fm.fm_arraysize(env,1)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ fm_arraysize }
\keyword{arraysize}% __ONLY ONE__ keyword per line