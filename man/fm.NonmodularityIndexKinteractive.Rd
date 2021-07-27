\name{fm.NonmodularityIndexKinteractive}
\alias{fm.NonmodularityIndexKinteractive}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{NonmodularityIndexKinteractive computation function 
}
\description{Calculate all the m = 2^n nonmodularity indices of k-interactive fuzzy measure v
given in standard representation (in cardinality ordering) 
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.NonmodularityIndexKinteractive(v, env = NULL, kadd = "NA")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{Fuzzy measure in general representation.}
  \item{env}{Environment variable obtained from fm.Init(n).}
  \item{kadd}{kadd is the value of k-additivity, which is used for reducing the complexity of fuzzy measures. default value is kadd = n. 1 < kadd < n+1; if kdd=n - f.m. is unrestricted}
 
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is an array of size m.}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  env<-fm.Init(3)
  fm.NonmodularityIndexKinteractive(c(0,0.3,0.5,0.6,0.4,0.8,0.7,1),env,2)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ NonmodularityIndexKinteractive }
\keyword{NonmodularityIndexKinteractive}% __ONLY ONE__ keyword per line