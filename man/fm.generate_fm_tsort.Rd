\name{fm.generate_fm_tsort}
\alias{fm.generate_fm_tsort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for random generation of fuzzy measures 
}
\description{Generate several random fuzzy measures (num is their number) stored in cardinality ordering in the array v using topological sort.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.generate_fm_tsort(num, kint, markov, option, K, env = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{Generated num random fuzzy measures stored consecutively in cardinality ordering in the array}
  \item{kint}{Interactive fuzzy measure. 0 < kint <= n}
  \item{markov}{Number of Markov steps to take, the randomness increases with that number}
  \item{option}{Option = 1 employs internal rejection method to improve uniformity, but for n > 5 is is not essential}
  \item{K}{K is the constant in k-interactive fuzzy measures}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is generate several random fuzzy measures}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  env<-fm.Init(3)
  fuzzymeasures <- fm.generate_fm_tsort(10,3,1000,0,0.7, env)
  fuzzymeasures
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{generate_fm_tsort }
\keyword{generate_fm_tsort}% __ONLY ONE__ keyword per line