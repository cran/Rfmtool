\name{fm.generate_fmconvex_tsort}
\alias{fm.generate_fmconvex_tsort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for generating convex fuzzy measures
}
\description{Generates num convex random fuzzy measures stored consecutively in cardinality ordering in the output array.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.generate_fmconvex_tsort(num, kint, markov, option, K, env = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{Several random fuzzy measures stored in cardinality ordering in the array v (num is their number)}
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
  \item{output}{The output is the generation of num convex random fuzzy measures stored consecutively in cardinality ordering in the array v}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  env<-fm.Init(3)
  fuzzymeasures <- fm.generate_fmconvex_tsort(1,3,1000,0,1, env)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{generate_fmconvex_tsort}
\keyword{generate_fmconvex_tsort}% __ONLY ONE__ keyword per line