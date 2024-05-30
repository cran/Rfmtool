\name{fm.generate_fm_kinteractivedualconvex}
\alias{fm.generate_fm_kinteractivedualconvex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for generating several k-interactive dual convex fuzzy measures in Mobius representation
}
\description{Generates num k-interactive dual convex fuzzy measures in Mobius representation using random walk of length markov of stepsize step.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.generate_fm_kinteractivedualconvex(num, n, kadd, markov,  step, Fn, env)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{Generated num random fuzzy measures stored consecutively in cardinality ordering in the array v.}
  \item{n}{Number of inputs.}
  \item{kadd}{kadd is the value of k-additivity, which is used for reducing the complexity of fuzzy measures. default value is kadd = n. 1 < kadd < n+1; if kdd=n - f.m. is unrestricted.}
  \item{markov}{Number of Markov steps to take, the randomness increases with that number.}
  \item{step}{The maximum size of random steps (with respect to each value). The actual step is a random value up to Step.}
  \item{Fn}{The callback function to verify any additional conditions on generated FM. Provided by the user.}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output are several k-interactive dual convex fuzzy measures in Mobius representation}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  env<-fm.Init(4)
  step <- 0.0001
  Fn <- NULL

  fuzzymeasures <- fm.generate_fm_kinteractivedualconvex(10, 4, 2, 1000, step, Fn, env)
  fuzzymeasures 
  
  env<-fm.Free(env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{generate_fm_kinteractivedualconvex}
\keyword{generate_fm_kinteractivedualconvex}% __ONLY ONE__ keyword per line
