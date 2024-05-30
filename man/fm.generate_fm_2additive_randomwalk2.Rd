\name{fm.generate_fm_2additive_randomwalk2}
\alias{fm.generate_fm_2additive_randomwalk2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for generating random 2-additive fuzzy measures in Mobius representation by using random walk.
}
\description{Generate a random 2-additive fuzzy measures in Mobius representation by using random walk.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.generate_fm_2additive_randomwalk2(num, n, markov, option, step, Fn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{Generated num random fuzzy measures stored consecutively in cardinality ordering in the array v.}
  \item{n}{Number of inputs.}
  \item{markov}{Number of Markov steps to take, the randomness increases with that number.}
  \item{option}{Not used, reserved for future use.}
  \item{step}{The maximum size of random steps (with respect to each value). The actual step is a random value up to Step.}
  \item{Fn}{The callback function to verify any additional conditions on generated FM. Provided by the user or NULL.}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output are random 2-additive fuzzy measure, it contains singletons and pairs but no emptyset.}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  num <- 10
  n <- 5
  fuzzymeasures <- fm.generate_fm_2additive_randomwalk2(num, n, 1000, 0, 0.001, NULL)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{generate_fm_2additive_randomwalk2}
\keyword{generate_fm_2additive_randomwalk2}% __ONLY ONE__ keyword per line
