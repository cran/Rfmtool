\name{fm.generate_fm_sorting}
\alias{fm.generate_fm_sorting}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for random generation of fuzzy measures in standard representation
}
\description{Generate several random fuzzy measures in standard representation
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.generate_fm_sorting(num, markov, option, env = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{Generates num random fuzzy measures stored in an array v of length num * 2n.}
  \item{markov}{Number of Markov steps to take, the randomness increases with that number.}
  \item{option}{Option = 1 employs internal rejection method to improve uniformity, but for n > 5 is not essential.}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output are several random fuzzy measures containing in an array v of length num * 2n}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  env<-fm.Init(3)
  markovsteps <- 100
  fuzzymeasures <- fm.generate_fm_sorting(5, markovsteps, 0, env)
  fuzzymeasures
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{generate_fm_sorting}
\keyword{generate_fm_sorting}% __ONLY ONE__ keyword per line