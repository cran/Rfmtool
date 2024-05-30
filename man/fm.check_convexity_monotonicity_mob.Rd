\name{fm.check_convexity_monotonicity_mob}
\alias{fm.check_convexity_monotonicity_mob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for checking  supermodularity of the set function v in Mobius representation
}
\description{Checks supermodularity of the set function v in Mobius representation using stan- dard check.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.check_convexity_monotonicity_mob(v, len, env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{matrix v stores fuzzy measurements consecutively in cardinal order v.}
  \item{len}{this is the length of array Mob (this array is usually smaller than 2^n), and is computed by fm.fm_arraysize_kadd(N, Kadd).}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is 1 or 0 to check for monotonicity.}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
env<-fm.Init(3)
step <- 0.001
Fn <- NULL
Option<- 3
fuzzymeasures <- fm.generate_fm_randomwalk(1, 3, 2, 1000, Option, step, Fn, env)
len <- fuzzymeasures$length
check <- fm.check_convexity_monotonicity_mob(fuzzymeasures$V, len, env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{check_convexity_monotonicity_mob}
\keyword{check_convexity_monotonicity_mob}% __ONLY ONE__ keyword per line
