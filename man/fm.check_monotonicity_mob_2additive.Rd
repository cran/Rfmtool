\name{fm.check_monotonicity_mob_2additive}
\alias{fm.check_monotonicity_mob_2additive}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for checking the monotonicity of the 2-additive set function v in Mobius representation.
}
\description{Check the monotonicity of the 2-additive set function v in Mobius representation using fast check.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.check_monotonicity_mob_2additive(v, n, temp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{Random 2-additive fuzzy measure in Mobius representation.}
  \item{n}{Number of inputs}
  \item{temp}{Auxiliary array of length n^2 (e.g: array(0.0,n*n)). It may or may not be specified (if speed matters, then preallocate it).}
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
  v <- fm.generate_fm_2additive(1, 10)
  n <- 10
  v$len
  v$V
  check <- fm.check_monotonicity_mob_2additive(v$V, n)
  check

  temp <- array(0.0,10*10);
  check <- fm.check_monotonicity_mob_2additive(v$V, n, temp)
  check

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{check_monotonicity_mob_2additive}
\keyword{check_monotonicity_mob_2additive}% __ONLY ONE__ keyword per line
