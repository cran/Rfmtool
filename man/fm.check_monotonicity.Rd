\name{fm.check_monotonicity}
\alias{fm.check_monotonicity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for checking monotonicity of the set function v
}
\description{Checks monotonicity of the set function v in standard representation using insert sort.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.check_monotonicity(v, env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{matrix v stores fuzzy measurements consecutively in cardinal order.}
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
v <- fm.generate_fm_sorting(1, 1000, 0, env)
monotonicity <- fm.check_monotonicity(v, env)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{check_monotonicity}
\keyword{check_monotonicity}% __ONLY ONE__ keyword per line