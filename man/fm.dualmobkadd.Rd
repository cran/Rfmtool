\name{fm.dualMobKadd}
\alias{fm.dualMobKadd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for calculating dual of k-additive fuzzy measure in Mobius representation
}
\description{Calculates the dual of a k-additive fuzzy measures for n inputs.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.dualMobKadd(Mob, env = NULL, kadd = "NA")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{Mobius fuzzy measure of size m=2^n. Its values can be provided by users,
  or by estimating from empirical data.}
  \item{env}{Environment variable obtained from fm.Init(n).}
  \item{kadd}{Value of k-interactivity, which is used for reducing the complexity of fuzzy measures. It is defined as an optional argument} 
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is the dual of a k-additive fuzzy measures for n inputs}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
env<-fm.Init(3)
  dualMob_Kadd <- fm.dualMobKadd(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1), env,2)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dualMobKadd  }
\keyword{dualMobKadd}% __ONLY ONE__ keyword per line