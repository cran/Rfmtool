\name{fm.Choquet2addMob}
\alias{fm.Choquet2addMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for calculating Choquet integral value for 2-additive fuzzy measure in Mobius representation
}
\description{Calculates the Choquet integral value of a 2-additive  fuzzy measure for n inputs given in Mobius representation.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.Choquet2addMob(n, x, Mob)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of inputs} 
   \item{x}{Input vector of size n, containing utility value of input criteria.
    x is in [0,1].}
 \item{Mob}{The Mobius fuzzy measure of size m=2^n. Its values can be provided by users,
  or by estimating from empirical data.}
  }

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is the Choquet integral value in Mobius representation.}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  Choquet <- fm.Choquet2addMob(3, c(0.2,0.5,0.4), c(0.2, 0.3, 0.5, -0.2, 0.4, 0.1))
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Choquet2addMob  }
\keyword{Choquet2addMob}% __ONLY ONE__ keyword per line