\name{fm.Shapley2addMob}
\alias{fm.Shapley2addMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for calculating Shapley values of 2-additive fuzzy measure in Mobius representation
}
\description{Calculate the Shapley values  of a 2-additive  fuzzy measure for n inputs given in Mobius representation. The results are in arrays.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.Shapley2addMob(n, Mob)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of inputs}
  \item{Mob}{Fuzzy measure value in Mobius representation}
  }

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is an array of size n, which contain Shapley indices of input criteria.}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
   Shapley <- fm.Shapley2addMob(3, c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1))
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Shapley2addMob }
\keyword{Shapley}% __ONLY ONE__ keyword per line