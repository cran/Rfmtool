\name{fm.generate_antibuoyant}
\alias{fm.generate_antibuoyant}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for generating one antibuoyant random fuzzy measure
}
\description{Generates one antibuoyant random fuzzy measure in standard representation.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.generate_antibuoyant(env = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is one antibuoyant random fuzzy measure.}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  env<-fm.Init(3)
  fuzzymeasures <- fm.generate_antibuoyant(env)
  fuzzymeasures 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{generate_antibuoyant}
\keyword{generate_antibuoyant}% __ONLY ONE__ keyword per line