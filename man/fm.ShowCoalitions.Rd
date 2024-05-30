\name{fm.ShowCoalitions}
\alias{fm.ShowCoalitions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Show Coalitions function 
}
\description{Return the decimal expression for the subsets A. In binary and in cardinality ordering respectively.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.ShowCoalitions(env = NULL)
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
  \item{output}{ is the array of
  integers which show the decimal expressions for all 2^n coalitions.}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  env<-fm.Init(3)
  ShowCoalitions <- fm.ShowCoalitions(env)
  ShowCoalitions 
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ShowCoalitions  }
\keyword{ShowCoalitions}% __ONLY ONE__ keyword per line