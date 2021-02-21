\name{fm.ShowCoalitionsCard}
\alias{fm.ShowCoalitionsCard}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Show CoalitionsCard function 
}
\description{Return the decimal expression for the subsets A. In binary and in cardinality ordering respectively. 
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.ShowCoalitionsCard(env = NULL)
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
  \item{output}{The output the decimal expression for the subsets A. It is the array of
  integers containing the decimal expressions for all 2^n coalitions.}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  env<-fm.Init(3)
  CoalitionsCard <- fm.ShowCoalitionsCard(env)
  CoalitionsCard 
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ShowCoalitionsCard  }
\keyword{ShowCoalitionsCard}% __ONLY ONE__ keyword per line