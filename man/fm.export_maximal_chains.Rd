\name{fm.export_maximal_chains}
\alias{fm.export_maximal_chains}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for exporting maximal chains
}
\description{Returns in mc the arrays of maximal chains (there are n! such arrays) of a fuzzy measure v. Each maximal chain corresponds to the coefficients of a linea. function on the respective simplex
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.export_maximal_chains(v, env = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{Fuzzy measure in general representation.} 
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is mc the arrays of maximal chains}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  env<-fm.Init(3)
  exportmaximalchains <- fm.export_maximal_chains(
   c(0, 0.00224, 0.0649, 0.510, 0.00965, 0.374,0.154, 1),env)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{export_maximal_chains}
\keyword{export_maximal_chains}% __ONLY ONE__ keyword per line