\name{fm.generate_fm_2additive}
\alias{fm.generate_fm_2additive}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for generating 2-additive fuzzy measures in Mobius representation}
\description{Generates num random 2-additive fuzzy measures in Mobius representation.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.generate_fm_2additive(num, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{Generates num random fuzzy measures stored consecutively in cardinality ordering in the array v.}
  \item{n}{Number of inputs}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output are random fuzzy measures, it contains singletons and pairs but no emptyset}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
    

num <- 2
n <- 5
fuzzymeasures <- fm.generate_fm_2additive(num,n)
fuzzymeasures$V
fuzzymeasures$len

  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{generate_fm_2additive}
\keyword{generate_fm_2additive}% __ONLY ONE__ keyword per line