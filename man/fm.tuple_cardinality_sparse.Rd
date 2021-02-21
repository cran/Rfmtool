\name{fm.tuple_cardinality_sparse}
\alias{fm.tuple_cardinality_sparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tuple cardinality  
}
\description{Returns the cardinality of the tuple numbered i in the list of tuples.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.tuple_cardinality_sparse(i, envsp = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{In the list of tuples.}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is the cardinality of the tuple numbered i in the list of tuple.}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
    n <- 3
    envsp <- fm.PrepareSparseFM(n, c(0.2,0.4,0.1), c(2,1,2,2,1,3,3,1,2,3)) 
    fm.tuple_cardinality_sparse(0,envsp)
     envsp <- fm.FreeSparseFM(envsp) 
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ tuple_cardinality_sparse}
\keyword{ tuples }% __ONLY ONE__ keyword per line