\name{fm.add_tuple_sparse}
\alias{fm.add_tuple_sparse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for adding singletons to the sparse fuzzy measure
}
\description{This is used for populating capacities which Add a tuple of size tupsize to the structure whose Indices are 1-based in tuple.

For populating capacities, adds a  whose 1-based indices are in tuple
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.add_tuple_sparse( tuple, v, envsp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tuple}{Collection of objects. It is a list of cardinalities of the nonzero tuples (cardinality, tuple composition)}
  \item{v}{The value of the tuple to be added}
  \item{envsp}{Structure required for sparse representation which stores the relevant values (k-tuples). It is obtained from fm.PrepareSparseFM(n).}
  }
  
  \value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    \item{output}{The output is adding a tuple of size tupsize}
  }
 
\author{
    %%  ~~who you are~~
      Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
  }
  
\examples{
    n <- 4
  tups<-vector()
  tupsidx<-vector()
  envsp <- fm.PrepareSparseFM(n, tups,tupsidx) 
  envsp <- fm.add_tuple_sparse(c(1,2,3),0.2,envsp)
  envsp <- fm.add_tuple_sparse(c(1,3,4),0.3,envsp)
  }
  % Add one or more standard keywords, see file 'KEYWORDS' in the
  % R documentation directory.
  \keyword{add_tuple_sparse}
  \keyword{add_tuple}% __ONLY ONE__ keyword per line