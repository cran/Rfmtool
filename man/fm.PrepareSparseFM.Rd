\name{fm.PrepareSparseFM}
\alias{fm.PrepareSparseFM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PrepareSparseFM preparation function 
}
\description{This function initialises Sparse representation structure. It is used to allocate storage and later populate these values 
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.PrepareSparseFM(n, tups, tupsidx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of inputs}
 \item{tups}{Tuples to be added (can be null vector)}   
\item{tupsidx}{Cardinalities and indices (1-based) of the elements of tuples (can be null vector)} 
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output allocate storage and later populate these values. envsp}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
   n<-3
    envsp <- fm.PrepareSparseFM(n, vector(), vector()) 
    envsp <- fm.FreeSparseFM(envsp)
    envsp <- fm.PrepareSparseFM(n, c(0.2,0.4,0.1), c(2,1,2,2,1,3,3,1,2,3)) 
envsp
     envsp <- fm.FreeSparseFM(envsp) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{PrepareSparseFM}
\keyword{PrepareSparseFM}% __ONLY ONE__ keyword per line