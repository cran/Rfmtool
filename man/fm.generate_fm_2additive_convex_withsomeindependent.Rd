\name{fm.generate_fm_2additive_convex_withsomeindependent}
\alias{fm.generate_fm_2additive_convex_withsomeindependent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for generating 2additive convex fuzzy measures with some independent inputs
}
\description{Generates num 2-additive convex (supermodular) fuzzy measures for n inputs. Some of the interaction indices are set to 0 (independence).
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.generate_fm_2additive_convex_withsomeindependent(num, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{Generates num convex random fuzzy measures stored consecutively in cardinality ordering in the array}
  \item{n}{Number of inputs}
  
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is the length of the part of the array v allocated for each fuzzy measure, and the array with singletons and pairs in Mobius representation}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{

  addconvex <- fm.generate_fm_2additive_convex_withsomeindependent(5,20)

  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{generate_fm_2additive_convex_withsomeindependent}
\keyword{generate_fm_2additive_convex_withsomeindependent}% __ONLY ONE__ keyword per line