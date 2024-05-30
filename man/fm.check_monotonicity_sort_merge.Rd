\name{fm.check_monotonicity_sort_merge}
\alias{fm.check_monotonicity_sort_merge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for checking monotonicity of the set function v
}
\description{Checks monotonicity of the set function v in standard representation using merge sort.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.check_monotonicity_sort_merge(v, indices=NULL, env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{matrix v stores fuzzy measurements consecutively in cardinal order.}
  \item{indices}{The indices can be used at subsequent steps of monotonicity verification. Initially indices need not be specified}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is a list of components (True/False, indices, values). The indices and values can be used at subsequent steps of monotonicity verification (e.g., values slightly perturbed)}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  env<-fm.Init(3)
  v <- fm.generate_fm_sorting(1, 1000, 0, env)
  v
  measure <- fm.check_monotonicity_sort_merge(v,NULL, env)

  print(measure$out)
  
    measure$V[1] = measure$V[1] *1.1
    measure <- fm.check_monotonicity_sort_merge(measure$V, measure$index, env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{check_monotonicity_sort_merge}
\keyword{check_monotonicity_sort_merge}% __ONLY ONE__ keyword per line
