\name{fm.generate_fm_randomwalk}
\alias{fm.generate_fm_randomwalk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for generating several k-additive fuzzy measure
}
\description{Generates num k-additive fuzzy measures in the standard or Mobius representation using random walk of length markov of stepsize step.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.generate_fm_randomwalk(num, n, kadd, markov, option, step, Fn, env)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{Generated num random fuzzy measures stored consecutively in standard or cardinality ordering in the array v.}
  \item{n}{Number of inputs}
  \item{kadd}{kadd is the value of k-additivity, which is used for reducing the complexity of fuzzy measures. default value is kadd = n. 1 < kadd < n+1; if kdd=n - f.m. is unrestricted. The parameter kadd only matters for options 3 and 5}
  \item{markov}{Number of Markov steps to take, the randomness increases with that number.}
  \item{option}{ Option = 0 - normal, 1 convex (supermodular), 2 antibuoyant, 3 kadditive , 4 belief measure, 5 kadditive convex. The measure generated is in standard representation fo all options except 3,5. The parameter kadd only matters for options 3 and 5. In that case the measure is in more compact Mobius representation.}
  \item{step}{The maximum size of random steps (with respect to each value). The actual step is a random value up to Step.}
  \item{Fn}{The callback function to verify any additional conditions on generated FM. Provided by the user. if not NULL, is a callback function to perform additional check at every Markov step of the current set function, i.e., any extra conditions}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output is named list with the first element v being the fuzzy measure and the second being the length of the array containing it}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
   Fn <- function(n,v){
        out <- 0.0

        for(i in 1:n) out<- out+v[i];
        
        if(out>1) {
        return(0)
        } else
        return(1)
    }
    env<-fm.Init(3)
    step <- 0.0010
    Option <- 3
    n <- 3

  fuzzymeasures <- fm.generate_fm_randomwalk(2, 3, 2, 1000, Option, step, Fn, env)
  print(fuzzymeasures)
  print(fuzzymeasures$length)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{generate_fm_randomwalk}
\keyword{generate_fm_randomwalk}% __ONLY ONE__ keyword per line
