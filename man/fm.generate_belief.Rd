\name{fm.generate_belief}
\alias{fm.generate_belief}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for random generation of belief fuzzy measures in standard representation
}
\description{Generate several random k-additive belief measures in Mobius representation.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  fm.generate_belief(num, kadd, env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{Generates num random belief measures stored in an array Mob of length num *  fm_arraysize_kadd(n, kadd).}
   \item{kadd}{k-additivity}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The output are several random belief measures containing in an array v of length num * fm_a}
}

\author{
  %%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{ 
  env<-fm.Init(5)
  belief  <- fm.generate_belief(2, 3, env)
  # 2 3-additive measures with n=5
  belief
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{generate_belief}
\keyword{generate_balanced}% __ONLY ONE__ keyword per line
