\name{fm.ChoquetMob}
\alias{fm.ChoquetMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Choquet integral value computation function in Mobius representation

}
\description{
This is an alternative calculation of the Choquet integral from the fuzzy measure in Mobius representation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.ChoquetMob(x, Mob, env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Input vector of size n, containing utility value of input criteria.
  x is in [0,1].
%%     ~~Describe \code{x} here~~
}
  \item{Mob}{The Mobius fuzzy measure of size m=2^n. Its values can be provided by users,
  or by estimating from empirical data.
%%     ~~Describe \code{Mob} here~~
}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The ouput is a single value of the computed Choquet integral.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
env<-fm.Init(3)
fm.ChoquetMob(c(0.2,0.5,0.4), c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Choquet }

