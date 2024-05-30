\name{fm.BanzhafMob}
\alias{fm.BanzhafMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Banzhaf value computation function in Mobius representation
}
\description{Calculates the Banzhaf indices of input criteria from general fuzzy measure in Mobius representation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.BanzhafMob(Mob,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{Fuzzy measure in Mobius representation.}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The output is an array of size n, which contain Banzhaf indices of input criteria.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
env<-fm.Init(3)
fm.BanzhafMob(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)
env<-fm.Free(env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Banzhaf }
\keyword{ BanzhafMob }
\keyword{ Banzhaf }% __ONLY ONE__ keyword per line
