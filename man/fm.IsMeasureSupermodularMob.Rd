\name{fm.IsMeasureSupermodularMob}
\alias{fm.IsMeasureSupermodularMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{IsMeasureSupermodular function in Mobius representation

}
\description{
Returns 1 if yes, 0 if no; Mob is a fuzzy measure in Mobius representation.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.IsMeasureSupermodularMob(Mob,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{Mobius fuzzy measure of size m=2^n. Its values can be provided by users,
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
\item{output}{The output is 1 if yes, 0 if no.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
env<-fm.Init(3)
fm.IsMeasureSupermodularMob(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IsMeasureSupermodular }
\keyword{ IsMeasureSupermodularMob }% __ONLY ONE__ keyword per line

