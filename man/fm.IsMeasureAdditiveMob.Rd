\name{fm.IsMeasureAdditiveMob}
\alias{fm.IsMeasureAdditiveMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{IsMeasureAdditive function in Mobius representation
}
\description{
Returns 1 if yes, 0 if no; v is a fuzzy measure in Mobius representation.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.IsMeasureAdditiveMob(Mob,env)
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
fm.IsMeasureAdditiveMob(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IsMeasureAdditive }
\keyword{ IsMeasureAdditiveMob }% __ONLY ONE__ keyword per line
