\name{fm.IsMeasureKmaxitiveMob}
\alias{fm.IsMeasureKmaxitiveMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{IsMeasureKmaxitive function in Mobius representation

}
\description{
Returns k; mob is a fuzzy measure in Mobius representation.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.IsMeasureKmaxitiveMob(Mob,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{Mobius fuzzy measure of size m=2^n. Its values can be provided by users, or
by estimating from emperical data.
%%     ~~Describe \code{v} here~~
}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The output is k. If k=n then not k-maxitive}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}
\examples{

env<-fm.Init(3)
fm.IsMeasureKmaxitiveMob(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IsMeasureKmaxitiveMob}


