\name{fm.OrnessChoquetMob}
\alias{fm.OrnessChoquetMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{OrnessChoquet function in Mobius representation 

}
\description{
Calculate Orness value of the Choquet integral of the fuzzy measure, where Mob is the Mobius representation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.OrnessChoquetMob(Mob,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{Mobius fuzzy measure of size m=2^n. Its values can be provided by users,
  or by estimating from empirical data.}
  \item{env}{Environment variable obtained from fm.Init(n).}
%%     ~~Describe \code{Mob} here~~
}
\value{
\item{output}{The output is the Orness the Choquet integral for the fuzzy measure.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}
\examples{

env<-fm.Init(3)
fm.OrnessChoquetMob(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Orness }


