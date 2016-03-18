\name{fm.OrnessChoquetMob}
\alias{fm.OrnessChoquetMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{OrnessChoquetMob function 

}
\description{
Calculates orness value of the Choquet integral of the fuzzy measure, where Mob is the Mobius representation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.OrnessChoquetMob(Mob,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{the Mobius fuzzy measure of size m=2^n. Its values can be provided by users,
  or by estimating from empirical data.}
  \item{env}{ Environment variable obtained from fm.Init(n).}
%%     ~~Describe \code{Mob} here~~
}
\value{
\item{output}{the ouput is the orness the Choquet integral for the fuzzy measure.}
}
\references{
%% ~put references to the literature/web site here ~
[1] http://www.deakin.edu.au/~gleb/fmtools.html
[2] H.Q. Vu, G. Beliakov, G. Li, 2014, 
A choquet integral toolbox and its application 
in customer preference analysis, in Data mining
applications with R, Elsevier Inc., Waltham, 
MA., pp.247-272.
}
\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Deakin University
}
\examples{

env<-fm.Init(3)
fm.OrnessChoquetMob(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Orness }


