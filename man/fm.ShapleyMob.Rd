\name{fm.ShapleyMob}
\alias{fm.ShapleyMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Shapley value computation function 
}
\description{Calculates the Shapley values of input criteria from Mobius fuzzy measure,
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.ShapleyMob(Mob,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{is fuzzy measure in Mobius representation.}
  \item{env}{ Environment variable obtained from fm.Init(n).}
}

\value{
\item{output}{The output is an array of size n, which contain Shapley values of input criteria.}
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
fm.ShapleyMob(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Shapley }

