\name{fm.InteractionMob}
\alias{fm.InteractionMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interaction Index computation function for Mobius fuzzy measure

}
\description{Calculates all the interaction indices of input criteria for a Mobius fuzzy measure.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.InteractionMob(Mob,env )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{fuzzy measure value in Mobius representation}
  \item{env}{ Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{the output is a matrix, whose first column stores the interaction
index values, and the second column stores the indices of criteria in coalitions.}
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
fm.InteractionMob(c( 0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interaction }
\keyword{ interactionMob }% __ONLY ONE__ keyword per line
