\name{fm.InteractionBMob}
\alias{fm.InteractionBMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Banzhaf InteractionB Index computation function in Mobius representation
}
\description{Calculates all the Banzhaf InteractionB indices of input criteria for a Mobius fuzzy measure.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.InteractionBMob(Mob,env)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{Fuzzy measure value in Mobius representation}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The output is a matrix, whose first column stores the Banzhaf Interaction
index values, and the second column stores the indices of criteria in coalitions.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{

env<-fm.Init(3)
fm.InteractionBMob(c( 0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ InteractionB }
\keyword{ InteractionBMob }% __ONLY ONE__ keyword per line
