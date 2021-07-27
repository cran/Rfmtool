\name{fm.InteractionB}
\alias{fm.InteractionB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Banzhaf Interaction Index computation function 

}
\description{Calculates all the Banzhaf Interaction indices of input criteria for a standard fuzzy measure.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.InteractionB(v,env)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{Fuzzy measure value in standard representation}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The output is a matrix, whose first column stores the Banzhaf Interaction index values, and the second column stores the indices of criteria in coalitions.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{

env<-fm.Init(3)
fm.InteractionB(c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ InteractionB }
\keyword{ InteractionBMob }% __ONLY ONE__ keyword per line
