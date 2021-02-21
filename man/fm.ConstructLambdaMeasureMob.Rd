\name{fm.ConstructLambdaMeasureMob}
\alias{fm.ConstructLambdaMeasureMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for Constructing Lambda in Mobius representation

}
\description{
Finds the value of lambda and calculates the rest of the values of the fuzzy measure,
given its values at singletons; singletons is an array of size n. The outputs are lambda and measure, measure is in Mobius representation. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.ConstructLambdaMeasureMob(singletons,env)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{singletons}{Singletons is an array of n.
%%     ~~Describe \code{singletons} here~~
}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The ouput is the list (lambda, measure), where measure is a fuzzy measure in Mobius representation.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}
\examples{
env<-fm.Init(3)
w <- fm.ConstructLambdaMeasureMob(c(0, 0.3, 0.5),env)
w$measure 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ConstructLambdaMeasure }
\keyword{ ConstructLambdaMeasureMob }% __ONLY ONE__ keyword per line

