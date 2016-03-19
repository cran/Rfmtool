\name{fm.ConstructLambdaMeasure}
\alias{fm.ConstructLambdaMeasure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ConstructLambdaMeasure function 
}
\description{
Finds the value of lambda and calculates the rest of the values of the fuzzy measure,
given its values at singletons; singletons is an array of size n.
The outputs are lambda and v, v is in standard representation and binary ordering. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.ConstructLambdaMeasure(singletons,env)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{singletons}{singletons is an array of n.
%%     ~~Describe \code{singletons} here~~
}
  \item{env}{ Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The ouput is the list (lambda, measure), where measure is a fuzzy measure in standard representation.}
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
w <- fm.ConstructLambdaMeasure(c(0, 0.3, 0.5),env)
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ConstructLambdaMeasure }

