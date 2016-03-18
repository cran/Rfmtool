\name{fm.Mobius}
\alias{fm.Mobius}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mobius transform function

}
\description{calculates Mobius representation of general fuzzy measure,
the input and output is an array of size 2^n=m in binary ordering.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.Mobius(v,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{fuzzy measure value in standard representation.}
  \item{env}{ Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{the output is the fuzzy measure in Mobius representation.}
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
##---- Should be DIRECTLY executable !! ----
env<-fm.Init(3)
fm.Mobius(c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Mobius }

