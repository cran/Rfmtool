\name{fm.IsMeasureSubadditive}
\alias{fm.IsMeasureSubadditive}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{IsMeasureSubadditive function 

}
\description{
Returns 1 if yes, 0 if no; v is a fuzzy measure in standard representation.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.IsMeasureSubadditive(v,env)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{the genral fuzzy measure of size m=2^n. Its values can be provided by users,
  or by estimating from empirical data.
%%     ~~Describe \code{v} here~~
}
  \item{env}{ Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The ouput is 1 if yes, 0 if no.}
}
\references{
%% ~put references to the literature/web site here ~
[1] http://www.deakin.edu.au/~gleb/fmtools.html
[2] H.Q. Vu, G. Beliakov, G. Li, 2014, 
A IsMeasureSubadditive integral toolbox and its application 
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
fm.IsMeasureSubadditive(c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env)
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IsMeasureSubadditive }

