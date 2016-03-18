\name{fm.OrnessChoquet}
\alias{fm.OrnessChoquet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{OrnessChoquet function 

}
\description{
Calculates orness value of the Choquet integral of the fuzzy measure, where v is a standard representation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.OrnessChoquet(v,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{a standard fuzzy measure of size m=2^n. Its values can be provided by users,
  or by estimating from empirical data.}
  \item{env}{ Environment variable obtained from fm.Init(n).}
%%     ~~Describe \code{v} here~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
fm.OrnessChoquet(c(0, 0.3, 0.5, 0.6, 0.4, 0.8, 0.7, 1),env)
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ OrnessChoquet }
\keyword{ orness }% __ONLY ONE__ keyword per line

