\name{fm.Init}
\alias{fm.Init}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Initialisation function
%%  ~~function to do ... ~~
}
\description{
This function initialises the internal structures which makes computations faster. The structures are saved
in the output environment variable, which should be subsequently passed to other functions. Several environment variables (for different dimensions)
can be initialised at the same time.
}
\usage{
fm.Init(n1)
}
\arguments{
  \item{n1}{the number of variables.}
}
\value{
\item{output}{The ouput is the enviromnet variable containing the internal structures.}
}
%- maybe also 'usage' for other objects documented here.

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
    Gleb Beliakov
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
env<-fm.Init(3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Init }


