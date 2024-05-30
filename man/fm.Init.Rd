\name{fm.Init}
\alias{fm.Init}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Initialisation function
%%  ~~function to do ... ~~
}
\description{
This function initialises the internal structures which makes computations faster. The structures are saved in the output environment variable, which should be subsequently passed to other functions. Several environment variables (for different dimensions) can be initialised at the same time.
}
\usage{
fm.Init(n1)
}
\arguments{
  \item{n1}{The number of variables.}
}
\value{
\item{output}{The ouput is the enviromnet variable containing the internal structures.}
}
%- maybe also 'usage' for other objects documented here.


\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
env<-fm.Init(3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Init }


