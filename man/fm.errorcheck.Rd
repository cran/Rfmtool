\name{fm.errorcheck}
\alias{fm.errorcheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Basic error check
%%  ~~function to do ... ~~
}
\description{
This function checks that the enviromnemt variable is internally consistent.
}
\usage{
fm.errorcheck(env)
}
\arguments{
  \item{env}{Environment variable obtained from fm.Init(n).}
}
\value{
%- maybe also 'usage' for other objects documented here.
\item{output}{The ouput is TRUE or FALSE.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
env<-fm.Init(3)
fm.errorcheck(env)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ errorcheck }


