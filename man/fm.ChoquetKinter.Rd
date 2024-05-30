\name{fm.ChoquetKinter}
\alias{fm.ChoquetKinter}

\title{Choquet integral value computation function in standard representation wrt  k-interactive fuzzy measure
}

\description{
This is an alternative calculation of the Choquet integral from the fuzzy measure in Mobius representation.
}
\usage{
fm.ChoquetKinter(x, v, kint, env)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Input vector of size n, containing utility value of input criteria.
  x is in [0,1].
}
  \item{v}{The  fuzzy measure of size less than m=2^n. Its values can be provided by users, or by estimating from empirical data.}
  \item{kint}{the k-interactivity parameter, must be smaller than n.}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
\item{output}{The ouput is a single value of the computed Choquet integral.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
env<-fm.Init(3)
fm.ChoquetKinter(c(0.6,0.3,0.8),c(0,0.3,0.5,0.6,0.4,0.8,0.7,1),2,env)
env<-fm.Free(env)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ChoquetKinter }

