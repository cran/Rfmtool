\name{fm.dualmMob}
\alias{fm.dualmMob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dualm computation function in Mobius representation

}
\description{
Calculates the dual of fuzzy measure v, returns it as value of the function (array of size m).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.dualmMob(Mob,env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{Mobius fuzzy measure of size m=2^n. Its values can be provided by users, or by estimating from emperical data.
%%     ~~Describe \code{Mob} here~~
}
  \item{env}{Environment variable obtained from fm.Init(n).}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{output}{The ouput is an array of size m with the dual of fuzzy measure.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}

\examples{
env<-fm.Init(3)
w <- fm.dualmMob(c(0.0, 0.3, 0.5, -0.2, 0.4, 0.1, -0.2, 0.1),env)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dual }
