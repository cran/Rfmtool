\name{fm.ChoquetCoMobKInter}
\alias{fm.ChoquetCoMobKInter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{function for calculating Choquet integral value with respect to dual k-interactive fuzzy measure in Mobius representation

}
\description{Calculates the Choquet integral of x with respect to dual k-interactive fuzzy measure in Mobius representation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.ChoquetCoMobKInter(x, Mob, kadd, env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Input vector of size n, containing utility value of input criteria. x is in [0,1].}
  \item{Mob}{The Mobius fuzzy measure of size m=2^n. Its values can be provided by users, or by estimating from empirical data}
  \item{kadd}{is the value of k-additivity, which is used for reducing the complexity of fuzzy measures. kadd is defined as an optional argument, 
  its default value is kadd = n. kadd is k in k-additive f-measure, 1 < kadd < n+1; if kdd=n - f.m. is unrestricted.}
 \item{env}{Environment variable obtained from fm.Init(n).}

}

\value{
\item{output}{The output is the Choquet integral value in Mobius representation.}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}
\examples{
env <-fm.Init(4)
step <- 0.0001
Fn <- NULL
fuzzymeasures <- fm.generate_fm_kinteractivedualconvex(1, 4, 2, 1000, step, Fn, env)
fuzzymeasures
env

fm.ChoquetCoMobKInter(c(0.2,0.5,0.4,0.1), fuzzymeasures$V, 2, env)
env<-fm.Free(env)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ fitting }
\keyword{ ChoquetCoMobKInter }% __ONLY ONE__ keyword per line
