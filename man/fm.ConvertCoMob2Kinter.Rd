\name{fm.ConvertCoMob2Kinter}
\alias{fm.ConvertCoMob2Kinter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for dual k-interactive fuzzy measure from Mobius to standard representation

}
\description{Converts dual k-interactive fuzzy measure from Mobius to standard representation. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fm.ConvertCoMob2Kinter(Mob,kadd, fullmu, env=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mob}{Mobius fuzzy measure of size m=2^n. Its values can be provided by users, or by estimating from emperical data.}
  \item{kadd}{is the value of k-additivity, which is used for reducing the complexity of fuzzy measures. kadd is defined as an optional argument, 
  its default value is kadd = n. kadd is k in k-additive f-measure, 1 < kadd < n+1; if kdd=n - f.m. is unrestricted.}
 \item{fullmu}{Integer flag. is 1 then all 2n are allocated, otherwise a more compact representation fo rk-interactive fuzzy measures is used.}
 \item{env}{Environment variable obtained from fm.Init(n).}

}

\value{
\item{output}{The output is k-interactive fuzzy measure standard representation}
}

\author{
%%  ~~who you are~~
    Gleb Beliakov, Andrei Kelarev, Quan Vu, Daniela L. Calderon, Deakin University
}
\examples{
env <-fm.Init(4)
fullmu <- 0
step<-0.001
Fn <- NULL
        fuzzymeasures <- fm.generate_fm_kinteractivedualconvex(1, 4, 2, 1000,
        step, Fn, env)

fm.ConvertCoMob2Kinter(fuzzymeasures$V, 2, fullmu, env )
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ fitting }
\keyword{ ConvertCoMob2Kinter }% __ONLY ONE__ keyword per line
