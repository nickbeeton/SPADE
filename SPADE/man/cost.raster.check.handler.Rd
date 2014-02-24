\name{cost.raster.check.handler}
\alias{cost.raster.check.handler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Switches between spatially homogeneous or heterogeneous parameter values
%%  ~~function to do ... ~~
}
\description{
Defines whether the strategy uses rasters of spatially varying values for set parameters, 
or a single spatially homogeneous value. The parameters this applies to are Target Density
(\code{TD}), Cost Slope (\code{CS}), Cost Intercept (\code{CI}) and Hourly Cost (\code{CH}).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cost.raster.check.handler(h, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
    Information about calling object - here used to gauge value of checkbox as
    checked or unchecked 
    by examining the \code{svalue} attribute of \code{h$obj}.
%%     ~~Describe \code{h} here~~
}
  \item{\dots}{
  Additional arguments - not used here.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
