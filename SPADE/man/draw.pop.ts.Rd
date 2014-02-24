\name{draw.pop.ts}
\alias{draw.pop.ts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw a raster of the population in each cell at a given point in time
%%  ~~function to do ... ~~
}
\description{
Using \code{plotdata}, draws a raster of the population in each cell at a given 
point in time on the Pop TS area in the SPADE GUI.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw.pop.ts(t)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{
  An integer describing which frame of the simulation to plot, where \code{1} is the 
  initial condition, and \code{1 + duration * seasons} is the last frame.
%%     ~~Describe \code{t} here~~
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
\code{\link{plotdata}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
