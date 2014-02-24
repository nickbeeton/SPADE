\name{add.strategy.handler}
\alias{add.strategy.handler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Handler for when new management strategy is added
%%  ~~function to do ... ~~
}
\description{
Updates the global \code{strategy.params} based on current GUI information,
then creates a new strategy in \code{strategy.params} with default attributes, 
and adds the strategy information to the droplist.

Note: A new strategy can only be added when carrying capacity information
is defined for the first species.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add.strategy.handler(h, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
  Calling object information - not used here but needed for object handlers
%%     ~~Describe \code{h} here~~
}
  \item{\dots}{
  Additional arguments - not used here but needed for object handlers  
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
