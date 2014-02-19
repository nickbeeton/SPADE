\name{vars.update}
\alias{vars.update}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update parameter information
%%  ~~function to do ... ~~
}
\description{
  Updates a parameter selection window's parameter information where necessary -
 saves value of edit boxes to global variables for either current time (where \code{istime = FALSE})
 or for all timesteps (where \code{istime = TRUE}), and then update edit boxes 
 with information for new current time as defined by time slider.
 Works for predator-prey, demographics and disease windows.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
vars.update(h, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
  Information about calling object - is used in this function to determine which
  window we are dealing with via the \code{id} attribute of \code{h$obj}.
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
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
