\name{ff.handler}
\alias{ff.handler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Handler for when a functional form is selected
%%  ~~function to do ... ~~
}
\description{
Deals with GUI elements in the current settings window: 
defines droplist labels, whether edit boxes are editable, and sets variables
to default where necessary.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ff.handler(h, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
  Calling object information - used here to determine which window the handler
  is called from by checking the \code{id} attribute of the droplist \code{h$obj}
  that called this function.
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
