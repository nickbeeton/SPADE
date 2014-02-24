\name{update.elements}
\alias{update.elements}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update the \code{interactions} global variable
%%  ~~function to do ... ~~
}
\description{
 Updates the \code{interactions} variable, which contains the intra- and inter-species interaction 
 information required by the SPADE engine, based on the values of global variables
 (which have in turn been set using the GUI elements).
 This function is run upon the exit of a settings window (disease, predator-prey or demographics).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
update.elements()
}
%- maybe also 'usage' for other objects documented here.
\details{
If "OK" has been pressed on the settings window, the variable is edited as usual.
If "Cancel" has been pressed, only cleanup operations are performed by \code{update.elements}.
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
