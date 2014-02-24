\name{plotdata}
\alias{plotdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot SPADE raster data
%%  ~~function to do ... ~~
}
\description{
Draw raster data to the currently selected plot device - using a Google Maps 
background if projection data exists.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotdata(DATA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATA}{
A matrix containing the raster data. Note that only data will be taken from this 
variable - any projection information will be taken from the carrying capacity 
information of the first species, \code{K.rast[[1]]}.
%%     ~~Describe \code{DATA} here~~
}
  \item{\dots}{
Any extra arguments are passed to \code{plot} from the \code{raster} package.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
If projection data is available in \code{K.rast[[1]]}, then the raster will be
"un-projected" to GDA94 lat-longs to a resolution of 0.01 degrees, and 
\code{plotdata} will attempt to download the matching Google Maps data 
using the \code{RgoogleMaps} package. Note that no error will be returned if 
the download fails for any reason, and the plot will continue without the 
Google Maps background. If successful, the Google Maps tile will be used as the 
background for the drawn raster.

%%  ~~ If necessary, more details than the description above ~~
}
\value{
No value returned.
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
