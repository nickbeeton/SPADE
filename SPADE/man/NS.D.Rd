\name{NS.D}
\alias{NS.D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Non-spatial density algorithm
%%  ~~function to do ... ~~
}
\description{
Given a required maximum final relative population size, the desired management area, and a specified management
strategy, outputs - where possible - an optimal target density and management rate
within budget.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NS.D(pn0, C.mask, s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pn0}{
The required maximum total population of the managed species after the management time period, 
as a proportion of its initial population size
%%     ~~Describe \code{pn0} here~~
}
  \item{C.mask}{
A boolean raster containing the locations where management is performed for the strategy
%%     ~~Describe \code{C.mask} here~~
}
  \item{s}{
Which strategy within the global \code{strategy.params} to use
%%     ~~Describe \code{s} here~~
}
}
\details{
The algorithm used here is based on that performed in STAR (McMahon et al., 2010)
but uses a different search technique. It first checks whether it is possible to
remove, respectively, 1\% and 99\% each season within budget 
for a number of pre-set target densities (\code{td}), and 
if required then searches through intermediate cases. Its priorities are, in order:
\enumerate{
\item Lowest possible removal rate
\item Highest possible target density
} 
This algorithm is called by the handler for the "Run Non-Spatial Density" button, \code{run.NSD}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list containing two elements:
\item{target.density}{The optimal target density}
\item{cull.rate}{The optimal management rate}
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
\code{\link{NS.B}} is a very similar algorithm, but constrained by budget rather than population.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#    # ---
#    # Example from run.NSD
#    # ---
#    test <<- NS.D(area.target, strategy.params$C.mask[[chosen.strategy]], chosen.strategy) # outputs results of optimisation to "test"
#    if (is.na(test$cull.rate)) # if the required density is too small to give a result
#    {
#      confirmDialog('Can\'t hit target density even at 99\% cull rate!')
#      rm(test, envir = globalenv())
#    }
#    else # run model based on found optimal parameters
#    {
#      out <<- run.sim.opt(chosen.strategy, test$cull.rate, test$cull.rate, test$target.density, budget, strategy.params$C.mask[[chosen.strategy]], strategy.params$C.mask[[chosen.strategy]], fenced) 
#      confirmDialog('Optimisation done!')
#    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
