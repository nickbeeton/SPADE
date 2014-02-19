\name{S.B}
\alias{S.B}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spatial budget algorithm
%%  ~~function to do ... ~~
}
\description{
Given a required budget, an allocated management area, and a specified management
strategy, outputs - where possible - an optimal target density, management rate,
and specified cells within the allocated area to manage, all within budget.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
S.B(budget, ctrl.mask, s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{budget}{
The maximum budget  
%%     ~~Describe \code{budget} here~~
}
  \item{ctrl.mask}{
A boolean rasters containing locations where management may be performed for the strategy  
%%     ~~Describe \code{ctrl.mask} here~~
}
  \item{s}{
Which strategy within the global \code{strategy.params} to use  
%%     ~~Describe \code{s} here~~
}
}
\details{
The algorithm used here is based on that performed in STAR (McMahon et al., 2010). 
It is similar to \code{NS.B}, except that it also incorporates the possibility of 
managing a subset of the given management area rather than the entire area. The 
algorithm optimises by minimising the cost-benefit score across all cells, effectively
attempting to obtain the highest level of removal possible per dollar spent.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list with elements:
\item{test}{A list containing two elements (as in \code{NS.B}):
  \itemize{
    \item{\code{target.density} The optimal target density}
    \item{\code{cull.rate} The optimal management rate}
  }
}
\item{out}{A list containing the output of the optimal run of \code{\link{run.sim.opt}} - see that page
for a description of this element's format}
\item{culling.area}{A boolean raster containing the optimal management area}

%   list(test = list(target.density = best.td, cull.rate = best.cullrate), out = final, culling.area = W) 
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
This algorithm extends \code{\link{NS.B}}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#    # ---
#    # Example from run.SB
#    # ---
#    test2 <<- S.B(budget, strategy.params$C.mask[[chosen.strategy]], chosen.strategy) # outputs results of optimisation to test2
#    if (is.na(test2$test$cull.rate)) # if the budget is too small to give a result
#    {
#      confirmDialog('Budget too small!')
#      rm(test2, envir = globalenv()) # remove test2
#    }
#    else # extract necessary variables from test2
#    {
#      optim.cull <<- test2$culling.area
#      out <<- test2$out
#      test <<- test2$test
#      confirmDialog('Optimisation done!')
#    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
