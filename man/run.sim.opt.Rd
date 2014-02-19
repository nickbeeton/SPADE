\name{run.sim.opt}
\alias{run.sim.opt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulation for optimisation
%%  ~~function to do ... ~~
}
\description{Runs a SPADE simulation with some pre-determined settings. Designed
to run effectively as part of an optimisation where multiple runs
need to be performed involving a single management strategy.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run.sim.opt(strategy, init.cull, maint.cull, target.density, budget, cull.mask, ctrl.mask, fenced, progress.bar = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strategy}{
An integer describing which numbered strategy is being optimised
(strategy information is found in the global variable \code{strategy.params})
%%     ~~Describe \code{strategy} here~~
}
  \item{init.cull}{
Which proportion of the population (specified in the 
strategy) the simulation will try to remove in the first season
%%     ~~Describe \code{init.cull} here~~
}
  \item{maint.cull}{
Which proportion of the population (specified in the 
strategy) the simulation will try to remove in subsequent seasons
%%     ~~Describe \code{maint.cull} here~~
}
  \item{target.density}{
The density (in animals per square kilometre) below which management will not 
occur in a cell
%%     ~~Describe \code{target.density} here~~
}
  \item{budget}{
A budget (in dollars) for the purposes of calculating cost-benefit scores
%%     ~~Describe \code{budget} here~~
}
  \item{cull.mask}{
A boolean raster mask describing which cells will be managed during the 
simulation
%%     ~~Describe \code{cull.mask} here~~
}
  \item{ctrl.mask}{
A boolean raster mask describing the "control area" - i.e. the area which is
assessed in terms of population reduction for optimisation purposes
%%     ~~Describe \code{ctrl.mask} here~~
}
  \item{fenced}{
A numerical raster mask describing fencing areas - cells with different values
are assumed to be fenced off from one another
%%     ~~Describe \code{fenced} here~~
}
  \item{progress.bar}{
A boolean value describing whether to draw a progress bar
%%     ~~Describe \code{progress.bar} here~~
}
}
\details{
This function is used in all of SPADE's optimisation functions (i.e. 
\code{NS.B}, \code{NS.D} and \code{S.B}) as a relatively sophisticated
wrapper function for \code{run.sim} - its main advantage is in that it will
pick out a single strategy and apply the simulation where only that strategy
is applied, unlike a normal run of \code{run.sim}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list with elements:
\item{total.cost}{A vector of the total cost from all management
strategies during each season, of length \code{duration * seasons}}
\item{total.pop}{A matrix with dimensions \code{N.species} by 
\code{1 + duration * seasons},
containing the total population for each species at the start of
the simulation and at the end of each season}
\item{pop.ts}{A list of matrices with dimensions \code{N.species}
 by \code{nrow(locs)}, where \code{nrow(locs)} is the number of
well-defined cells in the map. 
The matrix for each timestep contains the number of
 animals of each species in each cell}
\item{P.N0}{A matrix with dimension \code{N.strategies} by 
\code{duration * seasons}, containing the total population of 
each managed species as a proportion of its initial population, 
for each defined strategy}
\item{cost.benefit}{A vector of cost benefit score for each cell, of 
length \code{nrow(locs)}}
\item{total.cull}{A vector of the total removal performed in all 
management strategies during each season, of length 
\code{duration * seasons}}
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
\code{\link{run.sim}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#  # --- 
#  # Example from NS.B, running simulation under five different
#  # target densities at 99% removal to determine the cost of each
#  # ---
#  # initialise results of runs (cost output)
#  top = rep(NA, 5)
#  # work out the cost if we ramp up management to 99% for each target density level
#  for (i in 1:5) 
#  {
#    res=run.sim.opt(s, 0.99, 0.99, td[i], budget, C.mask, C.mask, fenced, progress.bar = FALSE); 
#    top[i] = sum(res$total.cost)
#  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
