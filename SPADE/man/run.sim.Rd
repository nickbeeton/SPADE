\name{run.sim}
\alias{run.sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SPADE simulation
%%  ~~function to do ... ~~
}
\description{
Runs a single SPADE simulation of the current system using the given parameters
and information. Makes use of pre-defined global variables as well as input arguments.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run.sim(params, budget, fenced, ctrl.mask, progress.bar = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
A list containing the required management parameters for the model - 
each object within the list contains as many elements (or rows) as there are 
management strategies.

The required objects are:
\itemize{
\item{\code{cull.species} Integers describing which species is managed by each strategy}
\item{\code{cull.seasons} A matrix of booleans of \code{dim}ension N.strategies by N.seasons,
describing whether management occurs in each season for each strategy}
\item{\code{culling.choice} Integers specifying whether proportional management (1) or
absolute management (2) is performed}
\item{\code{init.cull} Which proportion of the population (specified in the strategy) 
the simulation will try to remove in the first season}
\item{\code{maint.cull} Which proportion of the population (specified in the strategy) 
the simulation will try to remove in subsequent seasons}
\item{\code{init.cull.abs} Which total number of the population (specified in the strategy) 
the simulation will try to remove in the first season}
\item{\code{maint.cull.abs} Which total number of the population (specified in the strategy) 
the simulation will try to remove in subsequent seasons}
\item{\code{target.density} The density (in animals per square kilometre) below 
which management will not occur in a cell}
\item{\code{cost.int} The intercept of the power relationship between population density and time}
\item{\code{cost.slope} The intercept of the power relationship between population density and time}
\item{\code{helicopter.cost} Cost per hour of management}
\item{\code{EL} A list of rasters containing dispersal information for each strategy}
% TODO: Fix this - doesn't actually do anything
\item{\code{PR} A list of rasters containing priority information for each strategy - 
priority information is used in calculating cost-benefit information}
\item{\code{C.mask} A list of boolean rasters containing the locations where management is performed 
for each strategy}
\item{\code{TD} As for \code{target.density}, but a list of rasters with separate values for each cell,
and a separate raster for each strategy}
\item{\code{CI} As for \code{cost.int}, but a list of rasters with separate values for each cell,
and a separate raster for each strategy}
\item{\code{CS} As for \code{cost.slope}, but a list of rasters with separate values for each cell,
and a separate raster for each strategy}
\item{\code{CH} As for \code{helicopter.cost}, but a list of rasters with separate values for each cell,
and a separate raster for each strategy}
}
%%     ~~Describe \code{params} here~~
}

  \item{budget}{
A budget (in dollars) for the purposes of calculating cost-benefit scores
%%     ~~Describe \code{budget} here~~
}
  \item{fenced}{
A numerical raster mask describing fencing areas - cells with different values are assumed to be fenced off from one another
%%     ~~Describe \code{fenced} here~~
}
  \item{ctrl.mask}{
A boolean raster mask describing the "control area" - i.e. the area which is assessed in terms of population reduction for optimisation purposes
%%     ~~Describe \code{ctrl.mask} here~~
}
  \item{progress.bar}{
A boolean value describing whether to draw a progress bar
%%     ~~Describe \code{progress.bar} here~~
}
}
\details{
This function is essentially the core of SPADE - it, along with its called function \code{engine}, 
comprises the simulation of demographic, interaction and dispersal processes given 
a wide array of inputs.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#  # ---
#  # Example taken from run.model - the handler of the "Run Model" button in the SPADE GUI
#  # ---
#  update.strategy.sliders() # make sure the current values in the edit bars of Management are stored into memory for the simulation
#  out <<- run.sim(strategy.params, budget, fenced, strategy.params$C.mask[[1]]) # runs simulation, outputs model output to "out"
#  
#  draw.pop.plot() # draw population-time plot
#  draw.cost.plot() # draw cost-time plot
#  draw.pn0.plot() # draw proportional population plot
#  curr.t <<- 1 # set current time to 1 for the spatial time series plots
#  draw.pop.ts(curr.t) # draw abundance map at current timestep
#  draw.cb() # draw cost-benefit map
#  update.graphics() # update the text report box
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
