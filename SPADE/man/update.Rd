\name{update}
\alias{update}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate a season
%%  ~~function to do ... ~~
}
\description{
Solves the PDE system comprising the SPADE model over the course of one season.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
update(t, y, parms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{
Current time in the model run
%%     ~~Describe \code{t} here~~
}
  \item{y}{
A vector containing all the current population information for each species
and cell - can be obtained by applying \code{as.vector} to a matrix of dimensions
\code{N.species} by \code{nrow(locs)}, where \code{nrow(locs)} is the number of 
traversable cells in the model.
%%     ~~Describe \code{y} here~~
}
  \item{parms}{
A list containing elements:
\itemize{
\item{\code{interactions} The interactions matrix containing all necessary information
to compute the change in each population over a season}
\item{\code{season} Which season the solver is being run during}
\item{\code{all.cull} A matrix of dimensions \code{N.species} by \code{nrow(locs)}
defining how many animals of each species are expected to be removed}
}
}
%%     ~~Describe \code{parms} here~~
}

\details{
The parameters of \code{update} are set in such a way that it can be used in 
standard ODE solvers for continuous models (e.g. SPADE uses rk4 from the deSolve package).
It can also be used in discrete models (see Example below).
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of the same length as \code{y}, determining the annual rate of change of 
corresponding cells.
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
#      # ---
#      # Example taken from run.sim
#      # ---
#      if (timesteps == 1) # if number of integration timesteps is 1, assume a discrete model
#      {
#        # use "Euler method", i.e. work out the rates of change and just add them
#        # to the current population for the season (TODO: do we need to scale this due to seasons?)
#        pop = pop + matrix(update(0, as.vector(pop), list(interactions = interactions, season = t2, all.cull = all.cull))[[1]], N.species, ncol(pop))
#      }
#      else # otherwise assume a continuous model
#      {
#        # use an ODE solver to work out the continuous solution of the PDE problem
#        # uses technique known as "method of lines" - i.e. work out trajectory
#        # for each individual cell (ODE), incorporating interactions from other 
#        # cells as we go (PDE component of problem)
#        X <<- rk4(y = as.vector(pop), times = seq(0, 1/seasons, l = timesteps + 1), func = update, 
#          parms = list(interactions = interactions, season = t2, all.cull = all.cull))
#        pop = matrix(X[timesteps+1, -1], N.species, ncol(pop)) # extract solution from solver and place into matrix
#      }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
