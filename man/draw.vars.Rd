\name{draw.vars}
\alias{draw.vars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw information for variables in a parameter selection window
%%  ~~function to do ... ~~
}
\description{
Draws groups, labels and edit boxes for variables in a parameter selection window 
based on the available variables for the functional forms.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw.vars(variables.list, curr.variables.list, curr.defaults.list, container = "settings.group")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variables.list}{
  A vector of strings containing the names of all possible unique variables for the functional forms
%%     ~~Describe \code{variables.list} here~~
}
  \item{curr.variables.list}{
  A vector of strings containing the names of the variables describing the currently selected functional form
%%     ~~Describe \code{curr.variables.list} here~~
}
  \item{curr.defaults.list}{
  The defaults for the variables in \code{curr.variables.list}
%%     ~~Describe \code{curr.defaults.list} here~~
}
  \item{container}{
  The name of the container in which the UI elements will be placed
%%     ~~Describe \code{container} here~~
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
#    # ---
#    # Example from button.handler in interactions.r
#    # ---
#
#    # extract all possible unique variables for all demographic formulas
#    variables.list = unique(unlist(strsplit(as.character(demo.forms$Variables), ' ')))
#    # select variables from current functional form
#    curr.variables.list = strsplit(as.character(demo.forms$Variables[funcresp[curr.time]]), ' ')[[1]] 
#    # select default variable values from current functional form
#    curr.defaults.list = strsplit(as.character(demo.forms$Defaults[funcresp[curr.time]]), ' ')[[1]]
#    
#    # draw groups, labels and edit boxes for variables based on 
#    # the available variables for demographic functions
#    draw.vars(variables.list, curr.variables.list, curr.defaults.list, 'settings.group1')                                        
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
