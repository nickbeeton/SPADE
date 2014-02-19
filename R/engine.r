# run.sim.opt: run simulation set up for use in an optimisation
# Run model simulation with given values for: 
# initial cull, maintenance cull, cell target density, allocated budget,
# area to cull, and "control area" (area for which proportional population is calculated)
# (last two are kept separate for the spatial optimisation, where we may choose to cull a subset
# of the target control area for efficiency)

run.sim.opt = function(strategy, init.cull, maint.cull, target.density, budget, cull.mask, ctrl.mask, fenced, progress.bar = TRUE)
{
  # make sure the parameters are up to date before starting (using update.params in init.r)
  update.params()

  # create a new variable "params" with parameters taken both from chosen strategy
  # and from arguments passed to function

  # TODO: change strategy.params so each list object contains all the information for 
  # an individual strategy. Currently, each object contains the values of one variable across ALL strategies
  # (would make code like this one line instead of dozens...)

  params = NULL
  params$cull.species = strategy.params$cull.species[strategy]
  params$cull.seasons = as.matrix(strategy.params$cull.seasons[strategy,]) # as.matrix for case where 1 strategy left
#  params$area.target = strategy.params$area.target[strategy]
# TODO: Why was this here??
  params$culling.choice = strategy.params$culling.choice[strategy]
  params$init.cull = init.cull
  params$maint.cull = maint.cull
  params$init.cull.abs = strategy.params$init.cull.abs[strategy]
  params$maint.cull.abs = strategy.params$maint.cull.abs[strategy]
  params$target.density = target.density
  params$cost.int = strategy.params$cost.int[strategy]
  params$cost.slope = strategy.params$cost.slope[strategy]
  params$helicopter.cost = strategy.params$helicopter.cost[strategy]
  # including only matrix version and not raster versions
  # as these are not necessary to pass
#  params$EL = list(strategy.params$EL[[strategy]])
  params$PR = list(strategy.params$PR[[strategy]])
  params$C.mask = list(cull.mask)
  params$TD = list(NULL)
  params$CI = list(strategy.params$CI[[strategy]])
  params$CS = list(strategy.params$CS[[strategy]])
  params$CH = list(strategy.params$CH[[strategy]])

  # save the *real* number of strategies
  true.N.strategies = N.strategies
  # set N.strategies to 1 for the purposes of the optimisation
  # (as we can only optimise for one strategy at a time)
  N.strategies <<- 1
    
  # run simulation
  res = run.sim(params, budget, fenced, ctrl.mask, progress.bar)

  # change the number back to the true number of strategies
  N.strategies <<- true.N.strategies
  
  # output result
  res
}

# Run a single simulation of the current system using the given parameters
# and information
run.sim = function(params, budget, fenced, ctrl.mask, progress.bar = TRUE, gui = TRUE)
{
  # make sure the parameters are up to date before starting (using update.params in init.r)
  if (gui) update.params()
  
  # create a progress bar for the user
  if (progress.bar) newProgressBar()

  # for each initial condition raster, make sure cells start above threshold
  # TODO: will this affect future model runs? (yes it does! comment out)
#  for (i in 1:N.species) IC[[i]][IC[[i]] < (interactions[[i]][[i]]$min.d * cell.size)] <<- (interactions[[i]][[i]]$min.d * cell.size) 

  # gauge size of map based on first species' carrying capacity
  Nx = ncol(K[[1]]); Ny = nrow(K[[1]]); 

  # find x and y coordinates of each defined cell in first species' carrying capacity
  locs <<- which(!is.na(K[[1]]), arr.ind = TRUE)
  # work out how many defined cells
  N = nrow(locs)

  # convert relevant variables to vector form if needed
  
  # if it's currently in matrix form, extract each cell value as defined by locs
  if (length(dim(ctrl.mask))>0) ctrl.mask = ctrl.mask[locs]

  # extract the mask of which cells to manage from the params object
  cull.mask = params$C.mask
  # do the same for the management mask for each individual strategy
  for (i in 1:length(cull.mask)) 
    if (length(dim(cull.mask[[i]]))>0) cull.mask[[i]] = cull.mask[[i]][locs]

  # do the same for fencing raster
  if (length(dim(fenced))>0) fenced = fenced[locs]
  
  # calculate how many cells are managed for each strategy
#  if (N.strategies > 0)
#  {
#    cull.cells = rep(NA, N.strategies)
#    for (i in 1:N.strategies) cull.cells[i] = sum(cull.mask[[i]] > 0, na.rm=T)
#  }
  
  # generate Laplace matrix 

#  lapmat <<- spam(0, nrow = N, ncol = N)

  row.sparse = integer(4*N)
  col.sparse = integer(4*N)
    
  if (!sparse) 
    lapmat <<- matrix(0, nrow = N, ncol = N)
  else
  {
    Nmax = 4 * N
    n = 0
  }
    

  # initialise length of fencing required
  fence.length = 0
  # initialise placement of fencing on grid
  fence.place = fenced * 0

  # calculate interactions between each pair of cells
  loc.nrow = max(locs[,1])
  loc.ncol = max(locs[,2])
  
  for (j in 1:loc.ncol) # search by column
  {
    point = which(locs[,2]==j) # which cells are in column j
    rows = locs[point,1] # find valid row numbers in column
    adjs = which(diff(rows)==1) # find which rows are adjacent
    n.adjs = length(adjs)
    if (n.adjs>0)
    {
      row.sparse[n + (1:n.adjs)] = point[adjs]
      col.sparse[n + (1:n.adjs)] = point[adjs+1]
      n = n + n.adjs
    }
    if ((Nmax - n) < (2*loc.nrow)) # if getting close, double array sizes
    {
      row.sparse = c(row.sparse, rep(0, Nmax))
      col.sparse = c(col.sparse, rep(0, Nmax))          
      Nmax = Nmax * 2
    }    
  }
  
  for (j in 1:loc.nrow) # search by row
  {
    point = which(locs[,1]==j) # which cells are in row j
    cols = locs[point,2] # find valid col numbers in row
    adjs = which(diff(cols)==1) # find which cols are adjacent
    n.adjs = length(adjs)
    if (n.adjs>0)
    {
      row.sparse[n + (1:n.adjs)] = point[adjs]
      col.sparse[n + (1:n.adjs)] = point[adjs+1]
      n = n + n.adjs
    }
    if ((Nmax - n) < (2*loc.ncol)) # if getting close, double array sizes
    {
      row.sparse = c(row.sparse, rep(0, Nmax))
      col.sparse = c(col.sparse, rep(0, Nmax))          
      Nmax = Nmax * 2
    }    
  }  
  
  # trim off unnecessary bits
  row.sparse = row.sparse[1:n]
  col.sparse = col.sparse[1:n]    
  # add reverse relationship
  row.sparse = c(row.sparse, col.sparse)
  col.sparse = row.sparse[c(n + (1:n), 1:n)]
  # order by row
  ii = order(row.sparse)
  row.sparse = row.sparse[ii]
  col.sparse = col.sparse[ii]
  cell.start = 1+c(0,which(diff(row.sparse)>0),length(row.sparse))

  if (sparse) val.sparse = numeric(2*n)  # initialise values for sparse format
  
  for (i in 1:(length(cell.start)-1)) # loop over all but the last interacting cell (last cell can't interact with itself)
  {
    j = cell.start[i]:(cell.start[i+1]-1)
    k = col.sparse[j]

    # calculate fence information
    # find out which adjacent cells are in a different zone (i.e. have a fence between them)
    fence.which = which(fenced[i] != fenced[k]) 
    fence.length = fence.length + length(fence.which) # add lengths to total

    # half the cost of fencing is allocated to each side
    # should still work properly even if length of fence.which = 0
    fence.place[i] = fence.place[i] + 0.5 * length(fence.which) # allocate to current cell's side
    fence.place[k[fence.which]] = fence.place[k[fence.which]] + 0.5  # allocate to neighbour's side

    # incorporate effects of a fence separating regions in different zones
    # with failure rate fence.failure
    disp.mult = fence.failure + (1 - fence.failure) * (fenced[i] == fenced[k]) 
    if (length(k) > 0) # if the cell has any neighbours
    {
      if (!sparse)
      {
        lapmat[i, k] <<- disp.mult # allow dispersal between them depending on fencing
        lapmat[k, i] <<- disp.mult # in both directions
      }
      else # do the above, but for a sparse matrix
      {
        val.sparse[j] = disp.mult
      }
    }
  }
 
  # calculate the number of cells close to each individual cell
  if (!sparse)
    nears = apply(lapmat, 1, sum)
  else
  {
    # efficient way to calculate nears using sparse matrix info
    nears = numeric(N)
    test = aggregate(val.sparse, by = list(r = row.sparse), FUN = sum)
    nears[test$r] = test$x
    # create sparse matrix
    lapmat <<- spam(list(i = row.sparse, j = col.sparse, val.sparse), N, N)
  }

  # scale the length and cost of the fence to make it per km as opposed to per cell
  # (note: assuming square cells)
  fence.length = fence.length * sqrt(cell.size)
  fence.place <<- fence.place * sqrt(cell.size)
  
  # two different methods to deal with dispersal
  # TODO: allow user to choose?
  
  # disperse in all available directions (same overall dispersal on boundary, just pushed more in the available directions)
  #  lapmat = lapmat / outer(rep(1,N), nears, '*') 
  #  lapmat = lapmat - diag(N)
  
  # if cannot disperse in a direction, stay put (and/or assume that just as many are migrating the other way = zero net migration)
  lapmat <<- lapmat / 8 
  if (!sparse)
    lapmat <<- lapmat - diag(nears)/8    
  else
  {
    diagnears = spam(list(i = 1:length(nears), j = 1:length(nears), nears))
    lapmat <<- lapmat - diagnears/8
  }

  # create matrix containing population information for each species for each cell
  pop = matrix(NA, N.species, nrow(locs))
  for (i in 1:N.species) # initialise population for each species
    pop[i,] = IC[[i]][locs] # pop is the current population map, starting with initial condition
  
  # any cells that go below a minimum threshold density are held at that threshold
  for (i in 1:N.species)  pop[i, pop[i,] < (interactions[[i]][[i]]$min.d * cell.size)] =  (interactions[[i]][[i]]$min.d * cell.size) 
  
  pop.store = vector('list', 1 + duration * seasons) # create a list to store the entire population time series
  pop.store[[1]] = pop # first map is initial condition

  cull.store = vector('list', duration * seasons) # create a list to store the entire culling time series
  
  total.pop = matrix(NA, N.species, 1 + duration * seasons) # matrix to store total population size for each species

  # work out initial total population (note: includes ALL animals, managed or not)
  total.pop[,1] = apply(pop, 1, function(x) sum(x, na.rm=T)) 

  # create variables for management stuff - we don't include the initial condition as we do for pop size
  total.cost = rep(NA, duration * seasons) # total cost across all strategies per timestep
  # remaining population for each managed species as a proportion of the original
  P.N0 = matrix(NA, N.strategies, duration * seasons) 
  total.cull = rep(NA, duration * seasons) # total number of animals removed
  
  # initialise total cost-benefit score              
  total.CB = pop[1,] * 0        

  for (t in 1:duration) # for each year
  {
    for (t2 in 1:seasons) # for each defined season within the year
    {
      
      if (timesteps == 1) # if number of integration timesteps is 1, assume a discrete model
      {
        # use "Euler method", i.e. work out the rates of change and just add them
        # to the current population for the season (TODO: do we need to scale this due to seasons?)
#        pop = pop + matrix(calc.step(0, as.vector(pop), list(interactions = interactions, season = t2, all.cull = all.cull))[[1]], N.species, ncol(pop))
        X = calc.step(0, c(as.vector(pop), rep(0, nrow(locs)*2 + 1)), list(interactions = interactions, year = t, season = t2, params = params, cull.mask = cull.mask))[[1]]
        pop = pop + matrix(X[1:(N.species * nrow(locs))], N.species, nrow(locs)) 
      }
      else # otherwise assume a continuous model
      {
        # use an ODE solver to work out the continuous solution of the PDE problem
        # uses technique known as "method of lines" - i.e. work out trajectory
        # for each individual cell (ODE), incorporating interactions from other 
        # cells as we go (PDE component of problem)
        X = rk4(y = c(as.vector(pop), rep(0, nrow(locs)*2 + 1)), times = seq(0, 1/seasons, l = timesteps + 1), func = calc.step, 
          parms = list(interactions = interactions, year = t, season = t2, params = params, cull.mask = cull.mask))
#        X = rk4(y = as.vector(pop), times = seq(0, 1/seasons, l = timesteps + 1), func = calc.step, 
#          parms = list(interactions = interactions, year = t, season = t2, params = params, cull.mask = cull.mask))
#        pop = matrix(X[timesteps+1, -1], N.species, ncol(pop)) # extract solution from solver and place into matrix
        X = X[timesteps + 1, -1]
#        pop = matrix(X[timesteps+1, 2:(N.species + 1)], N.species, ncol(pop)) # extract solution from solver and place into matrix
        pop = matrix(X[1:(N.species * nrow(locs))], N.species, nrow(locs))
      }
      
      # look for NA or NaN cells - means an error has happened
      if (any(is.na(pop) | is.nan(pop)))
      {
        if (progress.bar) dispose(pwindow) # close progress bar
        if (gui) insert(text.1, 'Simulation failed!') # tell the user the simulation failed
        # do the same on the R console - stop running simulation
        stop('NA/NaN value detected - simulation halted') 
      }

      # any cells that go below a minimum threshold density are held at thatde threshold      
      for (i in 1:N.species)  
        pop[i, pop[i,] < (interactions[[i]][[i]]$min.d * cell.size)] =  (interactions[[i]][[i]]$min.d * cell.size) 

      # store population from each cell in results
      pop.store[[(t-1) * seasons + t2 + 1]] = pop 

      # calculate and store current total population
      total.pop[, (t-1) * seasons + t2 + 1] = apply(pop, 1, function(x) sum(x, na.rm=T)) 

      # ***PLACEHOLDERS***
#       all.CC = pop[1,] * 0 
#       all.cull = pop * 0
       all.cull = X[(N.species * nrow(locs) + 1):( (N.species + 1) * nrow(locs))]
       cull.store[[(t-1) * seasons + t2]] = all.cull
       
      # calculate current total cost and number of animals managed
#      total.cost[(t-1) * seasons + t2] = sum(all.CC, na.rm=T) 
      total.cost[(t-1) * seasons + t2] = X[(N.species + 2) * nrow(locs) + 1]
      total.cull[(t-1) * seasons + t2] = sum(all.cull, na.rm=T)
      total.CB = total.CB + X[((N.species + 1) * nrow(locs) + 1):( (N.species + 2) * nrow(locs))]

      # work out P.N0 (proportion of initial population) in control region only
      if (N.strategies > 0) # if any strategies defined
      {
        for (i in 1:N.strategies) # for each strategy
        {
          # work out population in control region divided by initial population in control region
          # for the species defined in current strategy
          P.N0[i, (t-1) * seasons + t2] = sum(pop[params$cull.species[i], ctrl.mask == 1], na.rm=T) / 
            sum(IC[[params$cull.species]][locs][ctrl.mask == 1], na.rm=T)
        }
      }
      else # if no strategies defined
        P.N0[,(t-1) * seasons + t2] = 0

      if (progress.bar) # if the progress bar is open
      {
        p = ((t - 1 + t2/seasons)/duration) # calculate how far through we are
        updateProgressBar(p) # draw info on progress bar
      }
    }
  }
  
  benefit = 0
  if (N.strategies > 0) # if any strategies defined
    for (i in 1:N.strategies)
      benefit = benefit + sum(pop[params$cull.species[i],] * params$PR[[i]][locs])

  # scale by 'benefit' = 1 / sum(N * PR)
  total.CB = total.CB * benefit

  if (progress.bar) dispose(pwindow) # close progress bar
  # return list containing relevant information from simulation
  list(total.cost = total.cost, total.pop = total.pop, pop.ts = pop.store, cull.ts = cull.store, P.N0 = P.N0, cost.benefit = total.CB, total.cull = total.cull)
}             

# run through a simulation step
calc.step = function(t, y, parms)
{
  # reshape input data into a matrix with a row for each species
  # not including the cost-benefit, culling or cost information
  # which is not needed as input (only output)
  pop = matrix(y[1:(N.species * nrow(locs))], N.species, nrow(locs))
  pop[pop < 0] = 0
#  dydt = pop*0 
  dydt = matrix(0, N.species, nrow(locs))   # initialise dy/dt information
  season = parms$season # extract current season
                                  
  # loop over all interactions between species (and within-species)
  for (i in 1:N.species)
  {
    for (j in 1:N.species)
    {
      # extract functional response for current interaction
      funcresp = parms$interactions[[i]][[j]]$funcresp[season] 
      if (i == j) # intra-species demographics
      {
        # extract boxes information about 'total population'
        counted = which(interactions[[i]][[j]]$boxes[,1]) 
        N = pop[1,]*0 # initialise 'total population' information
        KN = N*0 # and another one for its carrying capacity
        
        if (length(counted) > 0) # if there are checked boxes
        {
          for (k in counted) 
          {
            KN = KN + K[[k]][locs] # add carrying capacity info
            N = N + pop[k,] # add population info
          }
        }

        ii = KN < 0.5 * N
        if (any(is.na(ii))) browser()
        KN[ii] = 0.5 * N[ii] # to avoid instability issues
        KN = KN + 1e-6 # to avoid div by zero        
        
        # run line(s) of R code loaded from formulae.csv via demo.forms
        # (demo.forms is loaded from the csv file in init.r)
        # code should output change in species i population to a variable called 'tmp'
        if (timesteps == 1) # if we have a discrete model
          eval(parse(text = gsub('\\\\n', '\n', as.character(demo.forms$Discrete[funcresp]))))
        else # if continuous
          eval(parse(text = gsub('\\\\n', '\n', as.character(demo.forms$Continuous[funcresp]))))
          
        dydt[i,] = dydt[i,] + tmp # add result of code to dydt matrix
                                          
        # dispersal rate for species
        # = 'global' dispersal raster * species-specific dispersal raster
        EL = EL.all * EL.species[[i]]
        # extract functional response for diffusion
        diffresp = parms$interactions[[i]][[j]]$diffresp

        # run line(s) of R code loaded from formulae.csv via diffusion.forms
        # (demo.forms is loaded from the csv file in init.r)
        # code should output change in species i population to a variable called 'migration'
        if (timesteps == 1) # if we have a discrete model
          eval(parse(text = gsub('\\\\n', '\n', as.character(diffusion.forms$Discrete[diffresp]))))
        else  # if continuous
          eval(parse(text = gsub('\\\\n', '\n', as.character(diffusion.forms$Continuous[diffresp]))))

        dydt[i,] = dydt[i,] + migration # add result of code to dydt matrix
      }
      else # inter-species interactions
      {
        # for reference, list of potential options for status:
        # droplist.options <<- c('','predates on', 'is prey to', 'becomes', 'converts', 'infects', 'is infected by', 'cooperates with')
        
        if (status[i,j] == 2) # "predates on"
        {
          # run line(s) of R code loaded from formulae.csv via predprey.forms
          # (demo.forms is loaded from the csv file in init.r)
          # code should output change in species i and j population to a variable called 'tmp'
          if (timesteps == 1) # if we have a discrete model
            eval(parse(text = gsub('\\\\n', '\n', as.character(predprey.forms$Discrete[funcresp]))))
          else  # if continuous
            eval(parse(text = gsub('\\\\n', '\n', as.character(predprey.forms$Continuous[funcresp]))))

          # add result of code to dydt matrix for each species
          dydt[i,] = dydt[i,] + tmp # TODO: update GUI with assimilation efficiency or something
          dydt[j,] = dydt[j,] - tmp # (i.e. won't get the same increase in one species as decrease in the other)
        }
        else if (status[i,j] == 4) # "becomes"
        {
          # extract interaction information
          from = parms$interactions[[i]][[j]]$from # species being converted
          to = parms$interactions[[i]][[j]]$to # species being converted to
          via = parms$interactions[[i]][[j]]$via # causative agent - catalyst species 

          # extract boxes information about 'total population'
          counted = which(interactions[[i]][[j]]$boxes[,1])
          N = pop[1,]*0 # initialise 'total population' information
          if (length(counted) > 0) # if there are checked boxes
            for (k in counted) N = N + pop[k,] # add population info
          N = N + 1e-6 # to avoid div by zero
          
          # if there is no via variable or if it is set to zero,
          # set the causative species to the target ('to') species
          if (length(via) == 0)
            via = to
          else if (via == 0) 
            via = to

          # run line(s) of R code loaded from formulae.csv via disease.forms
          # (demo.forms is loaded from the csv file in init.r)
          # code should output change in species i population to a variable called 'tmp'
          if (timesteps == 1) # if we have a discrete model
            eval(parse(text = gsub('\\\\n', '\n', as.character(disease.forms$Discrete[funcresp]))))
          else  # if continuous
            eval(parse(text = gsub('\\\\n', '\n', as.character(disease.forms$Continuous[funcresp]))))

          # add result of code to dydt matrix for each species            
          dydt[from,] = dydt[from,] - tmp
          dydt[to,] = dydt[to,] + tmp         
        }
      }
    }
  }

  all.cull = calc.cull(pop, parms$year, parms$season, parms$params, parms$cull.mask) # call calc.cull function to work out removal based on current population and strategy parameters
  dydt = dydt - all.cull$cull # remove animals as specified by management strategies

  # any cells that go below a minimum threshold density are held at that threshold  
  for (i in 1:N.species)  dydt[i, dydt[i,] < 0 & pop[i,] < (interactions[[i]][[i]]$min.d * cell.size)] = 0 

  # quicker version of apply(all.cull$cull, 2, sum)
  tmp = numeric(ncol(all.cull$cull)); for (i in 1:nrow(all.cull$cull)) tmp = tmp + all.cull$cull[i,]

  # output dydt information, as well as total removal for each cell,
  # cost benefit for each cell and total cost across all cells
  list(c(as.vector(dydt), tmp, all.cull$CB, all.cull$CC))
}
      
# non-spatial budget (based on but using a different method to STAR version - didn't bother to replicate theirs entirely)
# note - didn't bother to put in progress bar
NS.B = function(budget, C.mask, s)
{
  # initialise results of runs (cost output)
  top = rep(NA, 5)
  bottom = rep(NA, 5)

  # work out the cost if we ramp up management to 99% for each target density level
  for (i in 1:5) {res=run.sim.opt(s, 0.99, 0.99, td[i], budget, C.mask, C.mask, fenced, progress.bar = FALSE); top[i] = sum(res$total.cost)}
  # alternatively, work out what happens if we do the bare minimum (1%)
  for (i in 1:5) {res=run.sim.opt(s, 0.01, 0.01, td[i], budget, C.mask, C.mask, fenced, progress.bar = FALSE); bottom[i] = sum(res$total.cost)}
  
  easy = which(top < budget)   # work out which target density levels still satisfies the budget even at the highest cull rate
  hard = which(bottom < budget) # work out which target density levels are achievable within budget doing the bare minimum
  
  if (length(easy) > 0) # if there's at least one case where we can get a 99% result within budget, we're done
  {
    m = max(easy) # pick the hardest case where we can do this (smallest target density)
    # (note: STAR picks the easiest case instead, even though we could still go under budget with 
    # a more ambitious target density)
    cull = 0.99 # set result to 99%
  }
  else if (length(hard) == 0) # if we can't go under budget no matter what we do
  {
    m = 5 # set the easiest case
    cull = NA # set result to NA (will be handled by run.NSB)
  }
  else # if we can go under budget, but can't do it at the maximum 99% rate
  {
    # look for easiest case (largest cell target density) where we can manage within budget - 
    # this is to maximise the rate we can get (this should always be m=1, or 0.75 density)
    m = min(which(top > budget & bottom < budget)) 
    # make a function to tell us how far we are over budget for a particular rate x
    sim = function(x) sum(run.sim.opt(s, x, x, td[m], budget, C.mask, C.mask, fenced, progress.bar = FALSE)$total.cost) - budget 
    # find where we are pretty much exactly on budget, somewhere between the min and max rates
    U = uniroot(sim, c(0.01, 0.99)) 
    # round down the rate so that we are strictly within budget 
    # (note - this won't work if the required budget is INCREASING with decreasing cull rate, shouldn't be an issue though)
    cull = floor(U$root * 100)/100 
  }
  
  # return optimal target density and management rate
  list(target.density = td[m], cull.rate = cull) 
}


# non-spatial control density 
# very similar to non-spatial budget, but based on achieving a target proportional density instead of cost
# we are here looking for 1) *minimum* cull rate and then 2) *maximum* target density

# TODO: put in progress bar
NS.D = function(pn0, C.mask, s)
{
  # initialise results of runs (proportional pop of last season in run)
  top = rep(NA, 5)
  bottom = rep(NA, 5)

  # work out the effect on population if we ramp up management to 99% for each target density level  
  for (i in 1:5) {res=run.sim.opt(s, 0.99, 0.99, td[i], budget, C.mask, C.mask, fenced, progress.bar = FALSE); top[i] = res$P.N0[length(res$P.N0)]}
  # alternatively, work out what happens if we do the bare minimum (1%)  
  for (i in 1:5) {res=run.sim.opt(s, 0.01, 0.01, td[i], budget, C.mask, C.mask, fenced, progress.bar = FALSE); bottom[i] = res$P.N0[length(res$P.N0)]}
  
  hard = which(top < pn0)  # work out for which target density levels the required density reduction is achieved at 99% reduction
  easy = which(bottom < pn0) # work out for which target density levels the required density reduction is achieved at 1% reduction
  
  if (length(easy) > 0)
  {
    m = min(easy) # easiest case (looking for the least effort required to make the target)
    cull = 0.99
  }
  else if (length(hard) == 0)
  {
    m = 1 # set the hardest case
    cull = NA # set result to NA (will be handled by run.NSD)
  }
  else # look for easiest case under required density reduction (lowest target density -> lowest management rate required) where we're under target
  {
    m = max(which(top < pn0 & bottom > pn0)) # should pretty much always be m=5 where it exists
    # make a function to tell us how far we are over the required prop. density for a particular rate x
    sim = function(x) sum(run.sim.opt(s, x, x, td[m], budget, C.mask, C.mask, fenced, progress.bar = FALSE)$P.N0[length(res$P.N0) - 1]) - pn0
    # find where we are pretty much exactly on target, somewhere between the min and max rates
    U = uniroot(sim, c(0.01, 0.99))
    # round up the rate so that we are strictly below the population threshold
    cull = ceiling(U$root * 100)/100
  }
  
  # return optimal target density and management rate
  list(target.density = td[m], cull.rate = cull)
}



#spatial budget
S.B = function(budget, ctrl.mask, s)
{
  newProgressBar() # draw progress bar
  
  #start with 50% management rate and target density
  best = run.sim.opt(s, 0.5, 0.5, 0.5, budget, ctrl.mask, ctrl.mask, fenced, progress.bar = FALSE) 
  # work out cost-benefit percentiles across cells
  CB = best$cost.benefit  
#  CB[is.na(CB) | is.nan(CB)] = 0 # set areas where no management performed to 0
  CB[is.na(CB) | is.nan(CB) | CB == 0] = max(CB, na.rm=T) #  (make sure managed areas take priority)
  Q = quantile(CB, probs = probs, na.rm=T) # calculate quantiles

  # iterate process five times 
  # (note - no measure of convergence used in STAR, may not converge properly or waste time when already converged)
  # TODO: add convergence checking?
  for (t in 1:5) 
  {
    # initialise "best" variables
    best.run = 0 # which run of the six is best
    best.dens = Inf # set best relative density to a value that will be beatable by anything useful
    best.cullrate = 0 # management rate for best option
    best.td = 0 # target density for best option
    
    # run a model for each of the six given quantiles calculated above
    # so working from managing very little to all of the area
    for (i in 1:6) 
    {
      W = (CB <= Q[i]) + 0 # set W to be where we want to manage
      test = NS.B(budget, W, s) # run non-spatial budget optimiser on the map
      # run a simulation of the optimal result of the optimisation
      out = run.sim.opt(s, test$cull.rate, test$cull.rate, test$target.density, budget, W, ctrl.mask, fenced, progress.bar = FALSE) 
#      # store the current density reduction to compare to the best
#      dred = out$P.N0[duration * seasons] 

      # store the current priority x density measure to compare to the best
      dred = sum(out$cost.benefit) / sum(out$total.cost)

      if (dred < best.dens) # if it's the best, store its information
      {
        best.run = i # store new best option
        best.dens = dred # store density reduction
        best.cullrate = test$cull.rate # store management rate
        best.td = test$target.density # store selected target density
      }

      p = (6*(t-1) + i)/30 # calculate how far through we are
      updateProgressBar(p) # draw information on progress bar
    }
    
    if (t < 5) # if we're not finished yet, run the best of the six quantiles managing 
    # over the whole map (as defined by ctrl.mask)
    # to get a new cost-benefit map under those conditions
    {
      # run best case scenario again, but with whole map instead of quantiles
      best = run.sim.opt(s, best.cullrate, best.cullrate, best.td, budget, ctrl.mask, ctrl.mask, fenced, progress.bar = FALSE)
      CB = best$cost.benefit  # store cost-benefit map for next run
      CB[is.na(CB) | is.nan(CB)] = 0 # set areas where no management performed to 0     
      Q = quantile(CB, probs = probs , na.rm=T) # calculate quantiles     
    }
    else # otherwise work out the *final* management map and simulation results based on the best run
    {
      W = (CB <= Q[best.run]) + 0 # calculate map from best run
      # run simulation for the last time to get information for output
      final = run.sim.opt(s, best.cullrate, best.cullrate, best.td, budget, W, ctrl.mask, fenced, progress.bar = FALSE)
    }
  }
  
  dispose(pwindow) # close progress bar
  # return best optimisation results, best simulation, and culling map
  list(test = list(target.density = best.td, cull.rate = best.cullrate), out = final, culling.area = W) 
}