# Draw raster data to currently selected plot device - using a Google Maps background if projection data exists
plotdata = function(DATA, ...)
{
  if (length(DATA) > 0) # if there is data in the supplied raster
  {
    # create a raster based on the first species' carrying capacity (including projection info)
    DATA.raster = K.rast[[1]] 
    DATA.raster = setValues(DATA.raster, getValues(raster(DATA))) # set its values to the input data
    if (is.na(projection(K.rast[[1]]))) # if there is no projection, just plot it
    {
      plot(DATA.raster, smallplot = c(.75,.8,0.2,0.8), ...)    
    }
    else if (projection(K.rast[[1]]) == 'NA')
    {
      plot(DATA.raster, smallplot = c(.75,.8,0.2,0.8), ...)    
    }    
    else
    {
     # "un-project" to GDA94 lat-longs (TODO: may need to tweak res, currently ~1km)
     mapRaster = projectRaster(DATA.raster, crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs", res = 0.01, method = 'ngb') 
     # extract coordinate limits of newly unprojected raster
     xmin = attributes(extent(mapRaster))$xmin
     xmax = attributes(extent(mapRaster))$xmax
     ymin = attributes(extent(mapRaster))$ymin
     ymax = attributes(extent(mapRaster))$ymax                               
     env = ls('.GlobalEnv')
     if (!("googlemap" %in% env)) # if the variable 'googlemap' doesn't yet exist in the global env, create it
        googlemap <<- try(GetMap.bbox(c(xmin, xmax), c(ymin, ymax)), silent=TRUE) # load map data from Google maps
     # plot the unprojected data (will be drawn over, is used only to initialise the plot)
     plot(mapRaster, alpha = 0.5, col = rev(terrain.colors(255)), smallplot = c(.75,.8,0.2,0.8), asp=1, ...)
     # if there was no error in loading the Google Maps data, draw it

    if (!inherits(googlemap, 'try-error'))   
    {
      givenlatcentre = 0.5*(googlemap$BBOX$ll[1] + googlemap$BBOX$ur[1])
      givenloncentre = 0.5*(googlemap$BBOX$ll[2] + googlemap$BBOX$ur[2])
      difflat = googlemap$lat.center - givenlatcentre
      difflon = googlemap$lon.center - givenloncentre

      rasterImage(googlemap[[4]], googlemap$BBOX$ll[1,2]+difflon, googlemap$BBOX$ll[1,1]+difflat,  googlemap$BBOX$ur[1,2]+difflon, googlemap$BBOX$ur[1,1]+difflat)
    }
#      rasterImage(googlemap[[4]], bb$lonR[1], bb$latR[1], bb$lonR[2], bb$latR[2])
#    rasterImage(googlemap[[4]], xmin, ymin, xmax, ymax)
    # now plot the unprojected data on top of our Google Maps data (or on its own if that doesn't exist),
    # with 50% transparency so both map and data are clearly visible
     plot(mapRaster, add = TRUE, alpha = 0.5, col = rev(terrain.colors(255)), 
       smallplot = c(.75,.8,0.2,0.8), ...)
    }
  }
}


# Update text report box
update.graphics = function()
{
  env = ls('.GlobalEnv') # loads all variable names in R workspace

  if ("out" %in% env) # if model has been run
  {
    finalPN0 = out$P.N0[length(out$P.N0)]*100  
    if (length(finalPN0) == 0) finalPN0 = NA
    if ("test" %in% env) # if an optimisation has been done
    {
      insert(text.1,sprintf('Total cost: $%s\nAnimals culled: %s\nTotal population post-cull: %s\nProportion remaining in control region: %.1f%%\nCull rate: %d%%\nTarget density: %.2f / sq. km\n\n', 
        format(sum(out$total.cost), big.mark=','), format(round(sum(out$total.cull)), big.mark=','), format(round(out$total.pop[length(out$total.pop)]), big.mark=','), finalPN0, round(test$cull.rate*100), test$target.density))
    }
    else # if a model but no optimisation
    {
      insert(text.1,sprintf('Total cost: $%s\nAnimals culled: %s\nTotal population post-cull: %s\nProportion remaining in control region: %.1f%%\n\n', 
        format(sum(out$total.cost), big.mark=','), format(round(sum(out$total.cull)), big.mark=','), format(round(out$total.pop[length(out$total.pop)]), big.mark=','), finalPN0))
    }
  }
}


# Runs a basic simulation based on the current parameters and variables
run.model = function()
{
  update.strategy.sliders() # make sure the current values in the edit bars of Management are stored into memory for the simulation
  out <<- run.sim(strategy.params, budget, fenced, strategy.params$C.mask[[1]]) # runs simulation, outputs model output to "out"
  
  draw.pop.plot() # draw population-time plot
  draw.cost.plot() # draw cost-time plot
  draw.pn0.plot() # draw proportional population plot
  curr.t <<- 1 # set current time to 1 for the spatial time series plots
  draw.pop.ts(curr.t) # draw abundance map at current timestep
  draw.cb() # draw cost-benefit map
  update.graphics() # update the text report box
}

# Creates window to select a strategy for optimisation routines
select.strategy = function()
{
  chosen.strategy <<- 0 # set global variable to 0 (a loop in the optimisers waits till it is non-zero before continuing)
  window.open <<- TRUE # prevents other windows being opened
  select.win <<- gwindow("Select strategy")
  addHandlerUnrealize(select.win, handler = function(h,...) return(TRUE)) # disable closing (will handle via "OK" and "Cancel" buttons)  
  select.pane <- ggroup(container = select.win, horizontal = FALSE)
  a.label <- glabel("Strategy to optimise over:", container = select.pane)
  select.select <- gdroplist(items = strategies.droplist[], container = select.pane)
  select.buttons <- ggroup(container = select.pane, horizontal = TRUE)
  # if OK selected, set strategy to chosen strategy from droplist and close window - optimiser will run using this strategy only
  select.ok <- gbutton('OK', container = select.buttons, handler = function(h,...) {chosen.strategy <<- svalue(select.select, index = TRUE); window.open <<- FALSE; dispose(select.win)})
  # if Cancel selected, set variable to NULL and close window - optimiser will skip
  select.cancel <- gbutton('Cancel', container = select.buttons, handler = function(h,...) {chosen.strategy <<- NULL; window.open <<- FALSE; dispose(select.win)})
}

# Runs a non-spatial budget optimisation based on the current parameters and variables
run.NSB = function()
{
  update.strategy.sliders() # make sure the current values in the edit bars of Management are stored into memory for the simulation
  select.strategy() # opens window to choose strategy for optimising
  while (chosen.strategy == 0) {} # loop waiting till window closes (TODO: find better way to deal with waiting for strategy selection)
  if (!is.null(chosen.strategy)) # if user pressed OK and not cancel
  {
    test <<- NS.B(budget, strategy.params$C.mask[[chosen.strategy]], chosen.strategy) # runs optimisation, outputs results to "test"
    if (is.na(test$cull.rate)) # if the budget is too small to give a result
    {
      confirmDialog('Budget too small!')
      rm(test, envir = globalenv())
    }
    else # run model based on found optimal parameters
    {
      out <<- run.sim.opt(chosen.strategy, test$cull.rate, test$cull.rate, test$target.density, budget, strategy.params$C.mask[[chosen.strategy]], strategy.params$C.mask[[chosen.strategy]], fenced) 
      confirmDialog('Optimisation done!')
    }
    # plot results to screen (see run.model for more detail)
    update.graphics() 
    draw.pop.plot()
    draw.cost.plot()
    draw.pn0.plot() 
    curr.t <<- 1 
    draw.pop.ts(curr.t)
    draw.cb()
  }
}


# Runs a non-spatial control-area density optimisation based on the current parameters and variables
run.NSD = function()
{
  update.strategy.sliders() # make sure the current values in the edit bars of Management are stored into memory for the simulation
  select.strategy() # opens window to choose strategy for optimising
  while (chosen.strategy == 0) {} # TODO: better way to deal with waiting for strategy selection
  if (!is.null(chosen.strategy))
  {
    test <<- NS.D(area.target, strategy.params$C.mask[[chosen.strategy]], chosen.strategy) # outputs results of optimisation to "test"
    if (is.na(test$cull.rate)) # if the required density is too small to give a result
    {
      confirmDialog('Can\'t hit target density even at 99% cull rate!')
      rm(test, envir = globalenv())
    }
    else # run model based on found optimal parameters
    {
      out <<- run.sim.opt(chosen.strategy, test$cull.rate, test$cull.rate, test$target.density, budget, strategy.params$C.mask[[chosen.strategy]], strategy.params$C.mask[[chosen.strategy]], fenced) 
      confirmDialog('Optimisation done!')
    }
    # plot results to screen (see run.model for more detail)
    update.graphics() 
    draw.pop.plot()
    draw.cost.plot()
    draw.pn0.plot()
    curr.t <<- 1 
    draw.pop.ts(curr.t)
    draw.cb()
  }
}


# Runs a spatial budget optimisation based on the current parameters and variables
run.SB = function()
{
  update.strategy.sliders() # make sure the current values in the edit bars of Management are stored into memory for the simulation
  select.strategy() # opens window to choose strategy for optimising
  while (chosen.strategy == 0) {} # TODO: better way to deal with waiting for strategy selection
  if (!is.null(chosen.strategy))
  {  
    test2 <<- S.B(budget, strategy.params$C.mask[[chosen.strategy]], chosen.strategy) # outputs results of optimisation to test2
    if (is.na(test2$test$cull.rate)) # if the budget is too small to give a result
    {
      confirmDialog('Budget too small!')
      rm(test2, envir = globalenv()) # remove test2
    }
    else # extract necessary variables from test2
    {
      optim.cull <<- test2$culling.area
      out <<- test2$out
      test <<- test2$test
      confirmDialog('Optimisation done!')
    }
    # plot results to screen (see run.model for more detail)
    draw.ocm()
    draw.pop.plot()
    draw.cost.plot()
    draw.pn0.plot() 
    curr.t <<- 1 
    draw.pop.ts(curr.t)
    draw.cb()
    update.graphics() 
  }
}


# Draws total population against time
draw.pop.plot = function()
{
  svalue(output.pad) = 1    # select the plot device
  visible(pop.plot.page)=T
  x.v = ((1:ncol(out$total.pop))-1)/seasons # timesteps (x-data)
  Rmax = max(out$total.pop) # work out range for plotting

  for (i in 1:N.species) # plot for each species
  {
    y.v= out$total.pop[i,] # extract y-axis data
    if (i == 1)
      plot(y.v ~ x.v, pch=19,xlab="Years",ylab="Population", ylim = c(0, Rmax)) # initialise plot
    else
      points(y.v ~ x.v, pch=19,xlab="Years",ylab="Population", col=i) # overlay plot
  }
  if (N.species > 1) # if we need it, generate a legend
    legend('topright', species, pch=19, col = 1:N.species)
}


# Draws total cost against time
draw.cost.plot = function()
{
  svalue(output.pad) = 2 # select the plot device
  visible(cost.plot.page)=T
  x.v = ((1:ncol(out$total.pop))-1)/seasons   # timesteps (x-data)
  y.v= c(0,out$total.cost)/1e6 # extract y-axis data (in millions of dollars)
  plot(y.v ~ x.v, pch=19,xlab="Years",ylab="Cost (million $)") # plot
  
}


# Draws proportional population in control area
draw.pn0.plot = function()
{
  if (N.strategies > 0) # only draw if at least one management strategy has been performed
  {
    svalue(output.pad) = 3  # select the plot device
    visible(pn0.plot.page)=T
    x.v = ((1:ncol(out$total.pop))-1)/seasons  # timesteps (x-data)
    Rmax = max(c(out$P.N0, 1)) # work out range for plotting (always at least 1, i.e. initial proportion)

    for (i in 1:N.strategies) # plot for each strategy
    {
      y.v = c(1,out$P.N0[i,]) # extract y-axis data (including 1 as initial value)
      if (i == 1)
        plot(y.v ~ x.v, pch=19,xlab="Years",ylab="Proportion Surviving", ylim = c(0, Rmax)) # initialise plot
      else
        points(y.v ~ x.v, pch=19,xlab="Years",ylab="Proportion Surviving", col = i)   # overlay plot
    }
    if (N.strategies > 1) # if we need it, generate a legend
      legend('topright', species[strategy.params$cull.species], pch=19, col = 1:N.strategies)
  }

}


# Draw a single frame of the population map time series
draw.pop.ts = function(t)
{
  svalue(output.pad) = 4  # select the plot device
  visible(pop.ts.page)=T
#  create a raster based on the first species' carrying capacity (including projection info)
  DATA = K[[1]]
  DATA[locs] = out$pop.ts[[t]][curr.species,] / cell.size # set its values to the input data, calculate density instead of abundance
  R = c(0,0) # initialise variable for range
  for (i in 1:length(out$pop.ts)) R = range(c(R, range(out$pop.ts[[i]][curr.species,]/cell.size))) # works out density range for species over all timesteps - inefficient but shouldn't matter
  if (R[2] == R[1]) R[2] = R[1] + 1 # make sure the difference between the range extents is nonzero
  plotdata(DATA, breaks = seq(min(R), max(R), l=255), axis.args = list(at = pretty(R), labels = pretty(R)), main = sprintf('Density (animals / km^2)\nTime\n%.1f', (t-1)/seasons)) # plot
}


# Draw cost-benefit map
draw.cb = function()
{
  if (length(out$cost.benefit)>0)
  {
    svalue(output.pad) = 5 # select the plot device
    visible(CB.page)=T
    plot.new()        # TODO: not actually sure why I put this in here!
#  create a raster based on the first species' carrying capacity (including projection info)
    DATA = K[[1]]
    DATA[locs] = out$cost.benefit # set its values to the input data
    plotdata(DATA) # plot
  }

}


# Draw optimal culling map
draw.ocm = function()
{
  svalue(output.pad) = 6 # select the plot device
  visible(OCM.page)=T
  plot.new()# TODO: not actually sure why I put this in here!
#  create a raster based on the first species' carrying capacity (including projection info)
  DATA = K[[1]]
  DATA[locs] = optim.cull   # set its values to the input data
  plotdata(DATA) # plot
}
