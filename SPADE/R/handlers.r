time.button.handler = function(h,...)
{ # if Apply button pressed, give warning - if user presses OK, goes through with it
 if (winDialog('okcancel', 'Changing the number of seasons will add repeated rows or delete rows of existing seasonal data. Continue?')=='OK')
 {
    oldseasons = seasons # store old number of seasons
    seasons <<- curr.time # store new number of seasons

    # Note: seasons is only relevant to the main GUI windows in choosing which
    # seasons in which to "manage" - we need to remove then recreate the
    # Management pane in order to reflect the new seasons

    svalue(input.pad) <<- 4 # choose Management pane
    dispose(input.pad) # delete Management pane
    
    # Update interactions
    repeats = ceiling(seasons / oldseasons) # maximum number of times repeat needed (note: will be 1 if seasons < curr.time)
    # idea is to repeat old values as many times as needed, then crop to total new number of seasons

    # TODO: make sure no rasters used in interactions, or if so take into account here
    for (n in setdiff(names(interactions[[1]][[1]]), "min.d")) # for every object in interactions except for min.d (which is constant across seasons)
    {
      for (i in 1:N.species)
      {
        for (j in 1:N.species)
        {
          tmp = interactions[[1]][[1]][[n]]
          l = dim(tmp)
          if (length(l) < 2) 
            interactions[[1]][[1]][[n]] <<- rep(tmp, repeats)[1:seasons] # if not an array, update with new values in 1d
          else # if a 2d array (assuming this is boxes or something that works similarly)
          {
            interactions[[1]][[1]][[n]] <<- matrix(NA, l[1], seasons)
            for (i in 1:l[1]) interactions[[1]][[1]][[n]][i,] <<- rep(tmp[i,], repeats)[1:seasons]
          }
        }
      }
    }    
    
    if (!is.null(strategy.params$cull.seasons)) # if there are actually management strategies defined
    {
      if (seasons > oldseasons) # if we have added seasons
      {
        # create a temporary matrix with each row representing a strategy, and columns for seasons
        # default values are TRUE
        tmp = matrix(TRUE, nrow(strategy.params$cull.seasons), seasons)
        tmp[,1:oldseasons] = strategy.params$cull.seasons[, 1:oldseasons] # copy the old seasons information into the temp matrix
        strategy.params$cull.seasons <<- tmp # set the seasonal management parameter to the temp matrix
        strategy.params$cull.seasons[is.na(strategy.params$cull.seasons)] <<- TRUE # any NA values are set to TRUE
      }
      else # otherwise, we have removed seasons (or kept them the same for some reason)
      {
        # choose the relevant subset of the old matrix for our new matrix - we use as.matrix because otherwise the seasons=1 case fails
        strategy.params$cull.seasons <<- as.matrix(strategy.params$cull.seasons[, 1:seasons])
        strategy.params$cull.seasons[is.na(strategy.params$cull.seasons)] <<- TRUE # any NA values are set to TRUE
      }
    }
    man.group <<- ggroup(container=input.pad, label="Management", horizontal = FALSE)  # create Management pane
    create.man.window() # populate the Management pane with current information
 }
}

# strategy droplist handler:
# handles when the currently selected strategy is changed
sdh = function(h=NULL, ...)
{
  # update strategy.params with the values of the current text boxes
  # ensures that strategy.params is up to date before changing the strategy
  update.strategy.sliders()

  # update curr.strategy to new value from current droplist place
  curr.strategy <<- svalue(strategies.droplist, index = TRUE)

  if (!is.null(curr.strategy)) # if there is actually something selected
  {
    # update all window objects from the stored strategy.params values in new droplist location
    svalue(culling.droplist, index = TRUE) = strategy.params$cull.species[curr.strategy]
    for (i in 1:seasons)
      svalue(seasons.boxes[[i]]) = strategy.params$cull.seasons[curr.strategy, i]
    svalue(area.target.slider) = strategy.params$area.target[curr.strategy]
    svalue(culling.method, index = TRUE) = strategy.params$culling.choice[curr.strategy] # define and use culling.choice
    a = svalue(culling.method, index = TRUE)
    visible(init.cull.group) = (a == 1)
    visible(maint.cull.group) = (a == 1)
    visible(init.cull.abs.group) = (a == 2)
    visible(maint.cull.abs.group) = (a == 2)
    svalue(init.cull.slider) = strategy.params$init.cull[curr.strategy]
    svalue(maint.cull.slider) = strategy.params$maint.cull[curr.strategy]
    svalue(init.cull.abs.slider) = strategy.params$init.cull.abs[curr.strategy]
    svalue(maint.cull.abs.slider) = strategy.params$maint.cull.abs[curr.strategy]
    svalue(target.density.slider) = strategy.params$target.density[curr.strategy]
    svalue(cb.a.slider) = strategy.params$cb.a[curr.strategy]
    svalue(cb.b.slider) = strategy.params$cb.b[curr.strategy]
    svalue(cb.c.slider) = strategy.params$cb.c[curr.strategy]
    svalue(cb.method, index = TRUE) = strategy.params$cb.choice[curr.strategy]    
  }
}

add.strategy.handler = function(h,...)
{
  if (!is.null(K.rast[[1]])) # if the first species' carrying capacity is defined
  {
    # update number of currently defined strategies, and also which labeling number we're up to
    N.strategies <<- N.strategies + 1
    strategy.no <<- strategy.no + 1
    # show the box containing all the parameters if it wasn't already visible
    visible(strategy.params.group) = TRUE

    # stored param values into strategy.params from numbers currently on gtext objects (using previous droplist place)
    update.strategy.sliders()

    # store defaults to strategy.params
    strategy.params$cull.species[N.strategies] <<- 1
    strategy.params$cull.seasons <<- rbind(strategy.params$cull.seasons, rep(TRUE, seasons))
    strategy.params$area.target[N.strategies] <<- 0.01
    strategy.params$culling.choice[N.strategies] <<- 1
    strategy.params$init.cull[N.strategies] <<- 0.5
    strategy.params$maint.cull[N.strategies] <<- 0.5
    strategy.params$init.cull.abs[N.strategies] <<- 1000
    strategy.params$maint.cull.abs[N.strategies] <<- 1000
    strategy.params$target.density[N.strategies] <<- 1
    strategy.params$cb.a[N.strategies] <<- 0.1464
    strategy.params$cb.b[N.strategies] <<- -1.445
    strategy.params$cb.c[N.strategies] <<- 1180
    strategy.params$cb.choice[N.strategies] <<- 1
    # all rasters here default to 1 everywhere, using template of first carrying capacity raster
#    strategy.params$EL.rast[[N.strategies]] <<- K.rast[[1]]*0 + 1
#    strategy.params$EL[[N.strategies]] <<- raster::as.matrix(strategy.params$EL.rast[[N.strategies]])
    strategy.params$PR.rast[[N.strategies]] <<- K.rast[[1]]*0 + 1
    strategy.params$PR[[N.strategies]] <<- raster::as.matrix(strategy.params$PR.rast[[N.strategies]])
    strategy.params$C.rast[[N.strategies]] <<- K.rast[[1]]*0 + 1
    strategy.params$C.mask[[N.strategies]] <<- raster::as.matrix(strategy.params$C.rast[[N.strategies]])
    # other rasters default to null
    strategy.params$TD.rast[[N.strategies]] <<- list()
    strategy.params$TD[[N.strategies]] <<- list()
    strategy.params$CB.B.rast[[N.strategies]] <<- list()
    strategy.params$CB.B[[N.strategies]] <<- list()
    strategy.params$CB.A.rast[[N.strategies]] <<- list()
    strategy.params$CB.A[[N.strategies]] <<- list()
    strategy.params$CB.C.rast[[N.strategies]] <<- list()
    strategy.params$CB.C[[N.strategies]] <<- list()

    if (N.strategies > 1) # if we already started with one or more strategies, add new strategy to the end of the droplist
      strategies.droplist[] = c(strategies.droplist[], sprintf("Strategy %d", strategy.no))
    else # otherwise, set this strategy as Strategy 1 (the only strategy is ALWAYS labelled as number 1)
      strategies.droplist[] = "Strategy 1"

    svalue(strategies.droplist, index = TRUE) = N.strategies # set selected droplist to new strategy

    # TODO: why are we not just going straight to N.strategies? or defining curr.strategy?
    # and why do we need to plot these anyway if it's a new strategy? and why not other rasters?

    if (!is.null(curr.strategy)) # if a strategy is selected
    {
      if (length(strategy.params$PR.rast[[curr.strategy]]) > 0) # if opening window with Priority raster already present
      {
        DATA = raster::as.matrix(strategy.params$PR.rast[[curr.strategy]]) # use currently selected Priority raster strategy
        svalue(strats.raster.pad) = 1 # choose plot window
        visible(PR.plot.raster)=T
        plotdata(DATA) # plot data
      }

      if (length(strategy.params$C.rast[[curr.strategy]]) > 0) # if opening window with Culling Mask raster already present
      {
        DATA = raster::as.matrix(strategy.params$C.rast[[curr.strategy]]) # use currently selected Culling Mask raster strategy
        svalue(strats.raster.pad) = 2 # choose plot window
        visible(C.plot.raster)=T
        plotdata(DATA) # plot data
      }
    }

  }
  else # if the first species' carrying capacity is not yet defined
    confirmDialog("Cannot add strategy until spatial information has been defined for at least the first species") # complain about it
}
  
  
rem.strategy.handler =  handler = function(h, ...){
  if (N.strategies > 0) # only do anything if any strategies exist
  {
    N.strategies <<- N.strategies - 1  # reduce total number of strategies

    if (N.strategies == 0) # if we're deleting the only strategy
    {
      visible(strategy.params.group) = FALSE # make parameters invisible (as there are now no strategies)
      strategies.droplist[] = "None"  # show that there are no strategies on the droplist
      curr.strategy <<- 1 # set current strategy number to 1
      svalue(strategies.droplist, index = TRUE) = curr.strategy # select the "None" option
      strategy.no <<- 0 # reset the strategy numbering system - now we restart from 1 for any new strategies
      strategy.params = list( # re-initialise strategy.params
        cull.species = NULL, cull.seasons = NULL, area.target = NULL, culling.choice = NULL, init.cull = NULL,
        maint.cull = NULL, init.cull.abs = NULL, maint.cull.abs = NULL, target.density = NULL,
        cb.a = NULL, cb.b = NULL, cb.c = NULL, cb.choice = NULL, EL = list(NULL), PR = list(NULL), C.mask = list(NULL),
        EL.rast = list(NULL), PR.rast = list(NULL), C.rast = list(NULL), CB.A = list(NULL), TD = list(NULL), CB.B = list(NULL), CB.C = list(NULL),
        CB.A.rast = list(NULL), CB.B.rast = list(NULL), CB.C.rast = list(NULL), TD.rast = list(NULL)
      )
    }
    else # if there are still other strategies left post-deletion
    {
      p = svalue(strategies.droplist, index = TRUE) # work out which strategy we are deleting
      # remove that strategy's information from every element of strategy.params
      strategy.params$cull.species <<- strategy.params$cull.species[-p]
      strategy.params$cull.seasons <<- as.matrix(strategy.params$cull.seasons[-p,]) # as.matrix for case where 1 strategy left
      strategy.params$area.target <<- strategy.params$area.target[-p]
      strategy.params$culling.choice <<- strategy.params$culling.choice[-p]
      strategy.params$init.cull <<- strategy.params$init.cull[-p]
      strategy.params$maint.cull <<- strategy.params$maint.cull[-p]
      strategy.params$init.cull.abs <<- strategy.params$init.cull.abs[-p]
      strategy.params$maint.cull.abs <<- strategy.params$maint.cull.abs[-p]
      strategy.params$target.density <<- strategy.params$target.density[-p]
      strategy.params$cb.a <<- strategy.params$cb.a[-p]
      strategy.params$cb.b <<- strategy.params$cb.b[-p]
      strategy.params$cb.c <<- strategy.params$cb.c[-p]
      strategy.params$cb.choice <<- strategy.params$cb.choice[-p]      
#      strategy.params$EL.rast[[p]] <<- NULL 
#      strategy.params$EL[[p]] <<- NULL
      strategy.params$PR.rast[[p]] <<- NULL  # note: setting an element of a list to NULL removes it entirely!
      strategy.params$PR[[p]] <<- NULL
      strategy.params$C.rast[[p]] <<- NULL
      strategy.params$C.mask[[p]] <<- NULL
      strategy.params$TD.rast[[p]] <<- NULL
      strategy.params$TD[[p]] <<- NULL
      strategy.params$CB.B.rast[[p]] <<- NULL
      strategy.params$CB.B[[p]] <<- NULL
      strategy.params$CB.A.rast[[p]] <<- NULL
      strategy.params$CB.A[[p]] <<- NULL
      strategy.params$CB.C.rast[[p]] <<- NULL
      strategy.params$CB.C[[p]] <<- NULL
      strategies.droplist[] = strategies.droplist[-p] # finally, remove the strategy from the droplist
      # set the currently displayed strategy to the element after the removed element, or,
      # if the last element was removed, the newly-last element
      svalue(strategies.droplist, index = TRUE) = min(p, N.strategies)
      # (note: changing the svalue AUTOMATICALLY calls the relevant handler)
    }
  }

}

cost.raster.check.handler = function(h,...)
{
  # update strategy.params with the values of the current text boxes
  # ensures that strategy.params is up to date before changing things
  update.strategy.sliders()
  use.cost.rasters <<- svalue(h$obj) # set whether we're using rasters based on the check box
  visible(cost.raster.group1) = use.cost.rasters # set visibility of the four separate groups
  visible(cost.raster.group2) = use.cost.rasters
  visible(spatially.implicit.group1) = !use.cost.rasters
  visible(spatially.implicit.group2) = !use.cost.rasters
  if (use.cost.rasters) # if we are using rasters
  {
    # set each raster to be the previously defined values for each cell, using the first species'
    # carrying capacity as a template
    strategy.params$TD.rast[[N.strategies]] <<- K.rast[[1]]*0 + strategy.params$target.density[curr.strategy]
    strategy.params$TD[[N.strategies]] <<- raster::as.matrix(strategy.params$TD.rast[[N.strategies]])
    strategy.params$CB.B.rast[[N.strategies]] <<- K.rast[[1]]*0 + strategy.params$cb.b[curr.strategy]
    strategy.params$CB.B[[N.strategies]] <<- raster::as.matrix(strategy.params$CB.B.rast[[N.strategies]])
    strategy.params$CB.A.rast[[N.strategies]] <<- K.rast[[1]]*0 + strategy.params$cb.a[curr.strategy]
    strategy.params$CB.A[[N.strategies]] <<- raster::as.matrix(strategy.params$CB.A.rast[[N.strategies]])
    strategy.params$CB.C.rast[[N.strategies]] <<- K.rast[[1]]*0 + strategy.params$cb.c[curr.strategy]
    strategy.params$CB.C[[N.strategies]] <<- raster::as.matrix(strategy.params$CB.C.rast[[N.strategies]])
  }
  else # if we are using a single value
  {
    # set all the rasters to null
    strategy.params$TD.rast[[N.strategies]] <<- list()
    strategy.params$TD[[N.strategies]] <<- list()
    strategy.params$CB.B.rast[[N.strategies]] <<- list()
    strategy.params$CB.B[[N.strategies]] <<- list()
    strategy.params$CB.A.rast[[N.strategies]] <<- list()
    strategy.params$CB.A[[N.strategies]] <<- list()
    strategy.params$CB.C.rast[[N.strategies]] <<- list()
    strategy.params$CB.C[[N.strategies]] <<- list()
  }

  visible(TD.plot.raster)=T # set the plot window
  svalue(strats.raster.pad)=3
  # plot the raster (note: plotdata can handle null rasters)
  plotdata(strategy.params$TD[[curr.strategy]])

  visible(CB.B.plot.raster)=T # set the plot window
  svalue(strats.raster.pad)=4
  plotdata(strategy.params$CB.B[[curr.strategy]]) # plot the raster

  visible(CB.A.plot.raster)=T # set the plot window
  svalue(strats.raster.pad)=5
  plotdata(strategy.params$CB.A[[curr.strategy]]) # plot the raster

  visible(CB.C.plot.raster)=T # set the plot window
  svalue(strats.raster.pad)=6
  plotdata(strategy.params$CB.C[[curr.strategy]])  # plot the raster
}

time.slider.handler = function(h,...) # handler for when number is changed in a time slider
{
  istime <<-FALSE; # make sure that we're not setting the parameters for ALL timesteps
  vars.update(list(obj = settings)) # store current parameters and update displayed parameters to new timestep
}

time.box.handler = function(h,...) # handler for when Keep Parameters Constant box is checked/unchecked
{
  # make sure we're setting parameters for ALL timesteps
  # (this is valid even if we're disabling the checkbox,
  # as we only want to change parameters individually AFTER
  # the current parameters are updated)
  istime <<- TRUE;
  vars.update(list(obj = settings)); # store current parameters to ALL timesteps, and update displayed parameters to new timestep
  # if we are keeping params constant, disable the time slider
  # (note that the time.slider handler assumes that the number is
  # only being changed when params are *not* being kept constant)
  enabled(time.slider) = !svalue(h$obj)
}

settings.OK.handler = function(h,...)
{
  istime <<- FALSE; # only update for current timestep (TODO: why?? also applies for other cases)
  vars.update(list(obj = settings)); #
  update.elements();
  dispose(settings);
  window.open <<- FALSE
}

settings.Cancel.handler = function(h,...) # with handler for when button pressed
{
  cancel <<- TRUE;
  update.elements();
  dispose(settings);
  window.open <<- FALSE
}

################################################################################################
# ff.handler: Define droplist labels in settings windows, and whether edit boxes are editable
################################################################################################

ff.handler = function(h,...) # droplist for various interactions (selecting functional form)
{
  # determines which window this has been called from and loads appropriate functional forms
  if (id(h$obj) == 1) func.forms = predprey.forms # predator-prey
  if (id(h$obj) == 2) func.forms = demo.forms # demographics
  if (id(h$obj) == 3) func.forms = disease.forms # disease
  if (id(h$obj) == 4) func.forms = diffusion.forms # diffusion

  curr.val = svalue(h$obj, index = TRUE) # set current value as the user selection

  if (id(h$obj) == 4) # for diffusion
    svalue(settings.difflabel) <<- as.character(func.forms$Model.formula[curr.val]) # show model formula for current selection on special diffusion label
  else # otherwise
    svalue(settings.label) <<- as.character(func.forms$Model.formula[curr.val]) # show model formula for current selection on current label

  # extract all possible unique variables for all formulas for interactions of the selected type
  variables.list = unique(unlist(strsplit(as.character(func.forms$Variables), ' ')))
  # select variables from current functional form
  curr.variables.list = strsplit(as.character(func.forms$Variables[curr.val]), ' ')[[1]]
  # select default variable values from current functional form
  curr.defaults.list = strsplit(as.character(func.forms$Defaults[curr.val]), ' ')[[1]]

  for (v in variables.list) # for all possible variables
  {
    # determines whether it's one of our current variables and hence whether it should be displayed
    is.vis = v %in% curr.variables.list
    eval(parse(text = paste('visible(settings.', v, ') <<- ', is.vis, sep=''))) # sets its visibility

    if (is.vis) # if it is visible
    {
      # work out whether it is currently defined or is NA
      var.is.na = is.na(eval(parse(text = paste('variables.', v, '[curr.time]', sep=''))))
      if (var.is.na) # if the variable is NA at current time, change it to the default defined by the current functional form
      {
        # set the variable to the default
        eval(parse(text = paste('variables.', v, '[curr.time] <<- ', curr.defaults.list[which(v == curr.variables.list)], sep='')))
        # set its label to the default
        eval(parse(text = paste('svalue(settings.', v, '.edit) = ', curr.defaults.list[which(v == curr.variables.list)], sep='')))
      }
    }
  }
}


K.button.handler = function(h,...)
{ # handler for when button pressed
  # load user-specified raster information into allthedata
  allthedata = raster.load.button.handler()
  if (!is.null(allthedata)) # if it worked
  {
    # make sure geometry is calculated when model run
    K.changed <<- TRUE
    # set global variables to necessary data
    K.file = allthedata$the.file
    K.rast[[ii]] <<- allthedata$the.rast
    # boolean for whether we're ready to populate default rasters
    # for global dispersal and fencing information
    # based on the first species' carrying capacity info
    ready = FALSE
    # set to true if we're defining the first species' CC info
    # (now set to work ANY time we define it, removed "is.null(K[[1]])" )
    if (ii == 1) ready = TRUE
    K[[ii]] <<- allthedata$the.data
    svalue(K.label)=allthedata$the.file # set label
    visible(K.plot.raster)=T # select plot window
    plotdata(K[[ii]]/cell.size, main = 'Density (per km^2)') # plot data (density, not abundance)
    if (ready) 
    {
      default.rasters() # create default rasters (from gui.r)
      projection = crs(K.rast[[1]], asText = TRUE)
      if (!is.na(projection))
      {
        if (projection != 'NA')
        {
          res = res(K.rast[[1]])
          if (grepl('longlat', projection)) # no coord system
          {
            mean.lat = mean(coordinates(K.rast[[1]])[,2])
            cell.coords = c(cos(mean.lat * pi/180),1) * res * 40004/360 # estimate of cell sides in km
            svalue(cell.size.x.slider) = cell.coords[1] # calling svalue should also auto-update values of cell.size.x and y, as well as value of cell.size
            svalue(cell.size.y.slider) = cell.coords[2]
          }
          else # has coord system in some units, just dump straight in (TODO: support other units than km)
          {
            svalue(cell.size.x.slider) = res[1]
            svalue(cell.size.y.slider) = res[2]
          }
        }
      }
                  
    }
    # if there is no initial condition info for the current raster
    # (would use is.null as test but doesn't pick up empty lists)
    if (length(IC[[ii]]) == 0)
    {
      visible(IC.plot.raster)=T # select plot window
      # set IC info to just loaded carrying cap info
      IC.rast[[ii]] <<- K.rast[[ii]]
      IC[[ii]] <<- raster::as.matrix(IC.rast[[ii]]) # set matrix info
      plotdata(IC[[ii]]/cell.size, main = 'Density (per km^2)') # plot data (density, not abundance)
    }
    if (length(EL.species[[ii]]) == 0)
    {
      visible(EL.plot.raster)=T # select plot window
      # set IC info to 0 using CC info as template
      EL.rast[[ii]] <<- K.rast[[1]]*0 + 1
      EL.species[[ii]] <<- raster::as.matrix(EL.rast[[ii]]) # set matrix info
      plotdata(EL.species[[ii]]) # plot data
    }
  }
}


# ---------------
# Culling methods
# ---------------
prop.abs.cull.calc = function(pop, p, abs, TD, eps, cull.mask) # capped proportional method for discrete model
{
  ii = which(pop > TD & pop > eps & cull.mask > 0) # calculate which cells to target

  cull = 0*pop # initialise
  if (length(ii) > 0) # if there is anything to do
  {
    p.abs = abs/sum(pop[ii]*cull.mask[ii]) # proportion to be culled to reach maximum absolute cull (abs)

    # proportion of pop that can be culled before reaching minimum density, 
    # scaled by mask (lower mask value means lower practical cull rate compared to others, meaning higher overall cull rate possible)
    p.lim = (1 - eps/pop[ii])/cull.mask[ii]
    
    # if specified proportion p is not higher than the maximum,
    # plan to cull proportion p.min == p of every targeted cell
    # otherwise plan to cull enough of every targeted cell to cull the maximum number of animals (abs)
    p.min = min(p, p.abs)
                                                        
    if (all(p.min < p.lim)) # if no cell will go under the minimum density, then go ahead with plan
    {
      cull = 0*pop # initialise
      cull[ii] = p.min * pop[ii] * cull.mask[ii] # set proportion for targeted cells
    }
    else # if at least one cell will reach minimum density, new plan:
    {
      # test to see if we will exceed the maximum number to cull (abs) by culling proportion p
      cull = 0*pop # initialise
      cull[ii] = pmin.int(p * pop[ii] * cull.mask[ii], pop[ii] - eps) # remove proportion p while ensuring no cell goes under minimum
      # if we don't exceed abs, then keep this as our result...
      
      if (sum(cull) > abs) # otherwise, work out the proportional removal that matches our required maximum
      {
        ij = order(p.lim) # work out in which order cells will hit their limits
        p.lim = p.lim[ij] # put limits in order
        pop.sort = pop[ii][ij] # put population values in order of limit hit
        cull.mask.sort = cull.mask[ii][ij]
  
        at.limits = cumsum(pop.sort - eps) # calculate limit of removal for all cells at or below each p.lim
        under.limits = c(rev(cumsum(rev(pop.sort[-1] * cull.mask.sort[-1]))),0) * p.lim # calculate removal from all other cells (before limit) at each p.lim
        limit = at.limits + under.limits # total removal at each p.lim
  
        index = max(which(limit <= abs)) # work out which cells will hit their limits at abs
        prop = (abs - limit[index]) / (limit[index+1] - limit[index]) # work out how far along we need to go with the remaining unlimited cells
        p.soln = p.lim[index] + prop*(p.lim[index+1] - p.lim[index]) # work out final proportion to achieve exactly abs removal
  
        cull = 0*pop # initialise
        cull[ii] = pmin.int(p.soln * cull.mask[ii] * pop[ii], pop[ii] - eps) # remove proportion p.soln while ensuring no cell goes under minimum
      }
    }
  }
  cull
}

abs.cull.calc = function(pop, abs, TD, eps, cull.mask) # absolute method for discrete model
{
  ii = which(pop > TD & pop > eps & cull.mask > 0) # calculate which cells to target

  cull = pop * 0 # initialise
  if (length(ii) > 0) # if there is anything to do
  {  
    cull[ii] = 1 # mark removable cells
    cull = cull * cull.mask # mask out unmanaged (or partly managed) areas  
    cull = cull * abs / sum(cull) # scale so total removal is abs
  
    # if abs removal won't put any cell under minimum density, keep. otherwise...
  
    if (!all((pop[ii] - cull[ii]) > eps)) # if at least one cell will reach minimum
    {
      a.lim = (pop[ii] - eps)/cull.mask[ii] # work out max absolute cull limit per cell (scaled by cull mask)
      ij = order(a.lim) # work out in which order cells will hit their limits
      pop.sort = pop[ii][ij] - eps # put removable population values in order of limit hit
      a.lim = a.lim[ij]
      cull.mask.sort = cull.mask[ii][ij]
      at.limits = cumsum(pop.sort) # calculate limit of removal for all cells at or below each a.lim
  #    under.limits = pop.sort * ((N-1):0) # calculate removal from all other cells (before limit) at each a.lim (simple case where cull.mask is 1 everywhere, left in for reference)
      under.limits = c(rev(cumsum(rev(cull.mask.sort[-1]))),0) * a.lim # calculate removal from all other cells (before limit) at each a.lim
      limit = at.limits + under.limits # total removal at each a.lim
  
      index = max(which(limit <= abs)) # work out which cells will hit their limits at abs
      if (index < length(limit))
      {
        prop = (abs - limit[index]) / (limit[index+1] - limit[index]) # work out how far along we need to go with the remaining unlimited cells
        p.soln = a.lim[index] + prop*(a.lim[index+1] - a.lim[index]) # work out final proportion to achieve exactly abs removal
      }
      else
        p.soln = a.lim[index]
  
      cull = 0*pop # initialise
      cull[ii] = pmin.int(p.soln * cull.mask[ii], pop[ii] - eps) # remove p.soln * cull.mask per cell while ensuring no cell goes under minimum
    }
  }
  cull
}

# ---
# Calculate all culling
# ---


# assuming: 

#      if (N.strategies>0) # if management options have been specified
#      {

#      all.cull = pop * 0  # initialise the (spatially explicit) number of managed animals for this season

calc.cull = function(pop, t, t2, params, cull.mask)
{
  all.cull = matrix(0, N.species, nrow(locs)) # initialise the (spatially explicit) number of managed animals for this season
  all.CC = pop[1,] * 0 # initialise the (spatially explicit) cost of management for this season                      
  total.CB = pop[1,] * 0 # initialise the (spatially explicit) cost of management for this season                      
  
  if (N.strategies > 0)
  {
    for (i in 1:N.strategies) # for each defined strategy
    {
      s = params$cull.species[i] # find which species is managed in this strategy
    
      # set "TD.curr" to represent target density information for this strategy
      if (length(params$TD[[i]])==0) # if a target density raster isn't defined 
        TD.curr = params$target.density[i] * cell.size # use the set value
      else
        TD.curr = params$TD[[i]][locs] * cell.size # use the raster
        
      eps = interactions[[s]][[s]]$min.d * cell.size

      cullable.cells = sum(pop[s,cull.mask[[i]]>0] > eps & pop[s,cull.mask[[i]]>0] > TD.curr)

      if (params$cull.seasons[i,t2] & cullable.cells > 0) # if we're in the right season(s) for management and there are cullable cells above targets
      {
        if (params$culling.choice[i] == 1) # if proportional management specified
        {
          if (timesteps == 1) # directly take out proportion of population
          {
            if (t == 1) # if in first year, perform 'initial' management, otherwise do 'maintenance' (scaled by cull.mask)
              CULL = pop[s,] * params$init.cull[i] * cull.mask[[i]]
            else
              CULL = pop[s,] * params$maint.cull[i] * cull.mask[[i]]
          }
          else # continuously take out a proportion that would result in removing required percentage annually in an otherwise stable population
          {
            if (t == 1) # if in first year, perform 'initial' management, otherwise do 'maintenance' (scaled by cull.mask)
              CULL = pop[s,] * -log(1 - params$init.cull[i]) * cull.mask[[i]]
            else
              CULL = pop[s,] * -log(1 - params$maint.cull[i]) * cull.mask[[i]]
          }
        }
        else if (params$culling.choice[i] == 2) # if absolute management specified
        {
          if (timesteps == 1) # discrete model
          {
            if (t == 1)  # if in first year, perform 'initial' management, otherwise do 'maintenance'
            {
              CULL = abs.cull.calc(pop[s,], params$init.cull.abs[i], TD.curr, eps, cull.mask[[i]])
      #                CULL = rep(params$init.cull.abs[i]/cull.cells[i], length(pop[s,]))
            }
            else
            {
              CULL = abs.cull.calc(pop[s,], params$maint.cull.abs[i], TD.curr, eps, cull.mask[[i]])
      #                CULL = rep(params$maint.cull.abs[i]/cull.cells[i], length(pop[s,]))
            }
          }
          else # continuous model
          {
            CULL = pop[s,] * 0 # initialise
            CULL[pop[s,] > TD.curr & pop[s,] > eps] = 1 # mark removable cells
            CULL = CULL * cull.mask[[i]] # mask out unmanaged (or partly managed) areas       
                 
            if (t == 1)  # if in first year, perform 'initial' management, otherwise do 'maintenance'
            {
              CULL = CULL * params$init.cull.abs[i] / sum(CULL) # scale so 
            }
            else
            {
              CULL = CULL * params$maint.cull.abs[i] / sum(CULL)
            }
          }
    
        }
        else # choice 3: 'capped proportional' up to absolute limit specified
        {
          if (timesteps == 1) # discrete model
          {
            if (t == 1) # if in first year, perform 'initial' management, otherwise do 'maintenance'
            {
              CULL = prop.abs.cull.calc(pop[s,], params$init.cull[i], params$init.cull.abs[i], TD.curr, eps, cull.mask[[i]])
      #                CULL = rep(params$init.cull.abs[i]/cull.cells[i], length(pop[s,]))
            }
            else
            {
              CULL = prop.abs.cull.calc(pop[s,], params$init.cull[i], params$maint.cull.abs[i], TD.curr, eps, cull.mask[[i]])
      #                CULL = rep(params$maint.cull.abs[i]/cull.cells[i], length(pop[s,]))
            }            
          }
          else # continuous model
          {
            CULL = pop[s,] * 0 # initialise
            CULL[pop[s,] > TD.curr & pop[s,] > eps] = 1 # mark removable cells
            CULL = CULL * cull.mask[[i]] # mask out unmanaged (or partly managed) areas
            
            if (t == 1)  # if in first year, perform 'initial' management, otherwise do 'maintenance'
            {
              cull.abs = params$init.cull.abs[i]
              cull.prop = params$init.cull[i]
            }
            else
            {
              cull.abs = params$maint.cull.abs[i]
              cull.prop = params$maint.cull[i]
            }
              
            # calculate rate required to remove absolute number at based on constant overall rate
            # (note that removal is still proportional between cells)
            p.abs = 1 - exp(-cull.abs / (sum(CULL * pop[s,])))
            
            # if the user-set proportion is too high, set to the absolute number rate
            if (cull.prop > p.abs) cull.prop = p.abs
            
            # set rate as for continuous proportional model
            CULL = pop[s,] * CULL * -log(1-cull.prop)
          }
        }
    
        # for each cell, can remove at *most* enough to bring down to threshold in discrete mode
        if (timesteps == 1)
          CULL[CULL > (pop[s,] - eps)] = (pop[s,] - eps)[CULL > (pop[s,] - eps)]
        else # in continuous mode, just cut cull rate to zero if threshold already crossed
          CULL[pop[s,] <= eps] = 0
    
        # don't remove from any cells that are already under the target density (either mode)
        CULL[pop[s,] <= TD.curr] = 0 # use the set value

        all.cull[s,] = all.cull[s,] + CULL # add to total management record
    
    

        
        if (length(params$CB.A) > 0) # if there are actually any elements in cost intercept raster
        {
          if (length(params$CB.A[[i]]) > 0) # additionally, if the current element has content
          {
            # work out cost of managing each cell using cost intercept, slope and hourly cost rasters
            if (params$cb.choice == 1)
              CC = (params$CB.A[[i]][locs]*(pop[s,]/cell.size)^params$CB.B[[i]][locs])*CULL*params$CB.C[[i]][locs]
            else
              CC = CULL * (params$CB.A[[i]][locs] + params$CB.B[[i]][locs] * exp(-params$CB.C[[i]][locs] * (pop[s,] / cell.size)))
          }
          else
          {
            # work out cost of managing each cell using the set values
            if (params$cb.choice == 1)  
              CC = (params$cb.a[i]*(pop[s,]/cell.size)^params$cb.b[i])*CULL*params$cb.c[i]
            else
              CC = CULL * (params$cb.a[i] + params$cb.b[i] * exp(-params$cb.c[i] * (pop[s,] / cell.size)))
          }
        }
        else # work out cost of managing each cell using the set values
        {
          if (params$cb.choice == 1)  
            CC = (params$cb.a[i]*(pop[s,]/cell.size)^params$cb.b[i])*CULL*params$cb.c[i]
          else
            CC = CULL * (params$cb.a[i] + params$cb.b[i] * exp(-params$cb.c[i] * (pop[s,] / cell.size)))
        }        
        
        # pop^cb.a can be inf, so fixing up any cases where Inf * 0 = NaN, should be 0
        CC[is.nan(CC)] = 0 
        
        CC = CC + fence.maintain.cost * fence.place # fencing maintain cost every timestep
        if (t2 == 1 & t == 1) # if first timestep in first year
          CC = CC + fence.build.cost * fence.place # fencing build cost incorporated in first timestep
        
        all.CC = all.CC + CC # add cost info to total cost record
        
        # work out cost-benefit value for each cell (note - based on *allocated budget*, not actual budget which is not yet known)
        # TODO: change to something more relevant for conservation/management targets?
#        CB = ((CC+1)/((budget/duration)/(cull.cells[i])))/params$PR[[i]][locs]  
#        CB = ((CC+1)/(budget/duration))/params$PR[[i]][locs]   # TODO: check STAR code to see if compares

        CB = CC # equate to cost, rescale later by overall benefit (i.e. 1 / sum(N * PR))
        
        total.CB = total.CB + CB # add cost-benefit info to total cost record

      }
    }
  }
  list(cull = all.cull, CC = sum(all.CC), CB = total.CB)
}
