#!/usr/bin/Rscript

#setwd("C:/Users/nick/Desktop/spade/spade.GTK.nick/spade"); source("init.r")

## Initiate spade model

# Define toolkit as GTK
options(guiToolkit="RGtk2")

## Load required libraries - insert required libraries into this list rather than calling them directly.
## This enables them to be auto-installed if possible
packages = c("gWidgets","gWidgetsRGtk2","raster","rgdal","fields","RGtk2","cairoDevice","deSolve","RgoogleMaps")

for(the.package in packages){
  if (!require(the.package,character.only=T)) {
    inst.text=paste("install.packages('",the.package,"')",sep="")
    eval(parse(text=inst.text))
  }
  if (!require(the.package,character.only=T)) {
    print(paste("Could not install package '",the.package,"', please contact your sysadmin.",sep=""))
    return() 
  }
}


## Load default parameters, or a specified file.  If a new file is being loaded (ie. after interface
## definition), then update slider contents to reflect new file

load_params=function(the_file=NULL,new_file=F){
  if (is.null(the_file))
  {
    package.dir = path.package('SPADE')
    the_file = paste(package.dir, '/extdata/params.csv', sep='')
  }
  DATA=read.csv(the_file,header=F)
  
  eval(parse(text=paste(DATA$V1,"<<-",DATA$V2,sep="")))
  if(new_file){
    for(the_row in seq_along(DATA$V1)){
      to_run=paste("svalue(",DATA$V1[the_row],".slider)=",DATA$V2[the_row],sep="")
      eval(parse(text=to_run),envir=.GlobalEnv)
    }
  }
}

## Save parameters to a new file

save_params=function(the_file="params.csv"){
  param_list=c('budget','area.target','elev.d.mod','init.cull','maint.cull','target.density','cell.size','r.m','theta','min.d','disp.max','disp.min','cost.int','cost.slope','helicopter.cost','fence.failure', 'fence.build.cost', 'fence.maintain.cost', 'duration', 'timesteps')
  params = c(budget,area.target,elev.d.mod,init.cull,maint.cull,target.density,cell.size,r.m,theta,min.d,disp.max,disp.min,cost.int,cost.slope,helicopter.cost,fence.failure,fence.build.cost,fence.maintain.cost,duration,timesteps)
  save_frame=data.frame(param_list,params,stringsAsFactors=F)
  write.table(save_frame,the_file,col.names=F,row.names=F,sep=",",quote=F)
}

#
update.params = function() # update parameters from edits and spinbuttons before running any sim
{
  strategy.params$cull.species <<- svalue(culling.droplist, index=TRUE)
  budget <<- as.double(svalue(budget.slider))
  area.target <<-svalue(area.target.slider)
  init.cull <<-svalue(init.cull.slider)
  maint.cull <<-svalue(maint.cull.slider)
  target.density <<- as.double(svalue(target.density.slider))
  cell.size <<- as.double(svalue(cell.size.slider))
  cost.int <<- as.double(svalue(cost.int.slider))
  cost.slope <<- as.double(svalue(cost.slope.slider))
  helicopter.cost <<- as.double(svalue(helicopter.cost.slider))
  duration <<- as.double(svalue(duration.slider))
  timesteps <<-svalue(timesteps.slider)
  fence.failure <<- svalue(fence.failure.slider)
  fence.build.cost <<- as.double(svalue(fence.build.cost.slider))
  init.cull.abs <<- as.double(svalue(init.cull.abs.slider))
  maint.cull.abs <<-as.double(svalue(maint.cull.abs.slider))  
  time.select[] <<- seq(from = 0, to = duration, by = 1/seasons)
}





# Not needed now we're making it a package
## Legacy UI functions, primarily functions that launch simulations in engine.r and update the interface.
#source("spade_ui.r")
#
## Some functions used as handlers for various UI elements
#source("handlers.r")
#
## Simulation engine
#source("engine.r")
#
## Actual GTK GUI definition
#source("gui.r")



#Start main GTK loop to stop script exiting.
#gtkMain()

# Run SPADE gui         
# TODO: Probably not everything needs to be global
SPADE = function()
{
  ################################################################################################
  # Initial species settings
  ################################################################################################
  
  species <<- 'Species 1' # names of species
  N.species <<- 1 # number of species
  
  ################################################################################################
  # Define global variables
  ################################################################################################
  
  # initialise species labels (can't do this in create.window as they need to be global
  # for e.g. change.handler function)
  # TODO: why do they need to be global?
  species.labels.left <<- vector('list', N.species) 
  species.labels.top <<- vector('list', N.species)
  # initialise buttons
  buttons <<- vector('list', N.species)
  for (i in 1:N.species) buttons[[i]] <<- vector('list', N.species)
  # initialise current season, species, culling species, number of seasons
  curr.time <<- 1
  curr.species <<- 1
  seasons <<- 1
  # set to cull during the only currently defined season
  cull.seasons <<- TRUE
  # allow windows to be opened
  window.open <<- FALSE
  # create cancel variable (determines whether we want to keep changes)
  cancel <<- FALSE
  # create matrix of droplist statuses for interactions
  status <<- matrix(1, N.species, N.species)
  # create interactions matrix as a nested list
  interactions <<- vector('list', N.species)
  # creates nested layer
  for (i in 1:N.species) interactions[[i]] <<- vector('list', N.species)
  # creates list to store carrying capacity matrix and raster
  K <<- vector('list', N.species)
  K.rast <<- vector('list', N.species)
  # make sure geometry is calculated when first species' carrying capacity is changed
  # or when model first run, but left alone otherwise
  K.changed <<- TRUE 
  # creates list to store initial condition matrix and raster
  IC <<- vector('list', N.species)
  IC.rast <<- vector('list', N.species)
  # creates list to store dispersal matrix and raster (for individual species)
  EL.species <<- vector('list', N.species)
  EL.rast <<- vector('list', N.species)



  ## Load initial parameters
  load_params()
  
  ## Load other source files
  package.dir = path.package('SPADE')
  the.file = paste(package.dir, '/extdata/formulae.csv', sep='')
  formulae <<- read.csv(the.file)
  demo.forms <<- formulae[formulae$X == 'Demographic', ]
  disease.forms <<- formulae[formulae$X == 'Disease', ]
  predprey.forms <<- formulae[formulae$X == 'Predator-prey', ]
  diffusion.forms <<- formulae[formulae$X == 'Diffusion', ]

  # global constant variables for use in optimisation
  td <<- c(0.75, 0.5, 0.25, 0.1, 0.01) # different levels of target density to try
  probs <<- c(0.01, 0.1, 0.25, 0.5, 0.75, 1) # different quantiles for spatial budget optimisation
  # TODO: make these user-editable

  timesteps <<- 1 # By default, one timestep per 'season' - i.e. discrete model

  # Define top menu list
  spade.menu <<- list()
  spade.menu$File$Exit$handler <<- function(h,...) gtkMainQuit() # menu option to quit

  # Define SPADE GUI main window
  SPADE.window <<- gwindow("SPADE", visible=F,  width=1108,height=855,handler=function(h,...){gtkMainQuit()})
  
  ################################################################################################
  # Define menus
  ################################################################################################
  
  # adds a species to the analysis, updating N.species, interactions and status - 
  # then updates graphics using create.window
  spade.menu$Species$'Add Species'$handler = function(h,...) 
  {
    # remove panes in order so we can rebuild them with the new species in the Interactions pane
    svalue(input.pad) <<- 4 # select Management pane
    dispose(input.pad) # remove it
    svalue(input.pad) <<- 3 # select Interactions pane
    dispose(input.pad) # remove it
    for (i in 1:N.species) interactions[[i]][[N.species + 1]] <<- list() # add new empty interactions boxes for new species
    
    for (i in 1:N.species) 
      if (length(interactions[[i]][[i]]$boxes)>0) 
        interactions[[i]][[i]]$boxes <<- rbind(interactions[[i]][[i]]$boxes, rep(FALSE, seasons)) # add new species to the currently existing boxes
    N.species <<- N.species + 1 # add 1 to the number of species list
    interactions[[N.species]] <<- list() # create new 'row' for the new species (necessary because of how lists work)
    for (i in 1:N.species) interactions[[N.species]][[i]] <<- list()  # within the new row, add new empty interactions boxes for new species
    
    species[N.species] <<- paste('Species', N.species) # add species to list with name based on its current number
    # add new elements to the carrying capacity, initial condition and dispersal raster info
    K[[N.species]] <<- list()
    K.rast[[N.species]] <<- list()
    IC[[N.species]] <<- list()
    IC.rast[[N.species]] <<- list()
    EL.species[[N.species]] <<- list()
    EL.rast[[N.species]] <<- list()
  
    old.status = status # make copy of current status
    status <<- matrix(1, N.species, N.species) # make new status, defaulting to blank (this will be the new species info)
    status[-N.species, -N.species] <<- old.status # set known data for original species from copy
    # create Interactions tab
    int.group <<- ggroup(container=input.pad,label="Interactions",horizontal=F)
    species.droplist[] <<- species # define droplist elements as new species list
    create.window() # populate Interactions tab
  
    # create Management tab  
    # Note: though this hasn't changed, we still need to remove/rebuild it
    # so it appears in the same place relative to Interactions
    man.group <<- ggroup(container=input.pad, label="Management", horizontal = FALSE) 
    create.man.window() # populate Management tab
    culling.droplist[] <<- species # update culling droplist with new info  
    
    svalue(input.pad) <<- 3  # open Interactions tab (where the action was started)
  }
  
  # creates a window to allow user to remove a species from the analysis
  spade.menu$Species$'Remove Species'$handler = function(h,...)
  {
    window.open <<- TRUE # ensures no other windows are opened
    remove.win <<- gwindow("Remove species") # create window
    addHandlerUnrealize(remove.win, handler = function(h,...) return(TRUE)) # disable closing (will handle via "OK" and "Cancel" buttons)  
    remove.pane <- ggroup(container = remove.win, horizontal = FALSE) # group for info
    a.label <- glabel("Species to remove:", container = remove.pane) # label
    remove.select <- gdroplist(items = species, container = remove.pane) # droplist to select species to remove
    remove.buttons <- ggroup(container = remove.pane, horizontal = TRUE) # group for OK and Cancel buttons
  
    # if OK is pressed, 
    # remove a species, updating N.species, interactions and status - then update graphics using create.window
    remove.ok <- gbutton('OK', container = remove.buttons, handler = function(h,...)
    {
      window.open <<- FALSE # allow new windows to be opened
      svalue(input.pad) <<- 4 # select Management pane
      dispose(input.pad) # remove it
      svalue(input.pad) <<- 3 # select Interactions pane
      dispose(input.pad) # remove it
      old.interactions = interactions # make copy of all interaction information
  
      # uses user-selected species value to work out which row/columns to delete
      to.remove = svalue(remove.select, index = TRUE)
      dispose(remove.win) # close Remove Species window
      new.vec = setdiff(1:N.species, to.remove) # vector containing only remaining species
      N.species <<- N.species - 1 # update number of species
      interactions <<- vector('list', N.species) # reinitialise interactions with smaller number of species
      for (i in 1:N.species) # create second dimension of list information
        interactions[[i]] <<- vector('list', N.species)
  
      for (i in 1:N.species) # populate with relevant values from previous interactions matrix
      {
        for (j in 1:N.species)
        {
          # find data from old interactions matrix corresponding to new species positions
          prev = old.interactions[[new.vec[i]]][[new.vec[j]]] 
          if (length(prev) > 0) # if there is data in that position
            interactions[[i]][[j]] <<- prev # copy the old data across
          else
            interactions[[i]][[j]] <<- list() # create an empty list (TODO: do we need this at all?)
        }          
      }
      
      for (i in 1:N.species)  # remove boxes belonging to removed species
      {
        if (!is.null(interactions[[i]][[i]]$boxes)) # if the current demographics information contains boxes
          interactions[[i]][[i]]$boxes <<- interactions[[i]][[i]]$boxes[-to.remove, ] # remove boxes
      }
  
      # remove relevant element of lists for carrying capacity, initial condition and
      # dispersal info (can do it this way due to how lists are structured)
      K[[to.remove]] <<- NULL 
      K.rast[[to.remove]] <<- NULL
      IC[[to.remove]] <<- NULL
      IC.rast[[to.remove]] <<- NULL
      EL.species[[to.remove]] <<- NULL
      EL.rast[[to.remove]] <<- NULL
      status <<- status[-to.remove, -to.remove] # remove relevant elements of status matrix
  
      species <<- species[new.vec] # remove species from list
  
      # Create Interactions tab
      int.group <<- ggroup(container=input.pad,label="Interactions",horizontal=FALSE)
      create.window() # populate Interactions tab
  
      # Create Management tab    
      man.group <<- ggroup(container=input.pad, label="Management", horizontal = FALSE) 
      create.man.window() # populate Management tab
      species.droplist[] <<- species # update species droplist with new info
      culling.droplist[] <<- species # update culling droplist with new info
      svalue(input.pad) <<- 3  # open Interactions tab (where the action was started)
    })
    # If Cancel selected, do nothing and close
    remove.cancel <- gbutton('Cancel', container = remove.buttons, handler = function(h,...) {window.open <<- FALSE; dispose(remove.win)})
  }
  
  # Loads saved interactions information from selected .Rdata file
  # TODO: incorporate parameters etc into this as well? Is there anything missing?
  spade.menu$Species$'Load Interactions'$handler = function(h,...) 
  {
    svalue(input.pad) <<- 4 # select Management pane
    dispose(input.pad) # remove it
    svalue(input.pad) <<- 3 # select Interactions pane
    dispose(input.pad) # remove it
  
    file = fileChoose(type = 'open') # user selects interactions file
    if (!is.na(file)) # if valid file selected
    {
      load(file, envir = .GlobalEnv) # load it into R (TODO: error handling?)
      curr.time <<- seasons # set current season to last season
    }
  
    # Create Interactions tab
    int.group <<- ggroup(container=input.pad,label="Interactions",horizontal=FALSE)
    create.window() # populate Interactions tab
    man.group <<- ggroup(container=input.pad, label="Management", horizontal = FALSE) 
  
    # Create Management tab    
    man.group <<- ggroup(container=input.pad, label="Management", horizontal = FALSE) 
    create.man.window() # populate Management tab
    species.droplist[] <<- species # update species droplist with new info
    culling.droplist[] <<- species # update culling droplist with new info

    svalue(input.pad) <<- 1 # open the Rasters tab
    svalue(raster.pad)=1  # open Dispersal sub-tab
    plotdata(EL.all) # plot data
    svalue(raster.pad)=2 # open Fenced area sub-tab
    plotdata(fenced)  # plot data    
    
    svalue(input.pad) <<- 3  # open Interactions tab
  }
  
  # Saves interactions information to .Rdata file
  # TODO: incorporate parameters etc into this as well? Is there anything missing?
  spade.menu$Species$'Save Interactions'$handler = function(h,...) 
  {
    # TODO: set so that default extension is .Rdata?
    file = fileChoose(type = 'save') # user selects filename to save to
    if (!is.na(file)) # if valid filename selected
    {
      # save relevant variables to an .Rdata file
      save(interactions, K, K.rast, IC, IC.rast, EL.species, EL.rast, N.species, species, status, seasons, cull.seasons, EL.all, fenced, file = file, envir = .GlobalEnv)
    }
  }

  # add the previously defined menu to the window
  add.menu <<- gmenu(spade.menu,container=SPADE.window)  
  
  # create a scroll window so that the window can be resized for different screens
  scroll.group <<- ggroup(container=SPADE.window,use.scrollwindow=T)
  # separate the window into two panes, starting with the left pane
  left.pane <<- ggroup(container=scroll.group,horizontal=F)
  # create a notebook consisting of multiple panes
  input.pad <<- gnotebook(container=left.pane,expand=T)
  # create the Rasters pane
  file.group <<- ggroup(container=input.pad,label="Rasters",horizontal=F)
  # button for setting the Dispersal and Fenced area rasters to their defaults
  # (using default.rasters)
  def.button <<- gbutton("Generate default rasters using Carrying Capacity info", container=file.group,
                      handler=default.rasters)
  glabel("",container=file.group) # create label solely for spacing purposes
  # Button to load Dispersal raster
  EL.all.button <<- gbutton("Load Dispersal Raster", container=file.group,
    handler=function(h,...){
      # load user-specified raster information into allthedata
      allthedata = raster.load.button.handler()
      if (!is.null(allthedata)) # if it worked
      {
        # set global variables to necessary data
        EL.all.rast <<- allthedata$the.rast 
        EL.all <<- allthedata$the.data
        svalue(EL.all.label)=allthedata$the.file # set label
        visible(EL.all.plot.raster)=T # select plot window
        svalue(raster.pad)=1
        plotdata(EL.all) # plot data
      }
    })
  EL.all.label <<- glabel("",container=file.group) # label showing loaded file name
        
  glabel("",container=file.group) # spacing
  # Button to load Fencing raster
  fenced.button <<- gbutton("Load Fence Raster", container=file.group,
    handler=function(h,...){
      # load user-specified raster information into allthedata
      allthedata = raster.load.button.handler()
      if (!is.null(allthedata))
      {
        # set global variables to necessary data    
        fenced.rast <<- allthedata$the.rast 
        fenced <<- allthedata$the.data
        svalue(fenced.label)=allthedata$the.file # set label
        visible(fenced.plot.raster)=T # select plot window
        svalue(raster.pad)=2
        plotdata(fenced) # plot data
      }
    })
  fenced.label <<- glabel("",container=file.group) # label showing loaded file name
                         
  raster.pad <<- gnotebook(container=file.group,expand=T) # create notebook with a tab for each raster
  
  EL.all.plot.group <<- ggroup(container=raster.pad,label="Dispersal",horizontal=F) # create Dispersal tab
  EL.all.plot.raster <<- ggraphics(container=EL.all.plot.group) # create graphics element for plotting
  EL.all.plot.buttons <<- ggroup(container = EL.all.plot.group, horizontal = TRUE) # create space for buttons
  EL.all.plot.edit <<- gbutton("Edit",container=EL.all.plot.buttons, # create Edit button
    handler=function(h,...){
      EL.all <<- edit(EL.all) # use R's inbuilt editor to edit the raster
      visible(EL.all.plot.raster)=T # select plot window
      svalue(raster.pad)=1
      EL.all.rast=raster(EL.all) # create raster from changed matrix (TODO: use K[[1]] as a template so we keep projection info)
      plotdata(EL.all) # plot data
    })
  EL.all.plot.save <<- gbutton("Save",container=EL.all.plot.buttons, # create Save button
    handler=function(h,...){
      filename=fileSaveChoose("print") # user selects a filename to save to
      if(!is.na(filename)){ # if valid filename selected
        EL.all.rast=raster(EL.all) # create raster from current matrix (TODO: use K[[1]] as a template so we keep projection info)
        writeRaster(EL.all.rast,filename,format="GTiff") # write in GTiff format to filename
      }
    })
  
  fenced.plot.group <<- ggroup(container=raster.pad,label="Fenced area",horizontal=F) # create Fenced Area tab
  fenced.plot.raster <<- ggraphics(container=fenced.plot.group)  # create graphics element for plotting
  fenced.plot.buttons <<- ggroup(container = fenced.plot.group, horizontal = TRUE)  # create space for buttons
  fenced.plot.edit <<- gbutton("Edit",container=fenced.plot.buttons,  # create Edit button
    handler=function(h,...){
      fenced <<- edit(fenced) # use R's inbuilt editor to edit the raster
      visible(fenced.plot.raster)=T # select plot window
      svalue(raster.pad)=2
      fenced.rast=raster(fenced)  # create raster from changed matrix (TODO: use K[[1]] as a template so we keep projection info)
      plotdata(fenced) # plot data
    })
  fenced.plot.save <<- gbutton("Save",container=fenced.plot.buttons, # create Save button
    handler=function(h,...){
      filename=fileSaveChoose("print") # user selects a filename to save to
      if(!is.na(filename)){ # if valid filename selected
        fenced.rast=raster(fenced)  # create raster from current matrix (TODO: use K[[1]] as a template so we keep projection info)
        writeRaster(fenced.rast,filename,format="GTiff") # write in GTiff format to filename
      }
    })                                   
  
  
  # Create the Parameters pane
  var.group <<- ggroup(container=input.pad,label="Parameters",horizontal=F)
  
  # matrix setting
  sparse <<- TRUE
  matrix.group <<- ggroup(container = var.group) # create space for label and radio buttons
  matrix.label = glabel("Matrix type:", container = matrix.group)
  addSpring(matrix.group)
  matrix.method <<- gradio(c("Sparse", "Dense"), container = matrix.group, 
    handler = function(h,...){ # handler for when method is changed
      a = svalue(h$obj, index = TRUE) # extract newly selected method
      sparse <<- (a == 1) # decide whether using sparse matrices or not
  })
  
  # Duration (years)
  duration.group<<-ggroup(container=var.group) # create space for label and edit box
  duration.label<<-glabel("Duration (years)",container=duration.group) # label
  addSpring(duration.group) # make sure edit box is aligned to the right
  duration.slider<<-gedit(text=duration,container=duration.group) # edit box
  
  # Number of "seasons" in a year
  time.pane <<- ggroup(container = var.group, horizontal = TRUE) # create space for label and edit box
  time.label <<- glabel('Number of "seasons" in a year', container=time.pane) # label
  addSpring(time.pane) # make sure edit box is aligned to the right
  time.slider <<- gspinbutton(from=1,to=100,by=1,value=curr.time,container=time.pane,
    handler=function(h,...) curr.time <<-svalue(h$obj)) # spin button (can select from 1 to 100)
  time.button <<- gbutton('Apply', container = time.pane, 
    handler = time.button.handler) # Apply button
  
  # Integration timesteps per season
  timesteps.group<<-ggroup(container=var.group) # create space for label and edit box
  timesteps.label<<-glabel("Integration timesteps per season",container=timesteps.group) # label
  addSpring(timesteps.group) # make sure edit box is aligned to the right
  timesteps.slider<<-gspinbutton(from=1,to=1000000,by=1,value=timesteps,container=timesteps.group,
    handler=function(h,...) timesteps <<-svalue(timesteps.slider)) # spin button (can select from 1 to 1,000,000)
  
  # Maximum available budget (used for optimisation)
  budget.group<<-ggroup(container=var.group) # create space for label and edit box
  budget.label<<-glabel("Maximum available budget (used for optimisation)",container=budget.group) # label
  addSpring(budget.group) # make sure edit box is aligned to the right
  budget.slider<<-gedit(text=budget,container=budget.group) # edit box
  
  # Cell size (km^2)
  cell.size.group<<-ggroup(container=var.group) # create space for label and edit box
  cell.size.label<<-glabel("Cell size (km^2)",container=cell.size.group)  # label
  addSpring(cell.size.group) # make sure edit box is aligned to the right
  cell.size.slider<<-gedit(text=cell.size,container=cell.size.group) # edit box
  
  fence.failure.group<<-ggroup(container=var.group) # create space for label and edit box
  fence.failure.label<<-glabel("Fence failure rate",container=fence.failure.group) # label
  addSpring(fence.failure.group) # make sure edit box is aligned to the right
  fence.failure.slider<<-gspinbutton(from=0,to=0.999,by=.001,value=fence.failure,container=fence.failure.group,
    handler=function(h,...) fence.failure <<-svalue(fence.failure.slider)) # spin button (can choose from 0 to 0.999)
  
  fence.build.cost.group<<-ggroup(container=var.group) # create space for label and edit box
  fence.build.cost.label<<-glabel("Fence build cost (per km^2)",container=fence.build.cost.group) # label
  addSpring(fence.build.cost.group) # make sure edit box is aligned to the right
  fence.build.cost.slider<<-gedit(text=fence.build.cost,container=fence.build.cost.group) # edit box
  
  fence.maintain.cost.group<<-ggroup(container=var.group) # create space for label and edit box
  fence.maintain.cost.label<<-glabel("Fence maintenance cost (per km^2)",container=fence.maintain.cost.group) # label
  addSpring(fence.maintain.cost.group) # make sure edit box is aligned to the right
  fence.maintain.cost.slider<<-gedit(text=fence.maintain.cost,container=fence.maintain.cost.group)# edit box
  
  Load.Params.button <<- gbutton("Load Parameter File", container=var.group, # button to load a parameter file from csv
    handler=function(h,...){
      param.file=fileChoose("print") # user selects file
      if(! is.na(param.file)) load_params(param.file,new=T) # if a valid file, loads it using load_params function (TODO: update now that I've changed billions of things)
    })
  
  Save.Params.button <<- gbutton("Save Parameter File", container=var.group,
    handler=function(h,...){
      param.file=fileSaveChoose("print") # user selects file
      if(! is.na(param.file)) save_params(param.file) # if a valid file, saves it using save_params function
    })
  
  
  #Create the Interactions pane
  int.group <<- ggroup(container=input.pad,label="Interactions",horizontal=F)
  create.window() # populate the pane using create.window() from interactions.r file
        
  # Create the Management pane
  man.group <<- ggroup(container=input.pad, label="Management", horizontal = FALSE)    
  
  # initialise strategy information 
  N.strategies <<- 0 # number of strategies currently defined
  strategy.no <<- 0 # current number for naming strategies
  curr.strategy <<- NULL # current strategy chosen for display
  strategy.params <<- list( # information for each strategy
    cull.species = NULL, cull.seasons = NULL, area.target = NULL, culling.choice = NULL, init.cull = NULL, 
    maint.cull = NULL, init.cull.abs = NULL, maint.cull.abs = NULL, target.density = NULL, 
    cost.int = NULL, cost.slope = NULL, helicopter.cost = NULL, EL = list(NULL), PR = list(NULL), C.mask = list(NULL),
    EL.rast = list(NULL), PR.rast = list(NULL), C.rast = list(NULL), TD = list(NULL), CS = list(NULL), CI = list(NULL), CH = list(NULL),
    CS.rast = list(NULL), CI.rast = list(NULL), CH.rast = list(NULL), TD.rast = list(NULL)
  )
  
  # now create management window as defined above
  create.man.window()
  
  # Add buttons at bottom of left pane to run various models
  
  run.group <<- ggroup(container=left.pane) # create group to contain buttons
  
  run.model.button <<- gbutton("Run Model",container=run.group, # Run Model button
    handler=function(h,...) run.model() ) # with handler that runs the run.model routine in spade_ui.r
                       
  run.NSB.button <<- gbutton("Run Non-Spatial Budget",container=run.group, # Run Non-Spatial Budget button
    handler=function(h,...) run.NSB()) # with handler that runs the run.NSB routine in spade_ui.r
                        
  run.NSD.button <<- gbutton("Run Non-Spatial Density",container=run.group, # Run Non-Spatial Density button
    handler=function(h,...) run.NSD()) # with handler that runs the run.NSD routine in spade_ui.r
                      
  run.SB.button <<- gbutton("Run Spatial Budget",container=run.group, # Run Spatial Budget button
    handler=function(h,...) run.SB()) # with handler that runs the run.SB routine in spade_ui.r
  
  
  # Create right pane, contains SPADE output
  right.pane <<- ggroup(container=scroll.group,horizontal=F, expand = TRUE)
  
  # Create notebook containing tabs for various outputs
  output.pad <<- gnotebook(container=right.pane,expand=TRUE)
  
  pop.plot.page <<- ggraphics(container=output.pad,label="Population-Time Plot") # graphics for population vs time plot
  cost.plot.page <<- ggraphics(container=output.pad,label="Cost-Time Plot") # graphics for cost vs time plot
  pn0.plot.page <<- ggraphics(container=output.pad,label="Proportional Population") # graphics for change in managed population
  pop.ts.group <<- ggroup(container=output.pad,label="Pop TS",horizontal=F) # group for graphics and buttons
  pop.ts.page <<- ggraphics(container=pop.ts.group,label="Population TS") # graphics for population raster time series
  anim.but.group <<- glayout(container=pop.ts.group) # sub-group to contain buttons
  
  anim.but.group[1,1] = (prev.but <<- gbutton("Previous",container=anim.but.group, # create Previous button
    handler=function(h,...){ # with handler for when button pressed
      curr.t <<- max(1, curr.t - 1) # set time back by 1 (unless it's already at the first frame)
      svalue(time.select, index=TRUE) = curr.t # update slider
#      draw.pop.ts(curr.t) # draw using draw.pop.ts routine in spade_ui.r (don't need, covered by slider)
    }))
  
  anim.but.group[1,2] = (next.but <<- gbutton("Next",container=anim.but.group, # create Next button
    handler=function(h,...){ # with handler for when button pressed
      curr.t <<- min(seasons * duration + 1, curr.t + 1) # set time forward by 1 (unless it's already at the last frame)
      svalue(time.select, index=TRUE) = curr.t # update slider
#      draw.pop.ts(curr.t) # draw using draw.pop.ts routine in spade_ui.r (don't need, covered by slider)
    }))
  
  anim.but.group[1,3] = (species.droplist <<- gdroplist(items = species, selected = 1, container=anim.but.group, # create droplist to select species to draw raster for
    handler = function(h,...){ # with handler for when item selected
      curr.species <<- svalue(h$obj, index = TRUE);  # change curr.species based on user decision
      if (is.null(curr.species)) curr.species <<- 1 # if nothing selected, default curr.species to 1
      env = ls('.GlobalEnv'); # define the global environment for R variables
      # if 'out' is defined in the global environment (which means that a model has been run)
      # then it's safe to draw the species at the current time using draw.pop.ts from spade_ui.r
      if ("out" %in% env) draw.pop.ts(curr.t) 
    }))

  anim.but.group[2,1:3, expand = TRUE] = (time.select <<- gslider(from = 0, to = duration, by = 1/seasons, horizontal = TRUE, container = anim.but.group, # create slider to choose time
    handler = function(h,...){ # with handler for when slider changed
      curr.t <<- svalue(time.select, index = TRUE) # set timestep to whatever slider is set to
#      curr.t <<- round(seasons*svalue(time.select)) + 1 
      draw.pop.ts(curr.t) # draw using draw.pop.ts routine in spade_ui.r
    }))
  
  CB.page <<- ggraphics(container=output.pad,label="CB Map") # graphics for cost benefit map
  OCM.page <<- ggraphics(container=output.pad,label="OCM Map") # graphics for optimal culling mask map
  
  scroll2.group <<- ggroup(container=right.pane,use.scrollwindow=T) # add a scroll pane for the right pane
  text.1 <<- gtext("",container=scroll2.group,expand=TRUE,height = 200,font.attr=c(family="monospace")) # text box for sim results
  

  svalue(raster.pad)=1 # start with first raster selected on the Rasters pane (Dispersal)
  svalue(input.pad)=1 # start with the Raster tab selected on the left pane
  svalue(output.pad)=1 # start with the Population-Time plot selected on the right pane
  visible(SPADE.window,T) # make window visible
  
  # Draw logo
  svalue(output.pad) = 1  
  visible(pop.plot.page)=TRUE
  the.file = paste(package.dir, '/extdata/spadelogo.png', sep='')  
  logo = raster(the.file)
  image(logo>0, col=c('black', 'white'), axes = FALSE, asp = 1, xlab='', ylab='')

}