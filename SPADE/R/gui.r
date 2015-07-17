# create a dialog box to select a file

newProgressBar = function(text = 'Progress bar')
{
  # adapted from http://metabolomics-forum.com/viewtopic.php?f=9&t=411

  pwindow <<- gwindow(container = TRUE, width = 400, height = 50, title = sprintf("%s (0%% done)", text))
  bp <<- gtkProgressBarNew()
  bp$'width-request' = 400
  bp$'height-request' = 50
  add(pwindow, bp)
}

updateProgressBar = function(p, text = 'Progress bar')
{
  bp$setFraction(p)
  svalue(pwindow) = sprintf("%s (%.1f%% done)", text, p*100)
}
  
fileChoose <- function(action="print", text = "Select a file...", type="open", ...) 
{
  gfile(text=text, type=type, ..., action = action, # create dialog box
    handler =function(h,...) { # with handler for when action performed
      if(h$action != "") # if the action has not been set to blank
      {
        print(h$action) # print the action name to R
        setwd(dirname(h$file)) # set directory of file to new working directory
        do.call(h$action, list(h$file)) # perform the action on the file (usually print)
      }
    })
}

# File save dialog
fileSaveChoose <- function(action="print", text = "Select a file...", type="save", ...) {
  gfile(text=text, type=type, ..., action = action, # create dialog box
    handler =function(h,...) { # with handler for when action performed
      if(h$action != "")
      {
        print(h$action) # print the action name to R
        setwd(dirname(h$file)) # set directory of file to new working directory
        do.call(h$action, list(h$file)) # perform the action on the file (usually print)
      }
    })
}

## Simple confirmation dialog
confirmDialog <- function(message, handler=NULL) {
   window <- gwindow("Confirm")
   group <- ggroup(container = window)
   gimage("info", dirname="stock", size="dialog", container=group)
  
   ## A group for the message and buttons
   inner.group <- ggroup(horizontal=FALSE, container = group)
   glabel(message, container=inner.group, expand=TRUE)
  
   ## A group to organize the buttons
   button.group <- ggroup(container = inner.group)
   ## Push buttons to right
   addSpring(button.group)
   gbutton("OK", handler = function(h,...) dispose(window), container=button.group)
   return()
}

# routine to load a user-selected raster file,
# outputs a list of the necessary outputs
# the.file = name of loaded file
# the.rast = data in raster form
# the.data = data in matrix form
raster.load.button.handler = function()
{
  the.file=fileChoose("print") # select file using dialog
  if (!is.na(the.file)) # if a file has been selected
  {
    ext = extension(the.file) # work out its extension
    if (ext == '.csv')  # if it is a csv file, use read.csv (use try to catch errors)
      DATA = try(read.csv(the.file, header = FALSE), silent = TRUE) 
    else   # otherwise, use raster (should work on all standard raster formats)
      DATA = try(raster(the.file), silent = TRUE)
    try.error = inherits(DATA, 'try-error')
    if (try.error) # if an error has been caught, complain in a dialog box (TODO: need further action?)
      confirmDialog("File not compatible! Please select file in either .csv or raster format")
    else # otherwise, convert data for output
    {
      if (ext == '.csv') 
        the.rast = raster(as.matrix(DATA)) # make a raster out of the csv file
      else
        # read raster into memory (note: this is necessary as DATA itself 
        # doesn't actually contain the raster data - only a reference
        # to the filename of the loaded raster)
        the.rast = readAll(DATA) 
      the.data = raster::as.matrix(the.rast) # TODO: default version didn't work when SPADE made package for some reason...
    }
    # return list of required outputs
    list(the.file = the.file, the.rast = the.rast,  the.data = the.data)
  }
  else # if no valid file has been selected, return NULL
    NULL
}

# set global (i.e. terrain-based) dispersal and fencing information to defaults
# and plot
default.rasters = function(h,...)
{
  svalue(input.pad) <<- 1 # choose the Rasters tab
  # set EL.rast (raster information) to 1 everywhere
  # - using the first species' carrying capacity as a template
  EL.all.rast <<- K.rast[[1]]*0 + 1 
  EL.all <<- raster::as.matrix(EL.all.rast) # set matrix version
  svalue(EL.all.label)='Default (1 for all cells)' # show default on label
  visible(EL.all.plot.raster)=TRUE  # select the plot device
  svalue(raster.pad)=1                    
  plotdata(EL.all) # plot data
  
  # set fenced.rast (raster information) to 1 everywhere
  # - using the first species' carrying capacity as a template  
  fenced.rast <<- K.rast[[1]]*0
  fenced <<- raster::as.matrix(fenced.rast) # set matrix version
  svalue(fenced.label)='Default (0 for all cells)' # show default on label
  visible(fenced.plot.raster)=TRUE # select the plot device
  svalue(raster.pad)=2
  plotdata(fenced)  # plot data
}

# routine to update the strategy.params list based on the current values of the management text boxes
# (the spin buttons update the variables automatically)
update.strategy.sliders = function()
{
  if (!is.null(curr.strategy)) # only if has already been defined 
  { 
    if (curr.strategy <= N.strategies) # and is an existent option (added because of remove strategy)
    {
      # stored param values into strategy.params from numbers currently on gtext objects (using previous droplist place)
      strategy.params$init.cull.abs[curr.strategy] <<- as.double(svalue(init.cull.abs.slider))
      strategy.params$maint.cull.abs[curr.strategy] <<- as.double(svalue(maint.cull.abs.slider))        
      strategy.params$target.density[curr.strategy] <<- as.double(svalue(target.density.slider))
      strategy.params$cb.a[curr.strategy] <<- as.double(svalue(cb.a.slider))
      strategy.params$cb.b[curr.strategy] <<- as.double(svalue(cb.b.slider))
      strategy.params$cb.c[curr.strategy] <<- as.double(svalue(cb.c.slider))                           
  }}
}

# function to generate Management window
create.man.window = function()
{
  strategies.group = ggroup(container=man.group) # group for Strategies selector
  a.label = glabel('Strategies:', container = strategies.group) # label
  addSpring(strategies.group)# make sure droplist is on right
  strategies.droplist <<- gdroplist(items = 'None', container = strategies.group, handler = sdh) # droplist to select current strategy

  sar.group = ggroup(container = man.group, horizontal = TRUE) # strategies add/remove buttons group

  # Add strategy button with handler for when button is pressed
  add.strategy = gbutton("Add strategy", container = sar.group, 
    handler = add.strategy.handler) # handler in handlers.r

  # Remove strategy button with handler for when button is pressed    
  rem.strategy = gbutton("Remove strategy", container = sar.group, 
    handler = rem.strategy.handler) # handler in handlers.r
  
  # define group to contain all the parameters for each strategy
  strategy.params.group <<- ggroup(container = man.group, horizontal = FALSE)
  # set visibility depending on whether there are any strategies defined
  visible(strategy.params.group) = (N.strategies > 0) 
  
  culling.group=ggroup(container=strategy.params.group) # create space for label and droplist
  a.label=glabel("Species to manage:",container=culling.group) # label
  addSpring(culling.group) # make sure droplist is aligned to the right
  culling.droplist <<- gdroplist(items = species, container=culling.group, selected = 1, # droplist for selecting species to manage with handler
    handler=function(h,...) # handles when option is changed
    {
      if (length(svalue(h$obj, index=TRUE))>0) # if it has a value
        strategy.params$cull.species[curr.strategy] <<- svalue(h$obj, index=TRUE) # set relevant cull.species option to current selected species
    }
  )
                        
  seasons.group = ggroup(container = strategy.params.group) # create space for label and checkbox(es)
  a.label = glabel('Seasons to cull:', container = seasons.group) # label       
  seasons.boxes <<- vector('list', seasons) # create list of checkbox objects (one checkboxbox for each season)
  for (i in 1:seasons) # for each season, make a checkbox
  {
    # create checkbox with handler for when checkbox is checked/unchecked
    seasons.boxes[[i]] <<- gcheckbox(i, checked = strategy.params$cull.seasons[curr.strategy, i], container = seasons.group,  
      handler = function(h,...) strategy.params$cull.seasons[curr.strategy, id(h$obj)] <<- svalue(h$obj)) # handler sets relevant cull.seasons option depending on checked status
    id(seasons.boxes[[i]]) <<- i # sets the id tag for the checkbox so that above handler works correctly
  }                              
  
  area.target.group=ggroup(container=strategy.params.group) # create space for label and spinbutton
  area.target.label=glabel("Target proportional population in region (used for optimisation)",container=area.target.group) # label
  addSpring(area.target.group) # make sure spinbutton is aligned to the right
  # slider with handler to set value in strategy.params to spinbutton value when changed
  area.target.slider <<- gspinbutton(from=0.01,to=1,by=.01,value=strategy.params$area.target[curr.strategy],container=area.target.group,
    handler=function(h,...) strategy.params$area.target[curr.strategy] <<-svalue(area.target.slider))
                    
  # Create a radio group to select the method of culling
  culling.frame = gframe("Removal method:", container = strategy.params.group, horizontal = FALSE) # label/frame
  culling.method <<- gradio(c("Proportional", "Absolute", "Capped proportional"), container = culling.frame, 
    handler = function(h,...){ # handler for when method is changed
      a = svalue(h$obj, index = TRUE) # extract newly selected method
      strategy.params$culling.choice[curr.strategy] <<- a # add information to strategy.params
      visible(init.cull.group) = (a == 1 | a == 3)    # visible if "Proportional" is selected
      visible(maint.cull.group) = (a == 1 | a == 3)
      visible(init.cull.abs.group) = (a == 2 | a == 3) # visible if "Absolute" is selected
      visible(maint.cull.abs.group) = (a == 2 | a == 3)                        
  })
  
  init.cull.group<<-ggroup(container=culling.frame) # create space for label and spinbutton
  init.cull.label=glabel("Removal rate in first year",container=init.cull.group) # label
  addSpring(init.cull.group) # make sure spinbutton is aligned to the right
  # slider with handler to set value in strategy.params to spinbutton value when changed
  init.cull.slider <<- gspinbutton(from=0,to=0.99,by=.01,value=strategy.params$init.cull[curr.strategy],container=init.cull.group,
    handler=function(h,...) strategy.params$init.cull[curr.strategy] <<-svalue(init.cull.slider))
    
  maint.cull.group<<-ggroup(container=culling.frame) # create space for label and spinbutton
  maint.cull.label=glabel("Removal rate after first year",container=maint.cull.group) # label
  addSpring(maint.cull.group) # make sure spinbutton is aligned to the right
  # slider with handler to set value in strategy.params to spinbutton value when changed
  maint.cull.slider <<- gspinbutton(from=0,to=0.99,by=.01,value=strategy.params$maint.cull[curr.strategy],container=maint.cull.group,
    handler=function(h,...) strategy.params$maint.cull[curr.strategy] <<-svalue(maint.cull.slider))
  
  init.cull.abs.group<<-ggroup(container=culling.frame) # create space for label and edit box
  init.cull.abs.label=glabel("Total removal in first year",container=init.cull.abs.group) # label
  addSpring(init.cull.abs.group) # make sure edit box is aligned to the right
  init.cull.abs.slider<<-gedit(text=strategy.params$init.cull.abs[curr.strategy],container=init.cull.abs.group) # edit box
  visible(init.cull.abs.group) = FALSE # set to originally be invisible (as Proportional mode is selected by default)
  
  maint.cull.abs.group<<-ggroup(container=culling.frame) # create space for label and edit box
  maint.cull.abs.label=glabel("Total removal after first year",container=maint.cull.abs.group)# label
  addSpring(maint.cull.abs.group) # make sure edit box is aligned to the right
  maint.cull.abs.slider<<-gedit(text=strategy.params$maint.cull.abs[curr.strategy],container=maint.cull.abs.group) # edit box
  visible(maint.cull.abs.group) = FALSE # set to originally be invisible
  
  # create group for spatially implicit definitions
  # so that we can set its visibility depending on whether we're using rasters or not
  # (they start off as visible)
  spatially.implicit.group1 <<- ggroup(container = culling.frame, horizontal = FALSE) 
  
  target.density.group=ggroup(container=spatially.implicit.group1) # create space for label and edit box
  target.density.label=glabel("Species density (per km^2) at which removal stops in a cell",container=target.density.group) # label
  addSpring(target.density.group) # make sure edit box is aligned to the right
  target.density.slider<<-gedit(text=strategy.params$target.density[curr.strategy],container=target.density.group) # edit box

  # create groups for spatially explicit definitions
  # so that we can set their visibility depending on whether we're using rasters or not
  # (they start off as invisible = value of use.cost.rasters)

  # first group for buttons (stacked horizontally to save space)
  cost.raster.group1 <<- ggroup(container=culling.frame, horizontal = TRUE)

  # buttons to load raster from file (spatially explicit)
  TD.button <- gbutton("Load Target Density Raster", container=cost.raster.group1,
    handler=function(h,...){ # handler for when button pressed
      allthedata = raster.load.button.handler() # function to load user-selected data
      if (!is.null(allthedata)) # if data loaded
      {
        # store data in strategy.params
        strategy.params$TD.rast[[curr.strategy]] <<- allthedata$the.rast 
        strategy.params$TD[[curr.strategy]] <<- allthedata$the.data
        visible(TD.plot.raster)=TRUE # select plot window
        svalue(strats.raster.pad)=3
        plotdata(strategy.params$TD[[curr.strategy]]) # plot raster                          
      }
    })
   
  # Create a radio group to select the cost per animal method
  cb.frame = gframe("Cost per animal method:", container = strategy.params.group, horizontal = FALSE) # label/frame
  cb.method <<- gradio(c("Power", "Exponential"), container = cb.frame, 
    handler = function(h,...){ # handler for when method is changed
      a = svalue(h$obj, index = TRUE) # extract newly selected method
      strategy.params$cb.choice[curr.strategy] <<- a # add information to strategy.params
      if (a == 1)
      {
        svalue(cb.a.label) = "Intercept"
        svalue(cb.b.label) = "Slope"
        svalue(cb.c.label) = "Hourly cost"      
        svalue(CB.A.button) = "Load Intercept Raster"
        svalue(CB.B.button) = "Load Slope Raster"
        svalue(CB.C.button) = "Load Hourly Cost Raster"  
      }
      else
      {
        svalue(cb.a.label) = "Intercept"
        svalue(cb.b.label) = "Coefficient"
        svalue(cb.c.label) = "Exponential rate"  
        svalue(CB.A.button) = "Load Intercept Raster"
        svalue(CB.B.button) = "Load Coefficient Raster"
        svalue(CB.C.button) = "Load Exponential Rate Raster" 
      }
  })   

  # create group for spatially implicit definitions
  # so that we can set its visibility depending on whether we're using rasters or not
  # (they start off as visible)
  spatially.implicit.group2 <<- ggroup(container = cb.frame, horizontal = FALSE) 
   
  cb.a.group=ggroup(container=spatially.implicit.group2) # create space for label and edit box       
  cb.a.label<<-glabel("Intercept",container=cb.a.group) # label
  addSpring(cb.a.group) # make sure edit box is aligned to the right
  cb.a.slider<<-gedit(text=strategy.params$cb.a[curr.strategy],container=cb.a.group) # edit box
  
  cb.b.group=ggroup(container=spatially.implicit.group2) # create space for label and edit box
  cb.b.label<<-glabel("Slope",container=cb.b.group) # label
  addSpring(cb.b.group) # make sure edit box is aligned to the right
  cb.b.slider<<-gedit(text=strategy.params$cb.b[curr.strategy],container=cb.b.group) # edit box
  
  cb.c.group=ggroup(container=spatially.implicit.group2) # create space for label and edit box
  cb.c.label<<-glabel("Hourly cost",container=cb.c.group) # label
  addSpring(cb.c.group) # make sure edit box is aligned to the right
  cb.c.slider<<-gedit(text=strategy.params$cb.c[curr.strategy],container=cb.c.group)  # edit box
    
 
  # buttons to load rasters from file

  # second group for buttons (stacked horizontally to save space)

  cost.raster.group2 <<- ggroup(container=cb.frame, horizontal = TRUE)
    
  CB.A.button <<- gbutton("Load Cost Intercept Raster", container=cost.raster.group2,
    handler=function(h,...){ # handler for when button pressed
      allthedata = raster.load.button.handler() # function to load user-selected data
      if (!is.null(allthedata)) # if data loaded
      {
        # store data in strategy.params
        strategy.params$CB.B.rast[[curr.strategy]] <<- allthedata$the.rast 
        strategy.params$CB.B[[curr.strategy]] <<- allthedata$the.data
        visible(CB.B.plot.raster)=TRUE  # select plot window
        svalue(strats.raster.pad)=4
        plotdata(strategy.params$CB.B[[curr.strategy]]) # plot raster                          
      }
    })
  
  CB.B.button <<- gbutton("Load Cost Slope Raster", container=cost.raster.group2,
    handler=function(h,...){ # handler for when button pressed
      allthedata = raster.load.button.handler() # function to load user-selected data
      if (!is.null(allthedata)) # if data loaded
      {
        # store data in strategy.params
        strategy.params$CB.A.rast[[curr.strategy]] <<- allthedata$the.rast 
        strategy.params$CB.A[[curr.strategy]] <<- allthedata$the.data
        visible(CB.A.plot.raster)=TRUE # select plot window
        svalue(strats.raster.pad)=5
        plotdata(strategy.params$CB.A[[curr.strategy]]) # plot raster
      }
    })
       
  CB.C.button <<- gbutton("Load Hourly Cost Raster", container=cost.raster.group2,
    handler=function(h,...){ # handler for when button pressed
      allthedata = raster.load.button.handler() # function to load user-selected data
      if (!is.null(allthedata)) # if data loaded
      {
        # store data in strategy.params
        strategy.params$CB.C.rast[[curr.strategy]] <<- allthedata$the.rast 
        strategy.params$CB.C[[curr.strategy]] <<- allthedata$the.data
        visible(CB.C.plot.raster)=TRUE # select plot window
        svalue(strats.raster.pad)=6
        plotdata(strategy.params$CB.C[[curr.strategy]]) # plot raster                          
      }
    })                      

  cost.raster.label.group = ggroup(container = strategy.params.group, horizontal = TRUE) # create space for label and checkbox
  a.label = glabel('Spatially explicit cost and target density:', container = cost.raster.label.group)    
  # set a variable which defines whether we're using rasters for management or not (initialise to false)
  use.cost.rasters <<- FALSE
  # create a checkbox that defines whether the strategy uses rasters or spatially homogeneous values
  cost.raster.check <<- gcheckbox('', checked = use.cost.rasters, container = cost.raster.label.group, 
    handler = cost.raster.check.handler) # handler in handlers.r
  
  # set visibility of raster group (initially false)
  visible(cost.raster.group1) = use.cost.rasters                       
  visible(cost.raster.group2) = use.cost.rasters     

  # buttons to load Priority and Culling Mask rasters from file      
  # (always visible as opposed to above four buttons)

  # label to show filename of loaded raster
  # TODO: do we need this? if so, should we have it for the above four rasters too?
  PR.label <<- glabel("",container=strategy.params.group) 
  PR.button <- gbutton("Load Priority Raster", container=strategy.params.group,
    handler=function(h,...){ # handler for when button pressed
      allthedata = raster.load.button.handler() # function to load user-selected data
      if (!is.null(allthedata))  # if data loaded
      {
        # store data in strategy.params
        strategy.params$PR.rast[[curr.strategy]] <<- allthedata$the.rast 
        strategy.params$PR[[curr.strategy]] <<- allthedata$the.data
        svalue(PR.label)=allthedata$the.file # set label to filename
        visible(PR.plot.raster)=TRUE # select plot window
        svalue(strats.raster.pad)=1
        plotdata(strategy.params$PR[[curr.strategy]]) # plot raster
      }
    })
  
  C.label <<- glabel("",container=strategy.params.group) # label to show filename of loaded raster
  C.button <- gbutton("Load Culling Mask Raster", container=strategy.params.group,
    handler=function(h,...){ # handler for when button pressed
      allthedata = raster.load.button.handler() # function to load user-selected data
      if (!is.null(allthedata))  # if data loaded
      {
        # store data in strategy.params
        strategy.params$C.rast[[curr.strategy]] <<- allthedata$the.rast 
        strategy.params$C.mask[[curr.strategy]] <<- allthedata$the.data
        svalue(C.label)=allthedata$the.file # set label to filename
        visible(C.plot.raster)=TRUE  # select plot window
        svalue(strats.raster.pad)=2
        plotdata(strategy.params$C.mask[[curr.strategy]]) # plot raster
      }
    })                        
      
  # create notebook to display raster plots
  strats.raster.pad <<- gnotebook(container=strategy.params.group,expand=TRUE)
  
  # TODO: any way to limit repetition in the raster plots below? 
  # Identical process, different only in that performed on different variables
  
  # create a tab in the notebook for the Priority raster
  PR.plot.group <- ggroup(container=strats.raster.pad,label="Priority",horizontal=FALSE)
  PR.plot.raster <<- ggraphics(container=PR.plot.group) # create plot window for tab
  PR.plot.buttons <- ggroup(container = PR.plot.group, horizontal = TRUE) # create space for buttons
  PR.plot.edit <- gbutton("Edit",container=PR.plot.buttons, # Edit button
    handler=function(h,...){ # handler for when button pressed
      # open R's inbuilt editor, save results
      strategy.params$PR[[curr.strategy]] <<- edit(strategy.params$PR[[curr.strategy]]) 
      visible(PR.plot.raster)=TRUE # select plot window
      svalue(strats.raster.pad)=1
      strategy.params$PR.rast[[curr.strategy]] <<- raster(strategy.params$PR[[curr.strategy]]) # save raster version (TODO: use first species carrying capacity as template)
      plotdata(strategy.params$PR[[curr.strategy]]) # plot raster
    })
  PR.plot.save <- gbutton("Save",container=PR.plot.buttons, # Save button
    handler=function(h,...){ # handler for when button pressed
      filename=fileSaveChoose("print") # choose filename to save raster file to
      if(!is.na(filename)){ # if valid filename chosen
        strategy.params$PR.rast[[curr.strategy]]=raster(strategy.params$PR[[curr.strategy]]) # save raster version (TODO: use first species carrying capacity as template)
        writeRaster(strategy.params$PR.rast[[curr.strategy]],filename,format="GTiff") # write raster to file in GTiff format
      }
    })

  # create a tab in the notebook for the Culling Mask raster
  C.plot.group <- ggroup(container=strats.raster.pad,label="Culling Mask",horizontal=FALSE)
  C.plot.raster <<- ggraphics(container=C.plot.group) # create plot window for tab
  C.plot.buttons <- ggroup(container = C.plot.group, horizontal = TRUE) # create space for buttons            
  C.plot.edit <- gbutton("Edit",container=C.plot.buttons, # Edit button
    handler=function(h,...){ # handler for when button pressed
      # open R's inbuilt editor, save results
      strategy.params$C.mask[[curr.strategy]] <<- edit(strategy.params$C.mask[[curr.strategy]])
      visible(C.plot.raster)=TRUE # select plot window
      svalue(strats.raster.pad)=2                                     
      strategy.params$C.rast[[curr.strategy]]<<- raster(strategy.params$C.mask[[curr.strategy]]) # save raster version (TODO: use first species carrying capacity as template)
      plotdata(strategy.params$C.mask[[curr.strategy]]) # plot raster
    })
  C.plot.save <- gbutton("Save",container=C.plot.buttons, # Save button
    handler=function(h,...){ # handler for when button pressed
      filename=fileSaveChoose("print") # choose filename to save raster file to
      if(!is.na(filename)){ # if valid filename chosen
        strategy.params$C.rast[[curr.strategy]] <<- raster(strategy.params$C.mask[[curr.strategy]]) # save raster version (TODO: use first species carrying capacity as template)
        writeRaster(strategy.params$C.rast[[curr.strategy]],filename,format="GTiff") # write raster to file in GTiff format
      }
    })
  
  # create a tab in the notebook for the Culling Mask raster
  TD.plot.group <- ggroup(container=strats.raster.pad,label="Target Density",horizontal=FALSE)
  TD.plot.raster <<- ggraphics(container=TD.plot.group) # create plot window for tab
  TD.plot.buttons <- ggroup(container = TD.plot.group, horizontal = TRUE) # create space for buttons            
  TD.plot.edit <- gbutton("Edit",container=TD.plot.buttons, # Edit button
    handler=function(h,...){ # handler for when button pressed
      # open R's inbuilt editor, save results
      strategy.params$TD[[curr.strategy]] <<- edit(strategy.params$TD[[curr.strategy]])
      visible(TD.plot.raster)=TRUE # select plot window
      svalue(strats.raster.pad)=3
      strategy.params$TD.rast[[curr.strategy]] <<- raster(strategy.params$TD[[curr.strategy]]) # save raster version (TODO: use first species carrying capacity as template)
      plotdata(strategy.params$TD[[curr.strategy]]) # plot raster
    })
  TD.plot.save <- gbutton("Save",container=TD.plot.buttons, # Save button
    handler=function(h,...){ # handler for when button pressed
      filename=fileSaveChoose("print") # choose filename to save raster file to
      if(!is.na(filename)){ # if valid filename chosen
        strategy.params$TD.rast[[curr.strategy]] <<- raster(strategy.params$TD[[curr.strategy]]) # save raster version (TODO: use first species carrying capacity as template)
        writeRaster(strategy.params$TD.rast[[curr.strategy]],filename,format="GTiff") # write raster to file in GTiff format
      }
    })      
            
  # create a tab in the notebook for the Cost Intercept raster           
  CB.B.plot.group <- ggroup(container=strats.raster.pad,label="Cost Intercept",horizontal=FALSE)
  CB.B.plot.raster <<- ggraphics(container=CB.B.plot.group) # create plot window for tab
  CB.B.plot.buttons <- ggroup(container = CB.B.plot.group, horizontal = TRUE) # create space for buttons             
  CB.B.plot.edit <- gbutton("Edit",container=CB.B.plot.buttons, # Edit button
    handler=function(h,...){ # handler for when button pressed
      # open R's inbuilt editor, save results
      strategy.params$CB.B[[curr.strategy]] <<- edit(strategy.params$CB.B[[curr.strategy]])
      visible(CB.B.plot.raster)=TRUE # select plot window
      svalue(strats.raster.pad)=4
      strategy.params$CB.B.rast[[curr.strategy]] <<- raster(strategy.params$CB.B[[curr.strategy]]) # save raster version (TODO: use first species carrying capacity as template)
      plotdata(strategy.params$CB.B[[curr.strategy]]) # plot raster
    })
  CB.B.plot.save <- gbutton("Save",container=CB.B.plot.buttons, # Save button
    handler=function(h,...){ # handler for when button pressed
      filename=fileSaveChoose("print") # choose filename to save raster file to
      if(!is.na(filename)){ # if valid filename chosen
        strategy.params$CB.B.rast[[curr.strategy]] <<- raster(strategy.params$CB.B[[curr.strategy]]) # save raster version (TODO: use first species carrying capacity as template)
        writeRaster(strategy.params$CB.B.rast[[curr.strategy]],filename,format="GTiff") # write raster to file in GTiff format
      }
    })      

  # create a tab in the notebook for the Cost Slope raster     
  CB.A.plot.group <- ggroup(container=strats.raster.pad,label="Cost Slope",horizontal=FALSE)
  CB.A.plot.raster <<- ggraphics(container=CB.A.plot.group) # create plot window for tab
  CB.A.plot.buttons <- ggroup(container = CB.A.plot.group, horizontal = TRUE) # create space for buttons             
  CB.A.plot.edit <- gbutton("Edit",container=CB.A.plot.buttons, # Edit button
    handler=function(h,...){ # handler for when button pressed
      # open R's inbuilt editor, save results
      strategy.params$CB.A[[curr.strategy]] <<- edit(strategy.params$CB.A[[curr.strategy]])
      visible(CB.A.plot.raster)=TRUE # select plot window
      svalue(strats.raster.pad)=5
      strategy.params$CB.A.rast[[curr.strategy]]<<-raster(strategy.params$CB.A[[curr.strategy]]) # save raster version (TODO: use first species carrying capacity as template)
      plotdata(strategy.params$CB.A[[curr.strategy]]) # plot raster
    })
  CB.A.plot.save <- gbutton("Save",container=CB.A.plot.buttons, # Save button
    handler=function(h,...){ # handler for when button pressed
      filename=fileSaveChoose("print") # choose filename to save raster file to
      if(!is.na(filename)){ # if valid filename chosen
        strategy.params$CB.A.rast[[curr.strategy]]<<-raster(strategy.params$CB.A[[curr.strategy]]) # save raster version (TODO: use first species carrying capacity as template)
        writeRaster(strategy.params$CB.A.rast[[curr.strategy]],filename,format="GTiff") # write raster to file in GTiff format
      }
    })   
  
  # create a tab in the notebook for the Hourly Cost raster   
  CB.C.plot.group <- ggroup(container=strats.raster.pad,label="Hourly Cost",horizontal=FALSE)
  CB.C.plot.raster <<- ggraphics(container=CB.C.plot.group) # create plot window for tab
  CB.C.plot.buttons <- ggroup(container = CB.C.plot.group, horizontal = TRUE) # create space for buttons            
  CB.C.plot.edit <- gbutton("Edit",container=CB.C.plot.buttons, # Edit button
    handler=function(h,...){ # handler for when button pressed
      # open R's inbuilt editor, save results    
      strategy.params$CB.C[[curr.strategy]] <<- edit(strategy.params$CB.C[[curr.strategy]])
      visible(CB.C.plot.raster)=TRUE # select plot window
      svalue(strats.raster.pad)=6
      strategy.params$CB.C.rast[[curr.strategy]]<<-raster(strategy.params$CB.C[[curr.strategy]]) # save raster version (TODO: use first species carrying capacity as template)
      plotdata(strategy.params$CB.C[[curr.strategy]]) # plot raster
    })
  CB.C.plot.save <- gbutton("Save",container=CB.C.plot.buttons, # Save button
  handler=function(h,...){ # handler for when button pressed
    filename=fileSaveChoose("print") # choose filename to save raster file to
    if(!is.na(filename)){ # if valid filename chosen
      strategy.params$CB.C.rast[[curr.strategy]]<<-raster(strategy.params$CB.C[[curr.strategy]]) # save raster version (TODO: use first species carrying capacity as template)
      writeRaster(strategy.params$CB.C.rast[[curr.strategy]],filename,format="GTiff") # write raster to file in GTiff format
    }
  })                                      
                                                                 
                                 
  # if the current strategy is defined, plot Priority and Culling Mask rasters 
  # (the other four are dealt with by the checkbox)
  if (!is.null(curr.strategy)) 
  {
    if (length(strategy.params$PR.rast[[curr.strategy]]) > 0) # if opening window with carrying capacity already present
    {
      DATA = raster::as.matrix(strategy.params$PR.rast[[curr.strategy]]) # extract data from strategy.params
      visible(PR.plot.raster)=TRUE # select plot window
      svalue(strats.raster.pad) = 1
      plotdata(DATA) # plot data
    } 
    
    if (length(strategy.params$C.rast[[curr.strategy]]) > 0) # if opening window with carrying capacity already present
    {
      DATA = raster::as.matrix(strategy.params$C.rast[[curr.strategy]])  # extract data from strategy.params
      visible(C.plot.raster)=TRUE # select plot window
      svalue(strats.raster.pad) = 2
      plotdata(DATA) # plot data
    }         
  }
}
