seasons.info = function(container = 'settings.group') # draws timestep information
{
  # group to store timestep information
  eval(parse(text = paste('time.pane <<- ggroup(container = ', container ,', horizontal = TRUE)', sep='')))

  # label
  time.label=glabel('Current season:', container=time.pane)
  curr.time <<- 1 # current timestep defaults to 1
  # slider to set timestep
  time.slider <<- gspinbutton(from=1,to=seasons,by=1,value=curr.time,container=time.pane,
    handler=time.slider.handler) # with handler for when slider changed
  time.box <<- gcheckbox('Keep parameters constant', container = time.pane, handler = time.box.handler)
}

draw.vars = function(variables.list, curr.variables.list, curr.defaults.list, container = 'settings.group')
{    
  # draw groups, labels and edit boxes for variables based on 
  # the available variables for the given functions
  for (i in 1:length(variables.list)) # loop over each variable name on the list
  {
    v = variables.list[i] # put current variable name in v
    # use a temporary variable to see if it's stored in interactions
    eval(parse(text = paste('tmp = interactions[[ii]][[jj]]$', v, sep=''))) 
  
    if (length(tmp)>0) # if a value for the variable is stored
    {
      # create a new variable from it called 'variables.' followed by the variable's name
      # with the value that we discovered in tmp
      eval(parse(text = paste('variables.', v, ' <<- tmp', sep=''))) 
    }
    else if (v %in% curr.variables.list) # if the variable is defined for the current functional form
    {
      # define it as the defined default
      eval(parse(text = paste('variables.', v, ' <<- rep(', curr.defaults.list[which(v == curr.variables.list)], ', seasons)', sep='')))
    }
    else # if the variable is not displayed, just define it as NA until we actually need it
    { # note that the visibility of the variable's group (defined below) is dealt with in ff.handler
      eval(parse(text = paste('variables.', v, ' <<- rep(NA, seasons)', sep='')))
    }
  
    # create a group for the variable's label and edit box
    eval(parse(text = paste('settings.', v, ' <<- ggroup(container = ', container ,', horizontal = T)', sep='')))
    # create a label with the variable's name
    eval(parse(text = paste('settings.', v, '.label = glabel("', v, '", container = settings.', v, ')', sep = '')))
    # create an edit box containing the current value of the variable as defined above
    eval(parse(text = paste('settings.', v, '.edit <<- gedit(variables.', v, '[curr.time], container = settings.', v, ')', sep = '')))
  }
}


################################################################################################
# vars.update: updates window's parameter information when necessary -
# Saves value of edit boxes to global variables for current time (istime = FALSE) or for
# all timesteps (istime = TRUE), and then update edit boxes with new current time as defined
# by time slider
################################################################################################

vars.update = function(h,...)
{
  # determines which window this has been called from and loads appropriate functional forms
  if (id(h$obj) == 1) func.forms = predprey.forms # predator-prey
  if (id(h$obj) == 2) func.forms = demo.forms # demographics
  if (id(h$obj) == 3) func.forms = disease.forms # disease

  if (istime) curr.time <<- 1:seasons # if necessary, save edit boxes to ALL timesteps

  env = ls('.GlobalEnv')
  if (id(h$obj) != 1) # if not predator-prey (doesn't include tickboxes - TODO: include multiple classes in pred-prey models)
    for (i in 1:N.species) tickboxes[i, curr.time] <<- svalue(boxes[[i]]) # save box information to tickboxes 
  # (which is directly transferred to the interactions object in update.elements)
  
  funcresp[curr.time] <<- svalue(settings.funcresp, index = TRUE) # set functional response to value specified in droplist
  # extract all possible unique variables for all formulas for interactions of the selected type
  variables.list = unique(unlist(strsplit(as.character(func.forms$Variables), ' ')))
  diff.variables.list = NULL
  if (id(h$obj) == 2) # if called from demographics (window includes diffusion)
  {
    diffresp <<- svalue(settings.diffresp, index = TRUE) # set diffusion response to value specified in droplist
    diff.variables.list = unique(unlist(strsplit(as.character(diffusion.forms$Variables), ' '))) # extract all possible unique variables for all formulas for diffusion interactions (as in ff.handler)
  }
  # define each one of both the demographic and diffusion interaction variables from the already present edit boxes  
  for (v in c(variables.list, diff.variables.list))
    eval(parse(text = paste('variables.', v, '[curr.time] <<- as.double(svalue(settings.', v, '.edit))', sep='')))
 
  curr.time <<-svalue(time.slider) # update current season to value indicated by time slider
  svalue(settings.funcresp, index = TRUE) = funcresp[curr.time] # update functional response droplist
  
  # display values in edit boxes based on the relevant interaction variables 
  for (v in variables.list)
    eval(parse(text = paste('svalue(settings.', v, '.edit) = variables.', v, '[curr.time]', sep='')))
    
  if (id(h$obj) == 2) # if called from demographics (window includes diffusion)
  {
    svalue(settings.diffresp, index = TRUE) = diffresp[curr.time] # update diffusion functional response droplist
    # display values in edit boxes based on the diffusion variables 
    for (v in diff.variables.list)
      eval(parse(text = paste('svalue(settings.', v, '.edit) = variables.', v, '[curr.time]', sep='')))
  }
}


################################################################################################
# update.elements
# Updates interactions variable, which contains most of the information required by the engine,
# based on the values of global variables (which are in turn set using the GUI elements).
# Is run upon exit of a settings window (disease, pred/prey or demographics)
# assuming that the changes made in the settings window have been accepted
################################################################################################

update.elements = function()
{
  # If cancel = TRUE then we are rejecting changes:
  # Change visible status of all buttons back to before window was opened
  # All current button and droplist statuses are stored in variable "status"
  if (cancel) 
  {
    # revert all droplists' visible settings to before window opened
    # (note: setdiff used here to exclude buttons, i.e. case i == j
    # as droplists are all other elements of GUI interactions matrix, i.e. i != j)
    for (i in 1:N.species) for (j in setdiff(1:N.species, i)) svalue(buttons[[i]][[j]], index = TRUE) = status[i,j]

    # Workaround:
    # Normally, if a droplist is set to a particular setting (e.g. "predates on"), selecting that setting again
    # will *not* trigger a handler event - we want to be able to do this in order to edit current settings!
    # This loop makes sure (using the addition of "current" for the droplist text for the current setting)
    # that the current setting is accessible - editing the label means that none of the options are officially
    # "selected" (i.e. svalue(the.object, index=TRUE) returns NA) whereas trying to achieve this effect
    # directly (i.e. svalue(the.object, index=TRUE) <- NA) doesn't work
    
    for (a in 1:N.species)
    {
      for (b in setdiff(1:N.species, a))
      {
        for (i in 1:8) buttons[[a]][[b]][i] <<- droplist.options[i] # first, refresh options (remove any previous "(current)" labels)
        if (status[a,b]>1) buttons[[a]][[b]][status[a,b]] <<- paste(buttons[[a]][[b]][status[a,b]], '(current)') # label current value as current    
      }
    }
  }
  else # if cancel = FALSE, accepting changes:
  {
    if (curr.val==0) # if we're dealing with a button ("curr.val" is the value of the object used to open the settings window)
    {
      interactions[[ii]][[jj]] <<- list() # wipe whatever was there previously 
      interactions[[ii]][[jj]]$funcresp <<- funcresp # add functional response info
      interactions[[ii]][[jj]]$diffresp <<- diffresp # add diffusion functional response info

      # extract all possible unique variables for all formulas for demographic and diffusion interactions
      variables.list = unique(unlist(strsplit(as.character(demo.forms$Variables), ' ')))
      diff.variables.list = unique(unlist(strsplit(as.character(diffusion.forms$Variables), ' ')))

      # store interaction information based on the diffusion variables 
      for (v in c(variables.list, diff.variables.list))
        eval(parse(text = paste('interactions[[ii]][[jj]]$', v, ' <<- variables.', v, sep='')))

      # store minimum possible density info
      interactions[[ii]][[jj]]$min.d <<- min.d

      # directly place tickboxes information into the interactions matrix
      interactions[[ii]][[jj]]$boxes <<- tickboxes      
    }
    else # if it's one of the droplists
    {
      # Update selection of current droplist based on value of curr.val, and status information
      # (status is used to recover droplist information if an interaction window is cancelled)
      svalue(buttons[[ii]][[jj]], index = TRUE) = curr.val
      status[ii,jj] <<- curr.val
         
      if (curr.val == 1 | curr.val == 8)  # if blank or cooperative (cooperative interactions not yet modelled)
      {
        interactions[[ii]][[jj]] <<- list() # wipe whatever was there previously 
        svalue(buttons[[jj]][[ii]], index = TRUE) = curr.val # also set the transverse droplist to the appropriate setting
        status[jj,ii] <<- curr.val # as well as its status info
      }
      else if (curr.val == 2 | curr.val == 3) # if predator/prey
      {
        interactions[[ii]][[jj]] <<- list() # wipe whatever was there previously 

        # deal with labels for current setting
        buttons[[ii]][[jj]][] <<- droplist.options # first, refresh options (remove any previous "(current)" labels)
        buttons[[ii]][[jj]][curr.val] <<- paste(buttons[[ii]][[jj]][curr.val], '(current)') # label current value as current
   
        # TODO: remove transverse stuff (i.e. make two-way interactions possible)
        svalue(buttons[[jj]][[ii]], index = TRUE) = 5 - curr.val # set transverse to opposite setting (i.e. if current is predator, set to prey, and vice versa)
        status[jj,ii] <<- 5 - curr.val

        # deal with labels for transverse setting
        buttons[[jj]][[ii]][] <<- droplist.options
        buttons[[jj]][[ii]][5 - curr.val] <<- paste(buttons[[jj]][[ii]][5 - curr.val], '(current)')

        # update interactions matrix (don't need to do this for transverse, as the two are linked)
        interactions[[ii]][[jj]]$funcresp <<- funcresp

        # extract all possible unique variables for all formulas for predator-prey interactions
        variables.list = unique(unlist(strsplit(as.character(predprey.forms$Variables), ' ')))
        # store interaction information based on the predator-prey variables
        for (v in variables.list)
          eval(parse(text = paste('interactions[[ii]][[jj]]$', v, ' <<- variables.', v, sep='')))
        
      }
      else if (curr.val >= 4 & curr.val <= 7) # if becomes/converts
      {
        # in this case, we use the "from" and "to" settings to deal with transverse elements of the interactions matrix       
        # ("via" is optional)
        from = svalue(settings.from, index=TRUE)
        to = svalue(settings.to, index=TRUE)
        via = svalue(settings.via, index=TRUE) - 1 # including "-1" because first option is "direct", after that are the species names

        # clear elements both ways
        interactions[[from]][[to]] <<- list()
        interactions[[to]][[from]] <<- list()
    
        # set status and visible setting of droplist for current and transpose
        svalue(buttons[[from]][[to]], index = TRUE) = 4
        status[from, to] <<- 4
        svalue(buttons[[to]][[from]], index = TRUE) = 5
        status[to, from] <<- 5

        # set "current" values on droplists
        buttons[[from]][[to]][] <<- droplist.options
        buttons[[from]][[to]][4] <<- paste(buttons[[from]][[to]][4], '(current)')
        
        buttons[[to]][[from]][] <<- droplist.options
        buttons[[to]][[from]][5] <<- paste(buttons[[from]][[to]][5] , '(current)')

        # set "to" and "from" settings to the interactions matrix for next time the data is accessed
        interactions[[from]][[to]]$from <<- from        
        interactions[[from]][[to]]$to <<- to

        interactions[[to]][[from]]$from <<- from  # add to/from settings to other droplists involved in the interaction, so that the settings can be accessed via these as well
        interactions[[to]][[from]]$to <<- to
                                                                                          
        if (via > 0) # use intermediary class (e.g. actual infectious organism for disease)
        {
          svalue(buttons[[from]][[via]], index = TRUE) = 7 # set droplist option for infected species as 'is infected by'
          status[from,via] <<- 7 # set status
          # set "current" values on droplists
          buttons[[from]][[via]][] <<- droplist.options
          buttons[[from]][[via]][7] <<- paste(buttons[[from]][[via]][7], '(current)')
          
          svalue(buttons[[via]][[from]], index = TRUE) = 6 # set droplist option for infecting species to 'infects'
          status[via,from] <<- 6 # set status
          # set "current" values on droplists
          buttons[[via]][[from]][] <<- droplist.options
          buttons[[via]][[from]][6] <<- paste(buttons[[via]][[from]][6], '(current)')

          # clear interactions between from and via, insert relevant from/to/via information
          interactions[[via]][[from]] <<- list()
          interactions[[from]][[via]] <<- list()

          interactions[[from]][[to]]$via <<- via
          interactions[[to]][[from]]$via <<- via
          
          # add to/from settings to other droplists involved in the interaction, so that the settings can be accessed via these as well
          interactions[[via]][[from]]$from <<- from        
          interactions[[via]][[from]]$to <<- to
          interactions[[via]][[from]]$via <<- via

          interactions[[from]][[via]]$from <<- from
          interactions[[from]][[via]]$to <<- to
          interactions[[from]][[via]]$via <<- via                    
        }    

        # note: we're only putting variables in the from/to element, 
        # don't need to put the information in the rest as they reference this one
        interactions[[from]][[to]]$funcresp <<- funcresp
        # extract all possible unique variables for all formulas for disease interactions
        variables.list = unique(unlist(strsplit(as.character(disease.forms$Variables), ' ')))
        # store interaction information based on the disease variables
        for (v in variables.list)
          eval(parse(text = paste('interactions[[from]][[to]]$', v, ' <<- variables.', v, sep='')))
        interactions[[from]][[to]]$boxes <<- tickboxes # save tickbox information
      }
    }
  }
}

################################################################################################
# button.handler: Handles opening settings window when "Demographics" button is pressed
################################################################################################

button.handler = function(h,...)
{
  if (!window.open) # only opens window if no other settings window is open (avoids overwriting and confusion)
  {
    window.open <<- TRUE # set window.open to true so that other windows won't be opened
    cancel <<- FALSE # initialise cancel (global so other functions know whether or not to save Demographics information)
    curr.val <<- 0 # curr.val set to 0 to indicate a button was pressed (and not a droplist)

    update.params() # make sure params are updated (particularly cell.size for density plots)
    
    ij = id(h$obj) # work out the coordinates of the button from the button's "id" method
    ii <<- ij[1]; jj <<- ij[2] # separate into row and column coordinates, store as globals ii and jj (these should be the same)

    # for each variable, check to see if information exists via interactions matrix, if so, use it
    # if not, initialise with defaults (the first option)
    tmp = interactions[[ii]][[jj]]$funcresp # functional response
    if (length(tmp)>0)  # if info exists
      funcresp <<- tmp  # set to interactions info
    else # otherwise
      funcresp <<- rep(1, seasons) # set to default (1)

    tmp = interactions[[ii]][[jj]]$diffresp # diffusion type
    if (length(tmp)>0) # if info exists 
      diffresp <<- tmp # set to interactions info
    else  # otherwise
      diffresp <<- 1 # set to default (1)
    
    tmp = interactions[[ii]][[jj]]$min.d # minimum possible density
    if (length(tmp)>0) # if info exists 
      min.d <<- tmp # set to interactions info
    else # otherwise
      min.d <<- 0.01 # set to default (0.01)
    
    tmp = interactions[[ii]][[jj]]$boxes # checkboxes
    if (length(tmp)>0) # if info exists 
      tickboxes <<- tmp  # set to interactions info
    else # otherwise
    {
      checked = rep(FALSE, N.species) # set all to false except for:
      checked[ii] = TRUE # just the current class (no other classes count towards species total)
      tickboxes <<- matrix(rep(checked, seasons), N.species, seasons) # repeat for every season
    }

    # Create window and contents       
    settings <<- gwindow("Single-species dynamics details", handler = function(h,...) window.open <<- FALSE, width = 800, height = 600)
    id(settings) = 2 # sets window ID so that vars.update updates the right variables
    addHandlerUnrealize(settings, handler = function(h,...) return(TRUE)) # disable closing (will handle via "OK" and "Cancel" buttons)

    # add scroll group so that we can navigate easily
    scroll.group <- ggroup(container=settings,use.scrollwindow=T) 

    # create group to contain buttons, will be organised in three vertical panes        
    settings.group <<- ggroup(container=scroll.group,horizontal=TRUE) 

    # create the left-most pane
    settings.group1 <<- ggroup(container=settings.group,horizontal=FALSE) 

    seasons.info(container = 'settings.group1') # draws timestep information

    # droplist containing functional responses
    settings.funcresp <<- gdroplist(items=as.character(demo.forms$Model.name), container = settings.group1, handler = ff.handler, selected = funcresp[curr.time])
    id(settings.funcresp) = 2 # identifying which window 
    settings.labeltitle = glabel("Functional response:", container = settings.group1) # label
    settings.label <<- glabel(demo.forms$Model.formula[funcresp[curr.time]], container = settings.group1) # showing formula of selected response

    # extract all possible unique variables for all demographic formulas
    variables.list = unique(unlist(strsplit(as.character(demo.forms$Variables), ' ')))
    # select variables from current functional form
    curr.variables.list = strsplit(as.character(demo.forms$Variables[funcresp[curr.time]]), ' ')[[1]] 
    # select default variable values from current functional form
    curr.defaults.list = strsplit(as.character(demo.forms$Defaults[funcresp[curr.time]]), ' ')[[1]]
    
    # draw groups, labels and edit boxes for variables based on 
    # the available variables for demographic functions
    draw.vars(variables.list, curr.variables.list, curr.defaults.list, 'settings.group1')

    # label
    a.label = glabel('Interacting classes within species (counting towards N):', container = settings.group1)

    # define a box for each season to contain info about interacting species
    boxes <<- vector('list', N.species)
    boxes.group = ggroup(container = settings.group1, horizontal = TRUE) # group for boxes
    
    for (i in 1:N.species) # for each species, create a box and store its info in the created variable
      boxes[[i]] <<- gcheckbox(species[i], checked = tickboxes[i, curr.time], container = boxes.group)
    
    ff.handler(list(obj = settings.funcresp)) # apply initial settings
    # use list(obj = object) because this is the expected format for a handler
    # (note ff.handler is also used as the handler for object settings.funcresp)
                      
    # Define minimum population density possible (TODO: is it density?)
    # Represents difficulty in getting population *really* low
    # May not be viable if we have a population invading a new habitat
    min.d.group=ggroup(container=settings.group1)# group for label and spinbutton
    min.d.label=glabel("Minimum possible density (per km^2) in a cell",container=min.d.group) #label
    addSpring(min.d.group) # make sure spinbutton is aligned to the right
    min.d.slider <<- gspinbutton(from=0,to=1,by=.01,value=min.d,container=min.d.group, # spinbutton with handler
      handler=function(h,...) min.d <<- svalue(min.d.slider)) # which sets variable to current spinbutton value
                     
    # Droplist containing functional forms for diffusion
    settings.diffresp <<- gdroplist(items=as.character(diffusion.forms$Model.name), container = settings.group1,
      handler = ff.handler, selected = diffresp) # handler for when diffusion model is changed
    id(settings.diffresp) = 4 # set id to let handler know we're dealing with diffusion
    settings.labeltitle <<- glabel("Diffusion term:", container = settings.group1) # label
    settings.difflabel <<- glabel(diffusion.forms$Model.formula[diffresp], container = settings.group1) # label showing model formula

    # extract all possible unique variables for all formulas for interactions of the selected type
    diff.variables.list = unique(unlist(strsplit(as.character(diffusion.forms$Variables), ' ')))
    # select variables from current functional form
    diff.curr.variables.list = strsplit(as.character(diffusion.forms$Variables[diffresp]), ' ')[[1]]
    # select default variable values from current functional form
    diff.curr.defaults.list = strsplit(as.character(diffusion.forms$Defaults[diffresp]), ' ')[[1]]

    # draw groups, labels and edit boxes for variables based on 
    # the available variables for diffusion functions
    for (i in 1:length(diff.variables.list))
    {
      v = diff.variables.list[i] # put current variable name in v
      # use a temporary variable to see if it's stored in interactions
      eval(parse(text = paste('tmp = interactions[[ii]][[jj]]$', v, sep='')))

      if (length(tmp)>0) # if a value for the variable is stored
      {
        # create a new variable from it called 'variables.' followed by the variable's name
        # with the value that we discovered in tmp      
        eval(parse(text = paste('variables.', v, ' <<- tmp', sep='')))
      }
      else if (v %in% diff.curr.variables.list) # if the variable is defined for the current functional form
      {
        # define it as the defined default
        eval(parse(text = paste('variables.', v, ' <<- ', diff.curr.defaults.list[which(v == diff.curr.variables.list)], sep='')))
      }
      else # if the variable is not displayed, just define it as NA until we actually need it
      { # note that the visibility of the variable's group (defined below) is dealt with in ff.handler
        eval(parse(text = paste('variables.', v, ' <<- rep(NA, seasons)', sep='')))
      }

      # create a group for the variable's label and edit box
      eval(parse(text = paste('settings.', v, ' <<- ggroup(container = settings.group1, horizontal = T)', sep='')))
      # create a label with the variable's name
      eval(parse(text = paste('settings.', v, '.label = glabel("', v, '", container = settings.', v, ')', sep = '')))
      # create an edit box containing the current value of the variable as defined above
      eval(parse(text = paste('settings.', v, '.edit <<- gedit(variables.', v, ', container = settings.', v, ')', sep = '')))
    }                     
                     
    ff.handler(list(obj = settings.diffresp)) # apply initial settings to diffusion stuff
    
    # create the second-from-left pane
    settings.group2 = ggroup(container=settings.group,horizontal=FALSE)

    K.label <<- glabel("",container=settings.group2) # label for carrying capacity
    # Create button for loading carrying capacity
    K.button <<- gbutton("Load Carrying Capacity Raster", container=settings.group2,
      handler= K.button.handler)

    K.plot.raster <<- ggraphics(container=settings.group2) # create plot area for CC

    # create space for buttons
    K.plot.buttons <- ggroup(container = settings.group2, horizontal = TRUE)

    # create Edit button
    K.plot.edit <<- gbutton("Edit",container=K.plot.buttons,
      handler=function(h,...){ # with handler for when button pressed
        K[[ii]] <<- edit(K[[ii]]) # use R's inbuilt editor to edit the raster
        visible(K.plot.raster)=T # select plot window
        K.rast[[ii]]=raster(K[[ii]]) # create raster from changed matrix (TODO: use K[[1]] as a template so we keep projection info)
        plotdata(K[[ii]]/cell.size, main = 'Density (per km^2)') # plot data (density, not abundance)
      })
    
    # create Save button
    K.plot.save <<- gbutton("Save",container=K.plot.buttons,
      handler=function(h,...){ # with handler for when button pressed
        filename=fileSaveChoose("print") # user selects a filename to save to
        if(!is.na(filename)){ # if valid filename selected
          K.rast[[ii]]=raster(K[[ii]]) # create raster from current matrix (TODO: use K[[1]] as a template so we keep projection info)
          writeRaster(K.rast[[ii]],filename,format="GTiff") # write in GTiff format to filename
        }
      })                                   
                                       
    if (length(K.rast[[ii]]) > 0) # if opening window with carrying capacity already present
    {
      DATA = raster::as.matrix(K.rast[[ii]]) # set matrix of data
      plotdata(DATA/cell.size, main = 'Density (per km^2)') # plot data (density, not abundance)
    } 

    # create the third-from-left pane
    settings.group3 = ggroup(container=settings.group,horizontal=FALSE)
  
    IC.label <<- glabel("",container=settings.group3) # label for initial condition
    # create button for loading initial condition
    IC.button <<- gbutton("Load Initial Condition Raster", container=settings.group3,
      handler=function(h,...){ # with handler for when button pressed
        # load user-specified raster information into allthedata
        allthedata = raster.load.button.handler()
        if (!is.null(allthedata)) # if it worked
        {
          # set global variables to necessary data
          IC.rast[[ii]] <<- allthedata$the.rast 
          IC[[ii]] <<- allthedata$the.data
          svalue(IC.label)=allthedata$the.file # set label
          visible(IC.plot.raster)=T # select plot window
          plotdata(IC[[ii]]/cell.size, main = 'Density (per km^2)') # plot data (density, not abundance)
        }  
      })
                         
    IC.plot.raster <<- ggraphics(container=settings.group3) # create plot area for IC

    # create space for buttons    
    IC.plot.buttons <- ggroup(container = settings.group3, horizontal = TRUE)   
                        
    # create Edit button                          
    IC.plot.edit <<- gbutton("Edit",container=IC.plot.buttons,
      handler=function(h,...){ # with handler for when button pressed
        IC[[ii]] <<- edit(IC[[ii]]) # use R's inbuilt editor to edit the raster
        visible(IC.plot.raster)=T # select plot window
        IC.rast[[ii]][locs]=IC[[ii]][locs] # create raster from changed matrix
        plotdata(IC[[ii]]/cell.size, main = 'Density (per km^2)') # plot data (density, not abundance)
      })
      
    # create Save button      
    IC.plot.save <<- gbutton("Save",container=IC.plot.buttons,
      handler=function(h,...){ # with handler for when button pressed
        filename=fileSaveChoose("print") # user selects a filename to save to
        if(!is.na(filename)){ # if valid filename selected
          IC.rast[[ii]][locs]=IC[[ii]][locs]  # create raster from current matrix
          writeRaster(IC.rast[[ii]],filename,format="GTiff") # write in GTiff format to filename
        }
      }) 
                                       
    if (length(IC.rast[[ii]]) > 0)# if opening window with initial condition already present      
    {
      DATA = raster::as.matrix(IC.rast[[ii]]) # set matrix of data
      plotdata(DATA/cell.size, main = 'Density (per km^2)') # plot data (density, not abundance)
    }    

    # create checkbox to select if initial capacity is same as carrying capacity
    # (the IC is initially set to carrying capacity by default - 
    # this is more for if it is changed and we want to change it back)
    ic.equals.k <<- gcheckbox('Starts at carrying capacity', checked = FALSE, container = IC.plot.buttons,
      handler = function(h, ...){ # with handler for if checkbox pressed
        # make IC info invisible if IC is set to same as K
        visible(IC.label) = !svalue(h$obj)
        visible(IC.button) = !svalue(h$obj)
        visible(IC.plot.raster) = !svalue(h$obj)
        # if checkbox ticked, set IC raster and matrix accordingly
        if (svalue(h$obj)) {IC[[ii]] <<- K[[ii]]; IC.rast[[ii]] <<- K.rast[[ii]]}
      }) 
    
    # create the right-most pane    
    settings.group4 = ggroup(container=settings.group,horizontal=FALSE)

    EL.label <<- glabel("",container=settings.group4)
    EL.button <<- gbutton("Load Dispersal Raster", container=settings.group4,
      handler=function(h,...){ # with handler for when button pressed
        # load user-specified raster information into allthedata
        allthedata = raster.load.button.handler()
        if (!is.null(allthedata)) # if it worked
        {
          # set global variables to necessary data
          EL.rast[[ii]] <<- allthedata$the.rast 
          EL.species[[ii]] <<- allthedata$the.data
          svalue(EL.label)=allthedata$the.file # set label
          visible(EL.plot.raster)=T # select plot window
          plotdata(EL.species[[ii]]) # plot data
        }  
      })

    EL.plot.raster <<- ggraphics(container=settings.group4)

    EL.plot.buttons <- ggroup(container = settings.group4, horizontal = TRUE)                         

    # create Edit button   
    EL.plot.edit <<- gbutton("Edit",container=EL.plot.buttons,
      handler=function(h,...){ # with handler for when button pressed
        EL.species[[ii]] <<- edit(EL.species[[ii]]) # use R's inbuilt editor to edit the raster
        visible(EL.plot.raster)=T # select plot window
        EL.rast[[ii]]=raster(EL.species[[ii]]) # create raster from changed matrix (TODO: use K[[1]] as a template so we keep projection info)
        plotdata(EL.species[[ii]]) # plot data
      })

    # create Save button 
    EL.plot.save <<- gbutton("Save",container=EL.plot.buttons,
      handler=function(h,...){ # with handler for when button pressed
        filename=fileSaveChoose("print") # user selects a filename to save to
        if(!is.na(filename)){ # if valid filename selected
          EL.rast[[ii]]=raster(EL.species[[ii]]) # create raster from current matrix (TODO: use K[[1]] as a template so we keep projection info)
          writeRaster(EL.rast[[ii]],filename,format="GTiff") # write in GTiff format to filename
        }
      })                                   
                                       
    if (length(EL.rast[[ii]]) > 0) # if opening window with dispersal info already present
    {
      DATA = raster::as.matrix(EL.rast[[ii]])  # set matrix of data
      plotdata(DATA) # plot data
    } 

    # space for OK and Cancel buttons on leftmost pane (TODO: make these more prominent?)
    settings.buttons = ggroup(container=settings.group1,horizontal=T) 
    # if OK is pressed, update global variables with all current information, update the interactions matrix, and close the window
    settings.OK = gbutton('OK', container = settings.buttons, handler = settings.OK.handler)
    # if Cancel is pressed, use update.elements to revert all changes to the main window, and close the settings window
    settings.Cancel = gbutton('Cancel', container = settings.buttons, handler = settings.Cancel.handler)
  }
}

################################################################################################
# droplist.handler: Handles opening settings window when any of the droplists are used
# Similarly set up to button.handler
################################################################################################

droplist.handler = function(h,...)
{
  # if a window isn't already open and handler is actually given an object
  if (!window.open & !is.null(svalue(h$obj, index = TRUE)))
  {
    window.open <<- TRUE # make sure no other windows are opened while we do this
    curr.val <<- svalue(h$obj, index = TRUE) # set global variable based on current choice (used in other functions)
    cancel <<- FALSE  # initialise cancel (global so other functions know whether or not to save droplist information)
    ij = id(h$obj) # gets location information from droplist (works out where we are in the matrix)
    ii <<- ij[1]; jj <<- ij[2] # splits information into "from" and "to" species

    # Open window for more settings
    if (curr.val == 1)  # if blank
    {
      # make sure the user doesn't accidentally wipe their work
      ans = winDialog(type='yesno', message='Are you sure you want to erase interaction information for these species?')
      if (ans == 'NO') cancel <<- TRUE
      # wipe interaction element (or not, if cancel = TRUE) and then finish
      update.elements();
      window.open <<- FALSE # allow other windows to be opened now we're done
    }
    else if (curr.val == 8)  # not implemented
    {

    }
    else if (curr.val == 2 | curr.val == 3) # if predator/prey
    {
      if (curr.val == 3) # prey
      {
        curr.val <<- 2 # changing positions in the matrix to predator->prey instead of prey->predator
        kk = ii # swap ii and jj so we're reading/writing the predator->prey value in the matrix
        ii <<- jj
        jj <<- kk
      }

      # create window for predator/prey details
      settings <<- gwindow("Predator-prey interaction details") 
      # add ID so that this window's droplist handler knows what options to use
      id(settings) = 1 
      
      # disable closing (will handle via "OK" and "Cancel" buttons)
      addHandlerUnrealize(settings, handler = function(h,...) return(TRUE)) 
      scroll.group <<- ggroup(container=settings,use.scrollwindow=T) # scrolling for easier user interaction
      settings.group <<- ggroup(container=scroll.group,horizontal=F) # group to put everything in

      seasons.info() # draws timestep information

      tmp = interactions[[ii]][[jj]]$funcresp # functional response
      if (length(tmp)>0) # if info exists
        funcresp <<- tmp # set to interactions info
      else  # otherwise
        funcresp <<- rep(1, seasons) # set to default (1)

      # droplist containing functions
      settings.funcresp <<- gdroplist(items=as.character(predprey.forms$Model.name), container = settings.group, selected = funcresp[curr.time], 
        handler = ff.handler) # with handler for when option changed
      id(settings.funcresp) = 1 # giving ID to droplist so that handler knows what to do
      settings.labeltitle = glabel("Functional response:", container = settings.group) # label
      # label with current formula
      settings.label <<- glabel(predprey.forms$Model.formula[funcresp[curr.time]], container = settings.group)

      # extract all possible unique variables for all formulas for interactions of the selected type
      variables.list = unique(unlist(strsplit(as.character(predprey.forms$Variables), ' '))) # all variables from every functional form
      # select variables from current functional form
      curr.variables.list = strsplit(as.character(predprey.forms$Variables[funcresp[curr.time]]), ' ')[[1]]
      # select default variable values from current functional form
      curr.defaults.list = strsplit(as.character(predprey.forms$Defaults[funcresp[curr.time]]), ' ')[[1]]
      
      # draw groups, labels and edit boxes for variables based on 
      # the available variables for predator-prey functions
      draw.vars(variables.list, curr.variables.list, curr.defaults.list)
            
      ff.handler(list(obj = settings.funcresp)) # apply initial settings
      
      # space for OK and Cancel buttons on leftmost pane (TODO: make these more prominent?)
      settings.buttons = ggroup(container=settings.group,horizontal=T) 
      # if OK is pressed, update global variables with all current information, update the interactions matrix, and close the window
      settings.OK = gbutton('OK', container = settings.buttons, handler = settings.OK.handler)
      # if Cancel is pressed, use update.elements to revert all changes to the main window, and close the settings window
      settings.Cancel = gbutton('Cancel', container = settings.buttons, handler = settings.Cancel.handler)
    }
    else if (curr.val >=4 & curr.val<=7) # if becomes/converts/infects/infected by
    {
      if (curr.val == 5 | curr.val == 6) # if converts or infects, we need to swap positions
      {
        if (curr.val == 5) 
          curr.val <<- 4 # changing the option to a "from->to" style instead of "to->from"
        else
          curr.val <<- 7 
        kk = ii # swapping positions for storage in the interactions matrix
        ii <<- jj
        jj <<- kk
      }

      # open window for conversion details
      settings <<- gwindow("Conversion interaction details")
      # add ID so that this window's droplist handler knows what options to use
      id(settings) = 3
      # disable closing (will handle via "OK" and "Cancel" buttons)
      addHandlerUnrealize(settings, handler = function(h,...) return(TRUE))
      scroll.group <<- ggroup(container=settings,use.scrollwindow=T) # add scroll window for ease of use
      settings.group <<- ggroup(container=scroll.group,horizontal=F) # group to put everything in

      seasons.info() # draws timestep information

      # for each variable, check to see if information exists via interactions matrix, if so, use it
      # if not, initialise with defaults (the first option)
      tmp = interactions[[ii]][[jj]]$from; # original species
        if (length(tmp)>0) from = tmp else from = ii # set as current 'from' species (ii) unless info already present 
      tmp = interactions[[ii]][[jj]]$to; # target species (what original species becomes)
      if (length(tmp)>0) to = tmp else to = jj # set as current 'to' species (jj) unless info already present 
      tmp = interactions[[ii]][[jj]]$via; # 'infecting' species (catalyst causing conversion)
      if (length(tmp)>0) via = tmp else via = 0 # assume there isn't one (0 setting) unless info already present

      # display interacting species' info
      settings.line1 = ggroup(container=settings.group,horizontal=T) # group to contain info in droplists in a horizontal line
      settings.from <<- gdroplist(items=species, selected=from, container = settings.line1) # showing 'from' species
      a.label = glabel('becomes', container = settings.line1) # label
      settings.to <<- gdroplist(items=species, selected=to, container = settings.line1) # showing 'to' species
      a.label = glabel('via', container = settings.line1) # label
      settings.via <<- gdroplist(items=c('direct conversion', paste('infection by', species)), container = settings.line1, # showing 'via' species (and 'direct' option)
        handler = function(h,...) # with handler for when option changed
          if (svalue(h$obj) > 1) svalue(boxes[[svalue(h$obj, index=TRUE) - 1]]) = TRUE, selected = via + 1) # add 'via' species to 'interacting classes' boxes (see below)

      tmp = interactions[[from]][[to]]$funcresp # functional response
      if (length(tmp)>0) funcresp <<- tmp else funcresp <<- rep(1, seasons) # if not found, default to 1
      
      tmp = interactions[[from]][[to]]$boxes  # 'interacting classes' boxes (see below)
      if (length(tmp)>0) 
        tickboxes <<- tmp 
      else 
      { # if not found, set to true for "from" and "to" species only for each season
        checked = rep(FALSE, N.species)
        checked[ii] = TRUE; checked[jj] = TRUE
        tickboxes <<- matrix(rep(checked, seasons), N.species, seasons) 
      }
 
      # draw functional response info      
      settings.funcresp <<- gdroplist(items=as.character(disease.forms$Model.name), container = settings.group, selected = funcresp[curr.time], 
        handler = ff.handler)
      id(settings.funcresp) = 3 # set ID so handler knows what to do
      settings.labeltitle = glabel("Functional response:", container = settings.group) # label
      settings.label <<- glabel(disease.forms$Model.formula[funcresp[curr.time]], container = settings.group) # label for formula

      # extract all possible unique variables for all formulas for interactions of the selected type
      variables.list = unique(unlist(strsplit(as.character(disease.forms$Variables), ' '))) # all variables from every functional form
      # select variables from current functional form
      curr.variables.list = strsplit(as.character(disease.forms$Variables[funcresp[curr.time]]), ' ')[[1]]
      # select default variable values from current functional form
      curr.defaults.list = strsplit(as.character(disease.forms$Defaults[funcresp[curr.time]]), ' ')[[1]]
      
      # draw groups, labels and edit boxes for variables based on 
      # the available variables for disease functions
      draw.vars(variables.list, curr.variables.list, curr.defaults.list)

      ff.handler(list(obj = settings.funcresp)) # apply initial settings

      # looks at which 'species' (or classes) contribute towards the total population (this is relevant
      # for some functional forms, esp. those used in disease ecology)

      # label
      a.label = glabel('Interacting classes within species (counting towards N):', container = settings.group)
      boxes <<- vector('list', N.species) # initialise boxes
      boxes.group = ggroup(container = settings.group, horizontal = TRUE) # create group
      for (i in 1:N.species) # for each species
        boxes[[i]] <<- gcheckbox(species[i], checked = tickboxes[i, curr.time], container = boxes.group) # create a checkbox

      # space for OK and Cancel buttons on leftmost pane (TODO: make these more prominent?)
      settings.buttons = ggroup(container=settings.group,horizontal=T) 
      # if OK is pressed, update global variables with all current information, update the interactions matrix, and close the window
      settings.OK = gbutton('OK', container = settings.buttons, handler = settings.OK.handler)
      # if Cancel is pressed, use update.elements to revert all changes to the main window, and close the settings window
      settings.Cancel = gbutton('Cancel', container = settings.buttons, handler = settings.Cancel.handler)
    }

  }
}

################################################################################################
# change.handler
# Opens window to allow user to change a selected species' name
# (name change is selected by clicking on species' name on left or top of matrix)
################################################################################################

change.handler = function(h,...)
{
  ij = id(h$obj) # gets ID from button to tell us which species' name we're changing
  window.open <<- TRUE # avoids opening other windows
  # create change window
  # note: in this case, we allow closing the window if user changes their mind
  change.name <<- gwindow("Change species name") 
  change.pane = ggroup(container = change.name, horizontal = TRUE) # group to contain info
  a.label = glabel("New name:", container = change.pane) # label
  change.edit = gedit(species[ij], container = change.pane) # create edit box for new species name
  # IF OK is pressed, change species' name in variable and corresponding information in window
  change.ok = gbutton("OK", container = change.pane, handler = function(h,...) # OK button
  {
    window.open <<- FALSE; # allow new windows to be opened
    species[ij] <<- svalue(change.edit); # update species name
    svalue(species.labels.left[[ij]]) = species[ij]; # update left label with species name
    svalue(species.labels.top[[ij]]) = species[ij]; # update top label
    species.droplist[] <<- species # update species droplist, including new value
    if (!is.null(visible(culling.droplist))) culling.droplist[] <<- species # define droplist elements as new species list    
    dispose(change.name) # remove window
  })
}

################################################################################################
# create.window: Function to populate Interactions panel with current settings
################################################################################################
create.window = function()
{
  # initialise list-within-list containing button objects
  buttons <<- vector('list', N.species)
  # create nested lists
  for (i in 1:N.species) buttons[[i]] <<- vector('list', N.species)

  # create group within pre-created Interactions tab for storing objects
  win.pane <- ggroup(container = int.group, horizontal = FALSE)
  
  # create group to store interactions matrix
  matrix.pane <- ggroup(container=win.pane, horizontal=TRUE)
  
  # create group for left-hand-side labels
  left.pane <- ggroup(container=matrix.pane, horizontal=FALSE)
  # first label is blank
  a.label = glabel("",container=left.pane)

  # create label for each species on left hand side
  # (actually buttons so they can be clicked to change species names)
  for (j in 1:N.species)
    species.labels.left[[j]] <<- gbutton(species[j], container=left.pane, border = FALSE, handler = change.handler)

  # set IDs for each newly created label so change.handler knows which species to deal with
  for (j in 1:N.species) # TODO: I don't know why the IDs can't be done in sequence instead of one after the other...
    id(species.labels.left[[j]]) <<- j

  # initial droplist options for all inter-species interactions
  # (only usually changes for each droplist are the addition of "current" to the currently selected option)
  droplist.options <<- c('','predates on', 'is prey to', 'becomes', 'converts', 'infects', 'is infected by', 'cooperates with')
  
  # draw all buttons
  for (i in 1:N.species)
  {
    # set up a group for a column of buttons
    a.pane <- ggroup(container=matrix.pane,horizontal=FALSE)
    # draw a species label to start the column 
    species.labels.top[[i]] <<- gbutton(species[i],container=a.pane, border = FALSE, handler = change.handler)
    # draw the buttons
    for (j in 1:N.species)
    {
      if (i == j) # if intra-species, put in a demographics button
      {
        # we use j,i format here as we're drawing these column = i first
        buttons[[j]][[i]] <<- gbutton('Demographics', container = a.pane, handler = button.handler)
      }
      else
      {
        curr.val <<- status[j,i] # work out which type of interaction
        if (length(curr.val) == 0) curr.val <<- 1 # if not specified, default to first option (blank)
        # create combobox (similar to droplist) for interaction options
        # TODO: editable = TRUE is a workaround
        # without it, dropboxes don't keep their status
        buttons[[j]][[i]] <<- gcombobox(items=droplist.options, editable = TRUE, container = a.pane, handler = droplist.handler, selected = curr.val)
      }
      id(buttons[[j]][[i]]) <<- c(j,i) # set ID of button so that handler knows which it's dealing with
    }
  }

  # set IDs for each newly created label so change.handler knows which species to deal with
  for (i in 1:N.species)
    id(species.labels.top[[i]]) <<- i  

}
#gtkMain()