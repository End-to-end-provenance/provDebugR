# at init:
# set current script to 1
# from all proc nodes, get list of possible nodes to step through
# subset to get corresponding proc nodes (are both pos.lines and proc.nodes necessary?)
# load existing variables
# display instructions
# start interactive loop

# main function!
debug.browser <- function()
{
	# the variable environment
	# contains the following:
	var.env <- new.env(parent = emptyenv())
	
	# procedure nodes: remove 'p' classification, just leave the numbers!
	var.env$proc.nodes <- provParseR::get.proc.nodes(.debug.env$prov)
	var.env$proc.nodes$id <- as.integer( sub("^[[:alpha:]]+", "", var.env$proc.nodes$id) )
	
	# get the list of scripts.
	# top-most level where we are starting at, the script number is 1
	var.env$scripts <- provParseR::get.scripts(.debug.env$prov)
	var.env$current.script = 1L
	
	# list of possible nodes to step through at the current script/execution level
	var.env$step.list <- .get.step.list(var.env$proc.nodes, var.env$current.script)
	
	# the first procedure node. this is where execution starts!
	var.env$current.proc <- var.env$step.list$id[1]
	
	# create an environment to store all the variables loaded from Global
	# load any pre-existing variables into vars.
	# as they were not created due to the execution of the script, the script number is NA.
	var.env$vars <- new.env(parent = var.env)
	.load.variables(var.env$vars, 
					.get.line.num(var.env$current.proc, var.env$proc.nodes),
					NA)
	
	# Print instructions on how to use the debugger as well as the name of the main script.
	cat("Debugger initialized, type \"help\" for more information or Q to quit\n")
	cat(paste(var.env$scripts$script[1L]), "\n", sep="")
	
	# Print out the code for the firstt procedure node
	.print.line(var.env$current.proc, var.env$proc.nodes)
	
	# Start "interactive conole" loop!
	# it will repeatedly prompt for an input until the user quits
	# It operates similarly to the R browser() function
	while(TRUE)
	{
		input <- readline(prompt = "Debug> ")
		quit <- .read.input(input, var.env)
		
		if(quit)
			break
	}
}


.read.input <- function(input, var.env)
{
	# TODO
}



# loads/saves the node environment
# variables from the Global Environment
# 
# this function relies on the function debug.from.line,
# which takes in the starting line number of the procedure node and the script number.
.load.variables <- function(vars, line.num, script.num)
{
	data.nodes <- debug.from.line(line.num, state = T, script.num = script.num)[[1]]
	
	# TODO
}

# for this current execution level, get the list of procedure nodes to step through
# can get complicated with stepping in and out of scripts & functions etc.
.get.step.list <- function(proc.nodes, current.script)
{
	proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
	proc.nodes <- proc.nodes[proc.nodes$scriptNum == current.script, ]
	
	return(proc.nodes)
}

# prints the given line.
.print.line <- function(proc.id, proc.nodes)
{
	node <- proc.nodes[proc.nodes$id == proc.id, ]
	cat(paste(node$startLine, ": ", node$name, "\n", sep=""))
}

.get.line.num <- function(proc.node.id, proc.nodes)
{
	return( proc.nodes$startLine[proc.nodes$id == proc.node.id] )
}