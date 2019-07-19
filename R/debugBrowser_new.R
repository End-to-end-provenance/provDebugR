# at init:
# set current script to 1
# from all proc nodes, get list of possible nodes to step through
# subset to get corresponding proc nodes (are both pos.lines and proc.nodes necessary?)
# load existing variables

# main function!
debug.browser <- function()
{
	current.script = 1
	proc.nodes <- provParseR::get.proc.nodes(.debug.env$prov)
	
	# list of possible nodes to step through at the current script/execution level
	step.list <- .get.step.list(proc.nodes, current.script)
	
	
}

# loads/saves the node environment
# variables from the Global Environment
# saves the proc num and script num of the node
.load.node.env <- function()
{}

# for this current execution level, get the list of procedure nodes to step through
# can get complicated with stepping in and out of scripts & functions etc.
.get.step.list <- function(proc.nodes, current.script)
{
	proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
	proc.nodes <- proc.nodes[proc.nodes$scriptNum == current.script, ]
	
	return(proc.nodes)
}

