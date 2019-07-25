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
	var.env$vars <- new.env(parent = var.env)
	.load.variables(var.env$vars, 
					.get.line.num(var.env$current.proc, var.env$proc.nodes),
					var.env$current.script)
	
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
	# Quit
	if(input == "Q")
	{
		print("Quitting")
		return(TRUE)
	}
	
	# TODO - MOVEMENT HERE
	
	# move forward - TODO implementation of .move.forward
	else if(input == "n" || grepl("^n[[:digit:]]", input))
	{
		.move.forward(input, var.env)
	}
	
	# move backwards - TODO implementation of .move.backwards
	else if(input == "b" || grepl("^n[[:digit:]]", input))
	{
		.move.backwards(input, var.env)
	}
	
	# TODO - continue until end of script
	else if(input == "c")
	{
		# TODO
	}
	
	# TODO - step in
	else if(input == "s")
	{
		.step.in(var.env)
	}
	
	# TODO - l, code to interpretor
	
	# list variables present in "execution"
	# users can then enter a variable name and its value will be printed
	else if(input == "ls")
	{
		print(ls(var.env$vars))
	}
	
	else if(input %in% ls(var.env$vars))
	{
		print(get(input, envir = var.env$vars))
	}
	
	# move variables to global
	else if(input == "mv")
	{
		if(length(ls(var.env$vars)) == 0)
		{
			cat("Environment is empty, nothing to move.\n")
		}
		else
		{
			# TODO
		}
	}
	
	# help
	else if(input == "help")
	{
		.print.help()
	}
	
	# TODO - else block
	else
	{
		
	}
	
	# continue interactive loop
	return(FALSE)
}



# loads/saves the node environment
# variables from the Global Environment
# 
# this function relies on the function debug.from.line,
# which takes in the starting line number of the procedure node and the script number.
.load.variables <- function(vars, line.num, script.num)
{
	data.nodes <- debug.from.line(line.num, state = T, script.num = script.num)[[1]]
	
	# convert to data frame if resulting table is a matrix (all cells have the same type)
	if(is.atomic(data.nodes))
	{
		# if it is a matrix of NA, there is nothing to load. return.
		if(all(is.na(data.nodes)))
			return()
		
		# there are variables to load. convert to data frame.
		data.nodes <- as.data.frame(data.nodes)
	}
	
	# load variables!
	# the rest of this function is from the original verision of .load.variables
	# the load process uses a temporary environment, which is later removed.
	load.env <- new env()
	
	# Assign each variable and it's value to the created environment
	apply(data.nodes, 1, function(row)
	{
		if(!is.na(row["val"][[1]]))
		{
			# Check if the value is a snapshot, regex here checks for starting with
			# data and is a file with an extension
			if(grepl("^data", row["val"][[1]]) & grepl("^.*\\.[^\\]+$", row["val"][[1]]))
			{
				# If the provenance folder was found the snapshots can be grabbed
				# from the file system
				if(!is.na(.debug.env$prov.folder))
				{
					# The file ext indicates what type of data will be stored and how to 
					# read it back in for the user, the file name is also used to complete
					# the path to the final file
					file.parts <- strsplit(row["val"][[1]], "\\.")
					file.ext <- tolower(file.parts[[1]][[length(file.parts[[1]])]])
					file.name <- file.parts[[1]][1]

					# A text file means that the data has been stored as an RObject
					# this can be loaded back in simply using load()
					if(file.ext == "txt") {
						full.path <- paste(.debug.env$prov.folder,
											"/", file.name,
											".RObject", sep = "")
						# Don't try and read in a file that could possibly not exist
						if(file.exists(full.path)) {
							var.name <- load(full.path, envir = load.env)
							assign(row["var/code"][[1]], get(var.name, load.env), envir = vars)
						} else {
							assign(row["var/code"][[1]], "INCOMPLETE SNAPSHOT", envir = vars)
						}
					# csv could be matrix, array, or data_frame
					} else if (file.ext == "csv") { 
						# a data frame can be read in using the read.csv function
						# which creates a data frame
						if(row[["container"]] == "data_frame") {
							full.path <- paste(.debug.env$prov.folder,
												"/", file.name,
												".csv", sep = "")
							if(file.exists(full.path)) {
								temp.var <- utils::read.csv(full.path, stringsAsFactors = F)
								assign(row["var/code"][[1]], temp.var, envir = vars)
							} else {
								assign(row["var/code"][[1]], "INCOMPLETE SNAPSHOT", envir = vars)
							}
						# A vector can be read in using read.csv but then needs the vectors extracted
						} else if (row[["container"]] == "vector" | row[["container"]] == "matrix") {
							full.path <- paste(.debug.env$prov.folder,
												"/", file.name,
												".csv", sep = "")
							# Grab each column out of the data frame and then bind to create matrix
							# or just a single vector.
							if(file.exists(full.path)) {
								temp.var <- utils::read.csv(full.path, stringsAsFactors = F)
								if(nrow(temp.var) == 0 ) {
									if (row[["container"]] == "vector") {
										temp.var <- rep("", length.out = as.integer(row[["dim"]]))
									}
								} else {
									indexes <- 1:ncol(temp.var)
									temp.var <- cbind(sapply(indexes, function(index) {
													temp.var[[index]]
												}))
									# a single vector is formatted differently than a single column of a matrix
									if(ncol(temp.var) == 1 & row[["container"]] == "vector" ) {
										temp.var <- as.vector(temp.var)
									}
								}
                  
								assign(row["var/code"][[1]], temp.var, envir = vars)
							
							# No file found
							} else {
								assign(row["var/code"][[1]], "INCOMPLETE SNAPSHOT", envir = vars)
							}
							
						# An array is very similar to a vector but can be coerced into an array post read
						} else if (row[["container"]] == "array") {
							if(file.exists(full.path)) {
								temp.var <- read.csv(full.path, stringsAsFactors = F)
								indexes <- 1:ncol(temp.var)
								temp.var <- cbind(sapply(indexes, function(index) {
												temp.var[[index]]
											}))
								temp.var <- as.array(temp.var)
							} else {
								assign(row["var/code"][[1]], "INCOMPLETE SNAPSHOT", envir = vars)
							} 
						
						#No identifiable container
						} else {
							assign(row["var/code"][[1]], "INCOMPLETE SNAPSHOT", envir = vars)
						} 
					
					# No identifiable file extension
					} else {
						assign(row["var/code"][[1]], "INCOMPLETE SNAPSHOT" , envir = vars)
					}
				
				# If the prov.folder was not located
				} else {
					assign(row["var/code"][[1]], "SNAPSHOT/MISSING PROVENANCE" , envir = vars)
				}
			
			#If the data is not a snapshot it can be loaded directly into the variable env
			} else {
				type <- jsonlite::fromJSON(row["type"])$type
				assign(row["var/code"][[1]], methods::as(row["val"][[1]], type), envir = vars)
			}
		}
	})

	# remove the temporary loading environment. the loaded variables are in vars.
	rm(list=ls(load.env), envir = load.env)
}

# clears the environment of loaded variables
.clear.environment <- function(var.env)
{
	var.env$vars <- new.env(parent = var.env)
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

# prints help for debugger
.print.help <- function()
{
	str <- paste("This is a time-traveling debugger for R \n", 
				 "n - Move forward one line\n",
				 "n* - Move forward * number of times (where * is an integer) \n",
				 "b - Move backward one line\n",
				 "b* - Move backward * number of times (where * is an integer)\n",
				 "s - step into a source() call \n",
				 "c - moves to end of \'execution\'\n",
				 "ls - prints name of variables at the current point of \'execution\'\n",
				 "l - print the current line\n",
				 "l* - Move to line * (where * is an integer)\n",
				 "mv - moves the current debugging environment to the Global Environment\n",
				 "help - brings up this dialog \n",
				 "Q - quits the debugger\n"
			))
	cat(str)
}
