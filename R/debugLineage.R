debug.lineage <- function(..., forward = F) {
  # Collect the arguments passed to the function
  args <- list(...)

  # This function is useless unless the adj.graph exists
  if(!debug.env$has.graph) {
    stop("debug.init must be run first")
  }

  # Collect possible results the user could ask for
  pos.vars <- get.data.nodes()
  pos.vars <- as.list(unique(pos.vars$name))
  #names(pos.vars) <- pos.vars

  # Make sure all the results passed by the user are valid
  pos.args <- lapply(args, function(arg, pos.vars){
    if(arg %in% pos.vars) {
      T
    } else {
      warning(paste(arg, " is not a possible result"))
    }
  }, pos.vars = pos.vars)

  args <- args[unlist(pos.args)]

  # If they did not provide any results themselve, list them out for them
  if(length(args) == 0) {
    print("Possible results:")
    print(pos.vars)
  } else {
    ls <- lapply(args, grab.lineage, pos.vars = pos.vars, forward = forward)
    names(ls) <- args
  }

}

grab.lineage <- function(result, pos.vars, forward) {
  # The data nodes have all the information on the variables
  data.nodes <- get.data.nodes()
  proc.nodes <- get.proc.nodes()

  # Get all the nodes from the requested variable, but since
  # it's going either forward or backward grab the end
  node.label <- NULL
  if(!forward) {
    node.label <- tail(n=1,data.nodes[data.nodes$name == result, ])$label
  } else {
    node.label <- head(n=1,data.nodes[data.nodes$name == result, ])$label
  }

  # Grab the nodes that have connections to the chosen node form the adj graph
  spine <- get.spine(node.label, forward)

  lines <- lapply(spine[grep("p[[:digit:]]", spine)], function(proc.node){
    list(proc.nodes[proc.nodes$label == proc.node, ]$startLine,
         proc.nodes[proc.nodes$label == proc.node, ]$name)
  })

  df <- data.frame(do.call(rbind, lines), stringsAsFactors = F)
  df <- df[seq(dim(df)[1], 1), ]
  colnames(df) <- c("line", "code")
  df
}
