#' importsExports
#' Summarise the  movement type of nodes (susceptible contacts)
#' as either imports (the number of incoming connections),
#' or outputs (the number of outgoing connections ).
#' Then identify the node with the largest number of outputs.
#' @param contact_network (class igraph)  Graph of connections/movements between sites produced with iGraph
#' in '03_CreateContactNetwork.R' of AquaNet-mod.
#' @param imports_exports string input of either 'imports' (in-degree movements) or 'exports'(out-degree movements)
#'
#' @return movements
#' @export
#'
importsExports <- function(contact_network, imports_exports){

  # assign movement as either 'in' or 'out' dependent on import_exports parameter
  if(imports_exports == "imports"){
    trade <- igraph::degree(contact_network,
                            mode = "in")
  } else if (imports_exports == "exports") {
    trade <- igraph::degree(contact_network,
                            mode = "out")
  }

  # summarise movements, identifying the sites with the largest number of exports
  most_trade <- which(trade == max(trade))
  movements <- data.frame(mean = mean(trade),
                          median = median(trade),
                          min_movement = min(trade),
                          max_movement = max(trade),
                          quant_95 = quantile(trade, 0.95),
                          site_with_most_trade = names(most_trade))
  return(movements)
}
