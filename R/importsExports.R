#' Imports and exports
#' Speficy if connection between nodes is imports or exports-imports represent in-degree connections (the number of incoming connections), outputs represents the number of out-degree connections (the number of outgoing connections ). Then calculate summary stats of the movements
#' @param contact_network network simulates and tracks disease transmission from infected to susceptible contacts
#' @param imports_exports string input- either 'imports' or 'exports'
#'
#' @return movements
#' @export
#'
importsExports <- function(contact_network, imports_exports){
  if(imports_exports == "imports"){
    trade <- igraph::degree(contact_network,
                            mode = "in")
  } else if (imports_exports == "exports") {
    trade <- igraph::degree(contact_network,
                            mode = "out")
  }
  most_trade <- which(trade == max(trade))
  movements <- data.frame(mean = mean(trade),
                          median = median(trade),
                          min_movement = min(trade),
                          max_movement = max(trade),
                          quant_95 = quantile(trade, 0.95),
                          site_with_most_trade = names(most_trade))
  return(movements)
}
