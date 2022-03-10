library("bayesplot")

#' Create histograms of feature's names contained in the dataset.
#'
#' @param feature.names  Charachter vector of feature's names.
#' @param climateset Dataframe of climate data.
#' @examples
#' feature_hist(c("temperature_us"), climateset)
feature_hist <- function(feature.names, climateset){

  sapply(feature.names, function(x, y){
                  hist(y[,x], main = paste0("Histogram, ", x))
              },
         y = climateset)

  return(invisible(NULL))
}


#' Plot a density graph of feature's names contained in the dataset.
#'
#' @param feature.names Charachter vector of feature's names.
#' @param climateset Dataframe of climate data.
#' @examples
#' feature_density(c("temperature_us"), climateset)
feature_density <- function(feature.names, climateset){

  sapply(feature.names, function(x, y){
                  plot(density(y[,x]), main = paste0("density ", x))
              },
         y = climateset)

  return(invisible(NULL))
}

#' Visual representation of the posterior predictive density.
#'
#' @param data Target variable of numeric type.
#' @param yrep Matrix of data sampled from the posterior distribution.
ppc_display <- function(data, yrep){

  g_plot = ppc_dens_overlay(data, yrep[1:100,])

  plot(g_plot, main = paste0("posterior_predictive_check_", col))

}

