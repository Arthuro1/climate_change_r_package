#' Check the model and display the results.
#' If the data values are positive, the logarithm function will be apply on these values.
#'
#' @param feature.names  Character vector of feature's names.
#' @param climateset Dataframe of climate data.
#' @param mu Mean parameter of normal distribution. Set per default to 0.
#' @param sigma Dispersion parameter of normal distribution. Set per default to 1.
#' @param prior A Boolean TRUE(Default) means prior_predictive_check is called, else Posterior_predictive_check.
#' @examples
#' model_check(c("temperature_us"), climateset, prior = FALSE)
model_check <- function(feature.names, climateset, mu = 0, sigma = 1, prior = TRUE){

  sample.size = 3000

  dataset.size = nrow(climateset)

  sapply(feature.names, function(feature.name, climateset, sample.size, dataset.size){

    data = climateset[,feature.name]

    #if( all( data >= 0) )
    # data = log(data + 0.01)

    #if( all( data < 0) )
    #  data = log(-data)

    #if(prior){
      ####TODO: scale the variables at the beginning
      #data = as.numeric(scale(data))
      #data = as.numeric(data)
    #}

    if(!prior){
      mu = mean(data)
      sigma = sd(data)
    }

    #Distribution sample
    yrep = matrix(rnorm(sample.size * dataset.size, mean = mu, sd = sigma),
                  nrow = sample.size,
                  ncol = dataset.size
                  )

    ppc_display(data, yrep)

    #feature_density(feature.names)
  }, climateset, sample.size, dataset.size)

  return(invisible(NULL))
}
