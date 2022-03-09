#prior: boolean , TRUE: prior_predictive_check, else Posterior_predictive_check
model_check <- function(feature.names, climateset, mu = 0, sigma = 1, prior = TRUE){

  sample.size = 3000

  dataset.size = nrow(climateset)

  sapply(feature.names, function(col, climateset, sample.size, dataset.size){


    data = climateset[,col]


    if( all( data >= 0) )
      data = log(data + 0.01)

    if( all( data < 0) )
      data = log(-data)

    if(prior){
      ####TODO: scale the variables at the beginning
      data = as.numeric(scale(data))
    }


    if(!prior){
      mu = mean(data)

      sigma = sd(data)
    }


    yrep = matrix(rnorm(sample.size * dataset.size, mean = mu, sd = sigma),
                  nrow = sample.size,
                  ncol = dataset.size
                  )

    ppc_display(data, yrep)

    #feature_density(feature.names)
  }, climateset, sample.size, dataset.size)

  return(invisible(NULL))
}
