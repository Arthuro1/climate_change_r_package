# Normal regression model 3
# -> Formulate posterior
# -> Sample from posterior
# -> Check convergence
# -> Posterior predictive checks

# -> Inference
# -> do cross-validation
# -> test some other models
# -> write the main function
# -> integrate new datasets

library(rstanarm)
library(bayesplot)

#' Analyse the posterior.
#'
#' @param formula Must be parsable into an object of type formula.
#' @param climateset Dataframe of climate data.
#' @return The posterior.
#' @examples
#' posterior_analysis(temperature_us ~ co2_emission_us , climateset)
posterior_analysis <- function(formula, climateset){


  if(!inherits(climateset, "data.frame"))
    stop("data should be of type data.frame")

  posterior = stan_glm(formula , data = climateset)

  #Print convergence(R_hat)
  cat("\n\n\n***********posterior info summary************\n\n")
  print(summary(posterior))


  #obtain the dependent variable
  dependent = trimws(strsplit(deparse(heatData_us ~ co2Data_us), split = "~")[[1]][1])

  #display ppc
  model_check(dependent, climateset, mu = posterior$stan_summary[,1][4],
                         sigma = posterior$stan_summary[,1][3], prior = FALSE)

  #plot parameter's posterior and their relationships
  mcmc_pairs(posterior)

  return(posterior)
}


#' Checks counterfactuals and plot the results.
#' Only for simple linear models (y = ax + b)
#'
#' @param posterior Object of type stanreg.
conterfactuals_check <- function(posterior){

  if(!inherits(posterior, "stanreg"))
    stop("object not of class stanreg")

  data.size =  length(posterior$fitted.values)

  #create sequence
  #95% of the values generated in normal distribution are between -2 and 2
  regressor_seq = seq(from = -2, to = 2, length.out = data.size)

  #retrieve alpha and beta coefficient from the posterior
  coefs = coef(posterior)

  #Generate number
  dependent_sim = sapply(1:data.size, function(i){
    rnorm(1e3,
          coefs[1] + coefs[2]*regressor_seq[i], posterior$ses[2])
  })

  #calculate the mean of each columns of the matrix (dependent_sim)
  dependent_post_mean = apply(dependent_sim, MARGIN = 2, mean)

  plot(regressor_seq, dependent_post_mean, type = "l")
}

#' Make forecast on new data based on the posterior.
#'
#' @param posterior Object of type stanreg.
#' @param new_data Must be a dataframe.
#' @return The prediction.
make_forecast <- function(posterior, new_data){

  prediction = predict(posterior, newdata = new_data)

  return(prediction)
}
