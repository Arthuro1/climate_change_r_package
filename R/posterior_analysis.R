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


#' Analyse the posterior
#'
#' @param formula Must be of type formula.
#' @param climateset Must be a dataframe.
#' @param verbose A boolean set per default to FALSE
#' @param conterfactuals.check A boolean set per default to FALSE
#' @return The posterior.
#' @examples
#' posterior_analysis(1, 1)
#' posterior_analysis(10, 1)
posterior_analysis <- function(formula, climateset, verbose = FALSE, conterfactuals.check = FALSE){


  if(!inherits(climateset, "data.frame"))
    stop("data should be of type data.frame")

  posterior = stan_glm(formula , data = climateset, verbose = verbose)

  if(verbose){
    cat("\n\n\n***********posterior info summary************\n\n")

    print(summary(posterior))
  }

  #obtain the dependent variable
  dependent = trimws(strsplit(deparse(heatData_us ~ co2Data_us), split = "~")[[1]][1])

  model_check(dependent, climateset, mu = posterior$stan_summary[,1][4],
                         sigma = posterior$stan_summary[,1][3], prior = FALSE)


  if(conterfactuals.check)
    conterfactuals_check(posterior)

  mcmc_pairs(posterior)

  return(posterior)
}


#' Checks counterfactuals and plot the results.
#' Only to check one to one relationship  A -> B.
#'
#' @param posterior A number.
#' @examples
#' conterfactuals_check(1)
#' conterfactuals_check(10)
conterfactuals_check <- function(posterior){

  if(!inherits(posterior, "stanreg"))
    stop("object not of class stanreg")

  data.size =  length(posterior$fitted.values)

  regressor_seq = seq(from = -2, to = 2, length.out = data.size)

  coefs = coef(posterior)

  dependent_sim = sapply(1:data.size, function(i){
    rnorm(1e3,
          coefs[1] + coefs[2]*regressor_seq[i], posterior$ses[2])
  })

  dependent_post_mean = apply(dependent_sim, MARGIN = 2, mean)

  plot(regressor_seq, dependent_post_mean, type = "l")
}

#' Make forecast on new data based on the posterior.
#'
#' @param posterior A number.
#' @param new_data Must be a dataframe.
#' @return The prediction.
#' @examples
#' make_forecast(1, 1)
#' make_forecast(10, 1)
make_forecast <- function(posterior, new_data){

  prediction = predict(posterior, newdata = new_data)

  return(prediction)
}
