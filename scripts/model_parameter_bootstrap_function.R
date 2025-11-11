#function to bootstrap parameter confidence intervals from glmer models

#works by simulating data from the model, and the refitting the model with the simulated data and recording the parameters

boot_param_CI <- function(nsim, model, data){
  
  #bootstrapping parameter values from model simulations
  betas <- matrix(NA, nrow = nsim,
                           ncol = length(fixef(model)))
  
  for(j in 1:nsim){
    sim_data <- data %>% 
      mutate(y = unlist(simulate(model)))  #simulate response variables from the model
    
    sim_model <- update(model, y ~ ., data = sim_data) #rerun the model with the simulated values as the response
    
    if(is.null(summary(sim_model)$optinfo$conv$lme4$messages) == TRUE){ #if there are no warning messages (model fit fine) then record the parameters
      betas[j,] <- fixef(sim_model)
    }
  }
  
  
  #calculating the confidence intervals for different model types
  if(model@call$family == "Poisson"){  beta_bs <- data.frame(FE = names(fixef(model)),
                                                             coef = exp(fixef(model)),
                                                             lower = exp(apply(betas, 2, function(x) quantile(x, probs = 0.025, na.rm = T))), #CI lower bound
                                                             upper = exp(apply(betas, 2, function(x) quantile(x, probs = 0.975, na.rm = T)))) #CI upper bound
  
  #plotting model coefficients
  (beta_plot <- beta_bs %>% 
      filter(FE != "(Intercept)") %>% 
      ggplot + 
      geom_point(aes(x = coef, y = FE), colour = "#00BFC4") +
      geom_segment(aes(x = lower, xend = upper, y = FE, yend = FE), colour = "#00BFC4") +
      geom_vline(xintercept = 0, lty = "dashed") +
      geom_text(aes(x = coef, y = FE, label = round(coef, 2),
                    vjust = -.6, hjust = .3), size = 3) +
      theme(legend.position = "none") +
      labs(title = paste0("fixed effects (iterations = ", nsim, ")"),
           y = "",
           x = "exp(\u03b2)"))
  }
  if(model@call$family == "binomial"){  beta_bs <- data.frame(FE = names(fixef(model)),
                                                             coef = boot::inv.logit(fixef(model)),
                                                             lower = boot::inv.logit(apply(betas, 2, function(x) quantile(x, probs = 0.025, na.rm = T))), #CI lower bound
                                                             upper = boot::inv.logit(apply(betas, 2, function(x) quantile(x, probs = 0.975, na.rm = T)))) #CI upper bound
  
  
  #plotting model coefficients
  (beta_plot <- beta_bs %>% 
      filter(FE != "(Intercept)") %>% 
      ggplot + 
      geom_point(aes(x = coef, y = FE), colour = "#00BFC4") +
      geom_segment(aes(x = lower, xend = upper, y = FE, yend = FE), colour = "#00BFC4") +
      geom_vline(xintercept = 0.5, lty = "dashed") +
      geom_text(aes(x = coef, y = FE, label = round(coef, 2),
                    vjust = -.6, hjust = .3), size = 3) +
      theme(legend.position = "none") +
      labs(title = paste0("fixed effects (iterations = ", nsim, ")"),
           y = "",
           x = "inv.logit(\u03b2)"))
  }
  

  
  return(list(betas, beta_bs, beta_plot))
}

