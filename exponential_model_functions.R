# DESCRIPTION OF THE FUNCTION

# FUNCTION - Exponential model
#########################################
# Creating function for exponential fit

title_size <- 12
text_size <- 10

# Example usage:
# mod <- myexpfit("Yield_Mg.ha", ind_var, wsub_dat)
myexpfit <- function(response_var, pred_var, mydata){
  # print(paste0("Model = ", "log(",response_var, ")~", pred_var))
  model.linear <- lm(paste0("log(",response_var, ")~", pred_var), data = mydata)
  a1 <- exp(model.linear$coefficients[1])
  b1 <- (model.linear$coefficients[2])
  R2 <- summary(model.linear)$r.squared
  
  # RMSE calculation
  mydata$PredictedYield <- a1*exp(b1*mydata[[pred_var]])
  mydata$Error <- mydata[["PredictedYield"]] - mydata[[response_var]]
  rmse_val <- sqrt(mean((mydata$Error)^2))
  
  # NRMSE calculation
  avg_yield <- mean(mydata[[response_var]])
  nrmse <- (rmse_val/avg_yield) * 100
  myresults <- c(as.numeric(a1), as.numeric(b1), as.numeric(R2), 
                 as.numeric(rmse_val), as.numeric(nrmse))
  
  return(myresults)
}

#########################################
# Example usage:
# mod <- getRMSE(mod_SI, "Yield.Mg.ha", "NDVI",  subdat)
getRMSE <- function(model_results, response_var, pred_var, mydata){
  
  a1 <- as.numeric(model_results[1])
  b1 <- as.numeric(model_results[2])
  
  #RMSE calculation
  mydata$PredictedYield <- a1*exp(b1*mydata[[pred_var]])
  mydata$Error <- mydata[["PredictedYield"]] - mydata[[response_var]]
  rmse_val <- sqrt(mean((mydata$Error)^2))
  
  # NRMSE calculation
  avg_yield <- mean(mydata[[response_var]])
  nrmse <- (rmse_val/avg_yield) * 100
  
  # MAE calculation
  mydata$absError <- abs(mydata$Error)
  mae <- mean(mydata$absError)
  
  myresults <- c(as.numeric(rmse_val), as.numeric(nrmse), as.numeric(mae))
  
  return(myresults)
}

#########################################

plot_fitted_data <- function(model_results, response_var, pred_var, mydata) {
  
  mydata$PredictedYield <- model_results[1] * exp(model_results[2]*mydata[[pred_var]])
  
  exp_plot <- ggplot(mydata, aes(x = mydata[[pred_var]], y = mydata[[response_var]])) +
    geom_point(size = 2) + 
    theme_bw() + 
    geom_line(data = mydata, aes(x = unlist(mydata[[pred_var]]), 
                                 y = PredictedYield, 
                                 color = "red"), size = 2) + 
    xlab(pred_var) + 
    ylab(response_var) + 
    theme(legend.position = "none", 
          axis.title = element_text(size = title_size, face = "bold"), 
          axis.text = element_text(size = text_size)) 
  
  return(exp_plot)
}

