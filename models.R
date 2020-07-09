# ***********************************************************************************
# R File to hold modelling functions
# ***********************************************************************************


# ***********************************************************************************
# ------------------------- Regression Modelling ------------------------------------


# ***************************************************************************
# How 5 different types of pesticides effect the amount of bees
# Return the coefficient number
#
# ***************************************************************************
Regression_lossPesticides<-function(dataset){
  fit_ptb=lm(total_annual_loss_colonies~nCLOTHIANIDIN+nIMIDACLOPRID
             +nTHIAMETHOXAM+nACETAMIPRID+nTHIACLOPRID, data=dataset)
  print(summary(fit_ptb))
  # cofficient number between two fields
  fit_ptb_cof=as.data.frame(coefficients(fit_ptb))
  
  MSE = sum(residuals(fit_ptb)^2)/fit_ptb$df.residual
  RMSE = sqrt(MSE)
  MAE = sum(abs(residuals(fit_ptb)))/fit_ptb$df.residual
  print(sprintf('MSE of the first MLR model is %.2f', MSE))
  print(sprintf('RMSE of the first MLR model is %.2f', RMSE))
  print(sprintf('MAE of the first MLR model is %.2f', MAE))
  print(fit_ptb_cof)
  importance<-as.data.frame(caret::varImp(fit_ptb, scale=FALSE))
  barplot(t(importance[order(importance$Overall),,drop=TRUE]))
  
  par(mfrow=c(1,2))
  plot(fit_ptb,which=c(1,2))
  shapiro.test(residuals(fit_ptb))
}

# *****************************************************
#  The amount of bees are affected by different factors
#
# *****************************************************
Regression_multipleLinear<-function(dataset){
  fit_mlr=lm(bee_colony_numbers~year+state+yield_per_col+total_production+stocks+price_per_lb
             +production_value+region+nCLOTHIANIDIN+nIMIDACLOPRID+nTHIAMETHOXAM
             +nACETAMIPRID+nTHIACLOPRID+nAllNeonic+total_annual_loss+beekeepers+beekeepers_exclusive_to_state+
               colonies+colonies_exclusive_to_state+total_annual_loss_colonies,data=dataset)
  
  print(summary(fit_mlr))
  fit_mlr_cof=as.data.frame(coefficients(fit_mlr))
  MSE = sum(residuals(fit_mlr)^2)/fit_mlr$df.residual
  RMSE = sqrt(MSE)
  MAE = sum(abs(residuals(fit_mlr)))/fit_mlr$df.residual
  print(sprintf('MSE of the first MLR model is %.2f', MSE))
  print(sprintf('RMSE of the first MLR model is %.2f', RMSE))
  print(sprintf('MAE of the first MLR model is %.2f', MAE))
  print(fit_mlr_cof)
  importance<-as.data.frame(caret::varImp(fit_mlr, scale=FALSE))
  barplot(t(importance[order(importance$Overall),,drop=TRUE]))
  
  par(mfrow=c(1,2))
  plot(fit_mlr,which=c(1,2))
  shapiro.test(residuals(fit_mlr))
}

# *******************************************************
# Discuss how the total production of honey and the state
# affect the value of bee production
#
# *******************************************************
Regression_productionValue<-function(dataset){
  fit_pv=lm(price_per_lb~total_production+state+yield_per_col+stocks+total_annual_loss+beekeepers, data=dataset)
  print(summary(fit_pv))
  fit_pv_cof=as.data.frame(coefficients(fit_pv))
  MSE1 = sum(residuals(fit_pv)^2)/fit_pv$df.residual
  RMSE1 = sqrt(MSE1)
  MAE1 = sum(abs(residuals(fit_pv)))/fit_pv$df.residual
  print(sprintf('MSE of the first MLR model is %.2f', MSE1))
  print(sprintf('RMSE of the first MLR model is %.2f', RMSE1))
  print(sprintf('MAE of the first MLR model is %.2f', MAE1))
  print(fit_pv_cof)
  importance<-as.data.frame(caret::varImp(fit_pv, scale=FALSE))
  barplot(t(importance[order(importance$Overall),,drop=TRUE]))
  
  par(mfrow=c(1,2))
  plot(fit_pv,which=c(1,2))
  shapiro.test(residuals(fit_pv))
  
}

# ****************************************************
# Automatically choose the high weight parameters
# that affect bee population
#
# ****************************************************
Regression_stepwisePrediction<-function(dataset) {
  fit_mlr3=lm(bee_colony_numbers~year+state+yield_per_col+total_production+stocks+price_per_lb
              +production_value+region+nCLOTHIANIDIN+nIMIDACLOPRID+nTHIAMETHOXAM
              +nACETAMIPRID+nTHIACLOPRID+nAllNeonic+total_annual_loss+beekeepers+beekeepers_exclusive_to_state+
                colonies+colonies_exclusive_to_state+total_annual_loss_colonies,data=dataset)
  stepAIC(fit_mlr3, direction="backward")
  
  stepwise_model=lm(formula = bee_colony_numbers ~ year+ state + yield_per_col + total_production
                    + price_per_lb + production_value + nTHIAMETHOXAM + nACETAMIPRID + nTHIACLOPRID +
                      total_annual_loss, data = dataset)
  print(summary(stepwise_model))
  fit_mlr_cof3=as.data.frame(coefficients(stepwise_model))
  fit_mlr_cof3
  MSE3 = sum(residuals(stepwise_model)^2)/stepwise_model$df.residual
  RMSE3 = sqrt(MSE3)
  MAE3 = sum(abs(residuals(stepwise_model)))/stepwise_model$df.residual
  print(sprintf('MSE of the Stepwise Regression Model is %.2f', MSE3))
  print(sprintf('RMSE of the Stepwise Regression Model is %.2f', RMSE3))
  print(sprintf('MAE of the Stepwise Regression model is %.2f', MAE3))
  importance<-as.data.frame(caret::varImp(stepwise_model, scale=FALSE))
  barplot(t(importance[order(importance$Overall),,drop=TRUE]))
  
  par(mfrow=c(1,2))
  plot(stepwise_model,which=c(1,2))
  shapiro.test(residuals(stepwise_model))
}


# ********************************************************************************************
# ------------------------ Self Organising Maps Modelling ------------------------------------

  
# Normalising data and matrixing
SOM_unsupervised_dataPrep <- function(data_selection) {
  
  zscale <- apply(data_selection, MARGIN = 2,
                 FUN = function(X) scale(X, center = TRUE, scale = TRUE))
  
  normalised <- scale(zscale[, -1], center = TRUE, scale = TRUE)
  matrix <- as.matrix(scale(normalised))
  
  return (matrix)
}

# SOM grid creation
SOM_createGrid <- function(GRIDSIZE, TOPO_TYPE) {
  
  som_grid <- kohonen::somgrid(xdim= GRIDSIZE, ydim = GRIDSIZE, topo = TOPO_TYPE)
  return(som_grid)
}

# SOM model creation
SOM_createModel <- function(matrix, GRIDSIZE, TOPO_TYPE, EPOCHS, LEARN_RATE,
                            RADIUS, KEEP_DATA, DISTANCE_FUNC) {
  
  model <- som(matrix,
               grid = SOM_createGrid(GRIDSIZE, TOPO_TYPE),
               rlen = EPOCHS,
               alpha = LEARN_RATE,
               radius = RADIUS,
               keep.data = KEEP_DATA,
               dist.fcts = DISTANCE_FUNC)
  return(model)
}

# Plotting som evaluation models
SOM_evaluate <- function(model, eval_type, title = "") {
  plot(model, type = eval_type, main = title)
}

# Plotting som models for individual variables
SOM_unsupervisedMain <- function(model, som_codes, field_num) {
  plot(model,
       type = "property",
       property = som_codes[,field_num],
       main = paste(names(data.frame(model$data))[field_num]))
}


# *****************************************************************************
# --------------------------------- END ---------------------------------------
# *****************************************************************************
