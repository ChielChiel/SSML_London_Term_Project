easypackages::packages ("sf", "sp", "spdep", "spatialreg", "GWmodel", "tmap", "mapview", "car", "RColorBrewer", "cowplot", "leafsync", "leaflet.extras2", "mapview", "tidyverse", "readr", "lmtest", "tseries")

setwd(this.path::here())

# data
ward_wellbeing <- read_sf("data/ward_wellbeing.gpkg")


########################## equations ####################################
equation_spatial <- SubWellBeing ~ LifeExp + ChildObesity + IncBenefitRate + UnempRate + 
  CrimeRate + DelibFires + GCSEScores + UnauthAbsence + PubTransAccess + 
  HomesOpenAccessNatGreen + BrownfieldProp + RailNoise + RoadNoise + 
  PM25

equation_non_spatial <- SubWellBeing ~ LifeExp + ChildObesity + IncBenefitRate + UnempRate + 
  CrimeRate + DelibFires + GCSEScores + UnauthAbsence

equations <- list(equation_non_spatial, equation_spatial)


################# distance matrix ##############################
neighbourhood <- poly2nb(ward_wellbeing, queen=TRUE)
neighbourhood_weights <- nb2listw(neighbourhood, style="W", zero.policy = TRUE)



############## step 1, OLS ###############################



step1 <- function(equation){
  model <- lm(equation, data = ward_wellbeing)
  residuals <- model$residuals
  AIC <- AIC(model)
  lagrange_decision <- NA
  
  mc_global <- moran.mc(residuals, neighbourhood_weights, 2999, zero.policy= TRUE, alternative="greater")$statistic
  summary.table <- data.frame(lagrange_decision, AIC, mc_global)

}






###################### step 2, semi-local spatial models ########################

step2 <- function(equation){
  model <- lm(equation, data = ward_wellbeing)
  
  lagrange <- lm.RStests(model, neighbourhood_weights, test="all")
  
  if (lagrange$RSerr$p.value <= 0.05 & lagrange$RSlag$p.value <= 0.05){
    lagrange_decision <- "SAC"
    model <- sacsarlm(equation,
                        data = ward_wellbeing, 
                        listw = neighbourhood_weights)
    
  } else if (lagrange$RSerr$p.value <= 0.05 & lagrange$RSlag$p.value > 0.05) {
    lagrange_decision <- "spatial error"
    model <- errorsarlm(equation,
                        data = ward_wellbeing, 
                        listw = neighbourhood_weights)
    
  } else if (lagrange$RSerr$p.value > 0.05 & lagrange$RSlag$p.value <= 0.05) {
    lagrange_decision <- "spatial lag"
    model <- lagsarlm(equation,
                        data = ward_wellbeing, 
                        listw = neighbourhood_weights, 
                        zero.policy = TRUE)
    
  } else {lagrange_decision <- "OLS"
  }
  
  residuals <- model$residuals
  AIC <- AIC(model)
  
  mc_global <- moran.mc(residuals, neighbourhood_weights, 2999, zero.policy= TRUE, alternative="greater")$statistic
  summary.table <- data.frame(lagrange_decision, AIC, mc_global)
  
}



############################## step 3, MGWR #######################################

ward_wellbeing_sp <- as_Spatial(ward_wellbeing)

step3 <- function(equation){
  model <- gwr.multiscale(
    equation,
    adaptive = TRUE,
    kernel = "gaussian",
    data = ward_wellbeing_sp,
    verbose = TRUE)
  

  residuals <- model$SDF$residual
  AIC <- model$GW.diagnostic$AIC
  lagrange_decision <- NA
  
  mc_global <- moran.mc(residuals, neighbourhood_weights, 2999, zero.policy= TRUE, alternative="greater")$statistic
  summary.table <- data.frame(lagrange_decision, AIC, mc_global)
  
}

############################## adding it all together ###############################

one <- t(sapply(equations, step1))
two <- t(sapply(equations, step2))
three <- t(sapply(equations, step3))

summary.data <- rbind(one, two, three)

equation <- rep(c("non spatial equation", "spatial equation"), length.out = 6)
step <- rep(c(1,2,3), each = 2)

full.summary.table <- cbind(step, equation, summary.data)
