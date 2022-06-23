#####################################################################################################
################################# 02_Interaction_uncertainty ########################################
############ Calculating stressor effect scores accounting for interaction uncertainty ##############
#####################################################################################################

library(tidyverse)
library(raster)
library(terra)

## Import effect scores
ES_data <- read_csv("ES_data.csv")
ES_data_add <- read_csv("ES_data_add.csv")

## Import stressor layers
Stressor_list <- list.files(pattern = ".asc")
Stressor_stack <- stack(Stressor_list)
names(Stressor_stack) <- c("Acid", "Hab_mod", "High_sal", "Hypoxia", "Light", 
                           "Low_sal", "Poll_met", "Poll_nut", "Temp")


## Additive CIA

rand_add <- data.frame(matrix(nrow = 1000, ncol = 15))
colnames(rand_add) <- c("A_SMD", "HM_SMD", "HS_SMD", "HX_SMD", "L_SMD", "LS_SMD", 
                        "MET_SMD", "NUT_SMD", "TEMP_SMD")
for (i in 1:1000){
  for (j in 1:15){
    # Mean SMD, lwr CI and upr CI are in columns 3,4 and 5 of ES_data_add
    rand_add[i,j] <- rnorm(1000, as.numeric(ES_data_add[j,3]), 
                           as.numeric((ES_data_add[j,5] - ES_data_add[j,4])/2/1.96))[j]
  }
}


# Generate 1000 CIA maps

r_CI_add <- vector(mode = "list", length = 1000)
for (i in 1:1000){ # 1000 iterations of a1S1
  
  r_CI_main[i] <- Stressor_stack$Acid*rand_add$A_SMD[i] + 
    Stressor_stack$Hab_mod*rand_add$HM_SMD[i] + 
    Stressor_stack$High_sal*rand_add$HS_SMD[i] + 
    Stressor_stack$Light*rand_add$L_SMD[i] + 
    Stressor_stack$Hypoxia*rand_add$HX_SMD[i] + 
    Stressor_stack$Low_sal*rand_add$LS_SMD[i] + 
    Stressor_stack$Poll_met*rand_add$MET_SMD[i] + 
    Stressor_stack$Poll_nut*rand_add$NUT_SMD[i] + 
    Stressor_stack$Temp*rand_add$TEMP_SMD[i] 
}


## Non-additive CIA

rand <- data.frame(matrix(nrow = 1000, ncol = 15))
colnames(rand) <- c("A_LS_SMD", "HM_L_SMD", "HS_HX_SMD", "L_HX_SMD", "L_LS_SMD", "L_NUT_SMD", 
                    "MET_A_SMD", "NUT_HS_SMD", "NUT_LS_SMD", "TEMP_HS_SMD", "TEMP_HX_SMD", 
                    "TEMP_L_SMD", "TEMP_LS_SMD", "TEMP_MET_SMD", "TEMP_NUT_SMD")
for (i in 1:1000){
  for (j in 1:15){
    
    # Define a normal distribution from which to sample SMD scores for each stressor combination
    # Mean SMD, lwr CI and upr CI are in columns 3,4 and 5 of ES_data
  rand[i,j] <- rnorm(1000, as.numeric(ES_data[j,3]), as.numeric((ES_data[j,5] - ES_data[j,4])/2/1.96))[j]
  }
}


# Generate 1000 CIA maps

r_CI <- vector(mode = "list", length = 1000)
for (i in 1:1000){ 
  
  r_CI[i] <- Stressor_stack$Acid*Stressor_stack$Low_sal*rand$A_LS_SMD[i] + 
    Stressor_stack$Hab_mod*Stressor_stack$Light*rand$HM_L_SMD[i] + 
    Stressor_stack$High_sal*Stressor_stack$Hypoxia*rand$HS_HX_SMD[i] + 
    Stressor_stack$Light*Stressor_stack$Poll_nut*rand$L_NUT_SMD[i] + 
    Stressor_stack$Light*Stressor_stack$Hypoxia*rand$L_HX_SMD[i] + 
    Stressor_stack$Light*Stressor_stack$Low_sal*rand$L_LS_SMD[i] + 
    Stressor_stack$Poll_met*Stressor_stack$Acid*rand$MET_A_SMD[i] + 
    Stressor_stack$Poll_nut*Stressor_stack$High_sal*rand$NUT_HS_SMD[i] + 
    Stressor_stack$Poll_nut*Stressor_stack$Low_sal*rand$NUT_LS_SMD[i] + 
    Stressor_stack$Temp*Stressor_stack$High_sal*rand$TEMP_HS_SMD[i] + 
    Stressor_stack$Temp*Stressor_stack$Light*rand$TEMP_L_SMD[i] + 
    Stressor_stack$Temp*Stressor_stack$Poll_met*rand$TEMP_MET_SMD[i] + 
    Stressor_stack$Temp*Stressor_stack$Poll_nut*rand$TEMP_NUT_SMD[i] + 
    Stressor_stack$Temp*Stressor_stack$Hypoxia*rand$TEMP_HX_SMD[i] + 
    Stressor_stack$Temp*Stressor_stack$Low_sal*rand$TEMP_LS_SMD[i]
  
}


## Calculate 0.025 and 0.975 quantiles of the CI maps

qf <- function(x){
  quantile(x, c(0.025, 0.5, .975), na.rm=TRUE)
}
 
quantiles_raster <- calc(r_CI, fun=qf)
freq(sign(quantiles_raster$layer.1) == sign(quantiles_raster$layer.3))



## Establish which quantiles of each stressor interaction does not contain 0
## indicating a significant deviation from additive

# Acidification*Hyposalinity

r_CI_A.LS <- vector(mode = "list", length = 1000)
for (i in 1:1000){ # 1000 iterations of a1S1
  
  r_CI_A.LS[i] <- Acid*Low_sal*rand_SMD_acid.sal[i]

  
}

r_CI_A.LS <- stack(r_CI_A.LS)
quantiles_A.LS <- calc(r_CI_A.LS, fun=qf)


# Habitat_modification*Reduced_light

r_CI_HM.L <- vector(mode = "list", length = 1000)
for (i in 601:1000){ # 1000 iterations of a1S1
  
  r_CI_HM.L[i] <- Hab_mod*Light*rand_SMD_hm.light[i]
  
  
}

r_CI_HM.L <- stack(r_CI_HM.L)
quantiles_HM.L <- calc(r_CI_HM.L, fun=qf)


# Hypersalinity*Hypoxia

r_CI_HS.HX <- vector(mode = "list", length = 1000)
for (i in 1:1000){ # 1000 iterations of a1S1
  
  r_CI_HS.HX[i] <- High_sal*Hypoxia*rand_SMD_hsal.oxy[i]
  
  
}

r_CI_HS.HX <- stack(r_CI_HS.HX)
quantiles_HS.HX <- calc(r_CI_HS.HX, fun=qf)


# Reduced_light*Pollution:nutrients

r_CI_L.NUT <- vector(mode = "list", length = 1000)
for (i in 1:1000){ # 1000 iterations of a1S1
  
  r_CI_L.NUT[i] <- light*Poll_nut*rand_SMD_light.nut[i]
  
  
}

r_CI_L.NUT <- stack(r_CI_L.NUT)
quantiles_L.NUT <- calc(r_CI_L.NUT, fun=qf)


# Reduced_light*Hypoxia

r_CI_L.HX <- vector(mode = "list", length = 200)
for (i in 891:1000){ # 1000 iterations of a1S1
  
  r_CI_L.HX[i] <- Light*Hypoxia*rand_SMD_light.oxy[i]
  
  
}

r_CI_L.HX <- stack(r_CI_L.HX)
quantiles_L.HX <- calc(r_CI_L.HX, fun=qf)


# Reduced_light*Hyposalinity

r_CI_L.LS <- vector(mode = "list", length = 1000)
for (i in 890:1000){ # 1000 iterations of a1S1
  
  r_CI_L.LS[i] <- Light*Low_sal*rand_SMD_light.sal[i]
  
  
}

r_CI_L.LS <- stack(r_CI_L.LS)
quantiles_L.LS <- calc(r_CI_L.LS, fun=qf)


# Pollution:metals*Acidification

r_CI_MET.A <- vector(mode = "list", length = 1000)
for (i in 890:1000){ # 1000 iterations of a1S1
  
  r_CI_MET.A[i] <- Poll_met*Acid*rand_SMD_met.acid[i]
  
  
}

r_CI_MET.A <- stack(r_CI_MET.A)
quantiles_MET.A <- calc(r_CI_MET.A, fun=qf)


# Pollution:nutrients*Hypersalinity

r_CI_NUT.HS <- vector(mode = "list", length = 1000)
for (i in 589:1000){ # 1000 iterations of a1S1
  
  r_CI_NUT.HS[i] <- Poll_nut*High_sal*rand_SMD_nut.hsal[i]
  
  
}

r_CI_NUT.HS <- stack(r_CI_NUT.HS)
quantiles_NUT.HS <- calc(r_CI_NUT.HS, fun=qf)


# Pollution:nutrients*Hyposalinity

r_CI_NUT.LS <- vector(mode = "list", length = 1000)
for (i in 889:1000){ # 1000 iterations of a1S1
  
  r_CI_NUT.LS[i] <- Poll_nut*Low_sal*rand_SMD_nut.sal[i]
  
  
}

r_CI_NUT.LS <- stack(r_CI_NUT.LS)
quantiles_NUT.LS <- calc(r_CI_NUT.LS, fun=qf)


# Increased_temperature*Hypersalinity

r_CI_T.HS <- vector(mode = "list", length = 1000)
for (i in 1:1000){ # 1000 iterations of a1S1
  
  r_CI_T.HS[i] <- Temp*High_sal*rand_SMD_temp.hsal[i]
  
  
}

r_CI_T.HS <- stack(r_CI_T.HS)
quantiles_T.HS <- calc(r_CI_T.HS, fun=qf)


# Increased_temperature*Reduced_light

r_CI_T.L <- vector(mode = "list", length = 1000)
for (i in 1:1000){ # 1000 iterations of a1S1
  
  r_CI_T.L[i] <- Temp*Light*rand_SMD_temp.light[i]
  
  
}

r_CI_T.L <- stack(r_CI_T.L)
quantiles_T.L <- calc(r_CI_T.L, fun=qf)


# Increased_temperature*Pollution

r_CI_T.MET <- vector(mode = "list", length = 1000)
for (i in 1:1000){ # 1000 iterations of a1S1
  
  r_CI_T.MET[i] <- Temp*Poll_met*rand_SMD_temp.met[i]
  
  
}

r_CI_T.MET <- stack(r_CI_T.MET)
quantiles_T.MET <- calc(r_CI_T.MET, fun=qf)


# Increased_temperature*Pollution:nutrients

r_CI_T.NUT <- vector(mode = "list", length = 1000)
for (i in 1:1000){ # 1000 iterations of a1S1
  
  r_CI_T.NUT[i] <- Temp*Poll_nut*rand_SMD_temp.nut[i]
  
  
}

r_CI_T.NUT <- stack(r_CI_T.NUT)
quantiles_T.NUT <- calc(r_CI_T.NUT, fun=qf)


# Increased_temperature*Hypoxia

r_CI_T.HX <- vector(mode = "list", length = 1000)
for (i in 1:1000){ # 1000 iterations of a1S1
  
  r_CI_T.HX[i] <- Temp*Hypoxia*rand_SMD_temp.oxy[i]
  
  
}

r_CI_T.HX <- stack(r_CI_T.HX)
quantiles_T.HX <- calc(r_CI_T.HX, fun=qf)


# Increased_temperature*Hyposalinity

r_CI_T.LS <- vector(mode = "list", length = 1000)
for (i in 1:1000){ # 1000 iterations of a1S1
  
  r_CI_T.LS[i] <- Temp*Low_sal*rand_SMD_temp.sal[i]
  
  
}

r_CI_T.LS <- stack(r_CI_T.LS)
quantiles_T.LS <- calc(r_CI_T.LS, fun=qf)

#``````````````````````````````````````````````````````````````````````````````````````
#```````````````THE FOLLOWING WAS RUN ON ALL STRESSOR PAIR QUANTILES``````````````````#

# Set areas of 0 to NA to avoind over-inflating significance (i.e. sign(0) == sign(0))
quantiles_T.LS[quantiles_T.LS == 0] <- NA

# Generate raster of 1's and 0's where 1 indicates significance (quantiles do not contain 0),
# and 0 indicates non-significance (quantiles contain 0)
sig.interactions <- sign(quantiles_T.LS$upr) == sign(quantiles_T.LS$lwr)














