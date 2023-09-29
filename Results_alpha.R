#Code to check for latent factors bias 

#Factors simulation study
samplesize <- c(250,1000)
categories <- c(5)
scale <- c('balanced','semi-balanced','unbalanced')
nitems <- c(12, 24)
Nfac <- c(1,2)
ARS <- c(0,.3,.6)
DIFARS <- c(F,T)

Simulation <- expand.grid(sample = samplesize, 
                          scale = scale, 
                          j = nitems, 
                          c = categories,
                          ARS = ARS,
                          Nfac = Nfac,
                          DIFARS = DIFARS
)

simdes <- cbind.data.frame(seed=c((200802):(200801+nrow(Simulation))),Simulation) 

simdes <- subset(simdes, simdes$DIFARS == T)
simdes <- simdes[,-c(8)]

Bias <- NA
MSE <- NA

BiasARS <- NA
MSEARS <- NA

#Bias results tables
Results.bias <- cbind(simdes, Bias, MSE, BiasARS, MSEARS
)

Results.bias_unequal <- cbind(simdes, Bias, MSE, BiasARS, MSEARS
)

#Run loop to extract results for equal and unequal ARS
setwd("C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Results_revision/")
for(k in c(1:72)){  
load(paste0("ResultsSimScale_",k,".RData"))


#  rem <- NA
#  Scale_level.models_temp <- list()
#    for(i in 1:length(Scale_level.models)){
#    if(all(Scale_level.models[[i]]$warning == 'none')){
#    rem[i] <- i      
#    Scale_level.models_temp[[i]] <- Scale_level.models[[i]]$value
#     }
#  }
  
#if(length(which(is.na(rem))) == 0){
#    Scale_level.models <- Scale_level.models_temp
#} else {
#  Scale_level.models_temp <- Scale_level.models_temp[-c(which(is.na(rem)))]
#  Scale_level.models <- Scale_level.models_temp
#}


if(simdes$Nfac[k] == 1){
error <- NA
errorARS <- NA
for(i in 1:100){

if(is.numeric(Scale_level.models[[i]]$value$alphas$Int.fit[[2]])){
error[i] = Scale_level.models[[i]]$value$alphas$Int.fit[[2]][1:simdes$Nfac[k]]
} else { 
error[i] = NA
}

#if(any(Scale_level.models[[i]]$value$Comparative.fit$ARSModels[3] != 'none' ,
# is.null(Scale_level.models[[i]]$value$Comparative.fit$ARSModels[3]))){
#errorARS[i] = NA
#} else {
#errorARS[i] = Scale_level.models[[i]]$value$alphas$Int.fitARS[[2]][1:simdes$Nfac[k]]
#}

if(is.double(Scale_level.models[[i]]$value$fit.measures$Int.fitARS)){
errorARS[i] = Scale_level.models[[i]]$value$alphas$Int.fitARS[[2]][1:simdes$Nfac[k]]
} else {
errorARS[i] = NA
}


Results.bias[k,]$Bias <- round(mean(error, na.rm = T),3)
Results.bias[k,]$MSE <- round(mean(error^2, na.rm = T),3)
Results.bias[k,]$BiasARS <- paste0(round(mean(errorARS[which(errorARS > -2.6 & errorARS < 2.6)], na.rm = T),3),                                         " (",
                                           length(errorARS[which(errorARS > -2.6 & errorARS < 2.6)])/length(errorARS)*100,"%)")
Results.bias[k,]$MSEARS <- round(mean(errorARS^2, na.rm = T),3)
}


} else {
error <- matrix(NA, nrow = 100, ncol =2)
errorARS <- matrix(NA, nrow = 100, ncol =2)

for(i in 1:100){
if(is.numeric(Scale_level.models[[i]]$value$alphas$Int.fit[[2]])){
error[i,] = Scale_level.models[[i]]$value$alphas$Int.fit[[2]][1:simdes$Nfac[k]]
} else { 
error[i,] = rep(NA,2)
}

#if(any(Scale_level.models[[i]]$value$Comparative.fit$ARSModels[3] != 'none' ,
# is.null(Scale_level.models[[i]]$value$Comparative.fit$ARSModels[3]))){
#errorARS[i,] = rep(NA,2)
#} else {
#errorARS[i,] = Scale_level.models[[i]]$value$alphas$Int.fitARS[[2]][1:simdes$Nfac[k]]
#}

if(is.double(Scale_level.models[[i]]$value$fit.measures$Int.fitARS)){
errorARS[i,] = Scale_level.models[[i]]$value$alphas$Int.fitARS[[2]][1:simdes$Nfac[k]]
} else {
errorARS[i,] = rep(NA,2)
}


}

Results.bias[k,]$Bias <- round(mean(rowMeans(error), na.rm = T),3)
Results.bias[k,]$MSE <- round(mean(rowMeans(error^2), na.rm = T),3)
Results.bias[k,]$BiasARS <- paste0(round(mean(errorARS[which(errorARS > -2.6 & errorARS < 2.6)], na.rm = T),3),                                         " (",
                                           length(errorARS[which(errorARS > -2.6 & errorARS < 2.6)])/length(errorARS)*100,"%)")
Results.bias[k,]$MSEARS <- round(mean(rowMeans(errorARS^2), na.rm = T),3)
}

if (simdes[k,]$scale == "unbalanced"){
    total_unbal = c(total_unbal, errorARS)
}
}

for(k in c(1:72)){
load(paste0("ResultsSimScale_",k+72,".RData"))

#  rem <- NA
#  Scale_level.models_temp <- list()
#    for(i in 1:length(Scale_level.models)){
#    if(all(Scale_level.models[[i]]$warning == 'none')){
#    rem[i] <- i      
#    Scale_level.models_temp[[i]] <- Scale_level.models[[i]]$value
#     }
#  }
  
#if(length(which(is.na(rem))) == 0){
#    Scale_level.models <- Scale_level.models_temp
#} else {
#  Scale_level.models_temp <- Scale_level.models_temp[-c(which(is.na(rem)))]
#  Scale_level.models <- Scale_level.models_temp
#}


if(simdes$Nfac[k] == 1){
error <- NA
errorARS <- NA
for(i in 1:100){

if(is.numeric(Scale_level.models[[i]]$value$alphas$Int.fit[[2]])){
error[i] = Scale_level.models[[i]]$value$alphas$Int.fit[[2]][1:simdes$Nfac[k]]
} else { 
error[i] = NA
}

#if(any(Scale_level.models[[i]]$value$Comparative.fit$ARSModels[3] != 'none' ,
# is.null(Scale_level.models[[i]]$value$Comparative.fit$ARSModels[3]))){
#errorARS[i] = NA
#} else {
#errorARS[i] = Scale_level.models[[i]]$value$alphas$Int.fitARS[[2]][1:simdes$Nfac[k]]
#}

if(is.double(Scale_level.models[[i]]$value$fit.measures$Int.fitARS)){
errorARS[i] = Scale_level.models[[i]]$value$alphas$Int.fitARS[[2]][1:simdes$Nfac[k]]
} else {
errorARS[i] = NA
}


Results.bias_unequal[k,]$Bias <- round(mean(error, na.rm = T),3)
Results.bias_unequal[k,]$MSE <- round(mean(error^2, na.rm = T),3)
Results.bias_unequal[k,]$BiasARS <- paste0(round(mean(errorARS[which(errorARS > -2.6 & errorARS < 2.6)], na.rm = T),3),                                         " (",
                                           length(errorARS[which(errorARS > -2.6 & errorARS < 2.6)])/length(errorARS)*100,"%)")
Results.bias_unequal[k,]$MSEARS <- round(mean(errorARS^2, na.rm = T),3)
}


} else {
error <- matrix(NA, nrow = 100, ncol =2)
errorARS <- matrix(NA, nrow = 100, ncol =2)

for(i in 1:100){
if(is.numeric(Scale_level.models[[i]]$value$alphas$Int.fit[[2]])){
error[i,] = Scale_level.models[[i]]$value$alphas$Int.fit[[2]][1:simdes$Nfac[k]]
} else { 
error[i,] = rep(NA,2)
}

#if(any(Scale_level.models[[i]]$value$Comparative.fit$ARSModels[3] != 'none' ,
# is.null(Scale_level.models[[i]]$value$Comparative.fit$ARSModels[3]))){
#errorARS[i,] = rep(NA,2)
#} else {
#errorARS[i,] = Scale_level.models[[i]]$value$alphas$Int.fitARS[[2]][1:simdes$Nfac[k]]
#}

if(is.double(Scale_level.models[[i]]$value$fit.measures$Int.fitARS)){
errorARS[i,] = Scale_level.models[[i]]$value$alphas$Int.fitARS[[2]][1:simdes$Nfac[k]]
} else {
errorARS[i,] = rep(NA,2)
}


}

Results.bias_unequal[k,]$Bias <- round(mean(rowMeans(error), na.rm = T),3)
Results.bias_unequal[k,]$MSE <- round(mean(rowMeans(error^2), na.rm = T),3)
Results.bias_unequal[k,]$BiasARS <- paste0(round(mean(errorARS[which(errorARS > -2.6 & errorARS < 2.6)], na.rm = T),3),                                         " (",
                                           length(errorARS[which(errorARS > -2.6 & errorARS < 2.6)])/length(errorARS)*100,"%)")
Results.bias_unequal[k,]$MSEARS <- round(mean(rowMeans(errorARS^2), na.rm = T),3)
}

}

#Create tables 
Alpha_final_ARS <- cbind(
    subset(Results.bias, (Results.bias$Nfac ==1 & Results.bias$ARS != 0))[,-c(9,11)],
    subset(Results.bias, (Results.bias$Nfac ==2 & Results.bias$ARS != 0))[,c(7,8,10)],
    subset(Results.bias_unequal, (Results.bias_unequal$Nfac ==1 & Results.bias_unequal$ARS != 0))[,c(7,8,10)],
    subset(Results.bias_unequal, (Results.bias_unequal$Nfac ==2 & Results.bias_unequal$ARS != 0))[,c(7,8,10)]
)
Alpha_final_ARS <-  Alpha_final_ARS[
  with(Alpha_final_ARS, order(ARS, scale, sample, j)),
]
Alpha_final_ARS 

Alpha_final_Null <- cbind(
    subset(Results.bias, (Results.bias$Nfac ==1 & Results.bias$ARS == 0))[,-c(9,11)],
    subset(Results.bias, (Results.bias$Nfac ==2 & Results.bias$ARS == 0))[,c(7,8,10)],
    subset(Results.bias_unequal, (Results.bias_unequal$Nfac ==1 & Results.bias_unequal$ARS == 0))[,c(7,8,10)],
    subset(Results.bias_unequal, (Results.bias_unequal$Nfac ==2 & Results.bias_unequal$ARS == 0))[,c(7,8,10)]
)
Alpha_final_Null <-  Alpha_final_Null[
  with(Alpha_final_Null, order(ARS, scale, sample, j)),
]

# Save tables
write.csv(format(Alpha_final_ARS, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/AlphaARS.csv", quote = F)
write.csv(format(Alpha_final_Null, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/AlphaNull.csv", quote = F)
