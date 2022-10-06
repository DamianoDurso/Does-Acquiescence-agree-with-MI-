################################################################################################################
#
#   SIMULATION STUDY D'URSO et al. (2022)
#
#

library(doParallel)
library(parallel)
library(GPArotation)
library(psych)
library(BDgraph)
library(lavaan)
library(semTools)
library(MASS)

source("Subroutines.R")

#Factors simulation study
samplesize <- c(250,1000)
categories <- c(5)
scale <- c('balanced','semi-balanced','unbalanced')
nitems <- c(12, 24)
Nfac <- c(1,2)
ARS <- c(0,.3,.6)

Simulation <- expand.grid(sample = samplesize, 
                          scale = scale, 
                          j = nitems, 
                          c = categories,
                          ARS = ARS,
                          Nfac = Nfac
)
simdes <- cbind.data.frame(seed=c((200802):(200801+nrow(Simulation))),Simulation) 

#Run in Parallel
cores <- detectCores()
cl <- parallel::makeCluster(cores - 1)
doParallel::registerDoParallel(cl)

foreach(i = 1:nrow(simdes))%dopar%{
  library(psych)
  library(BDgraph)
  library(lavaan)
  library(semTools)
  library(MASS)
  
# Generate Data  
  SimulatedSet <- GenSimulationStudy(seed = simdes$seed[i], 
                                     Nfac = simdes$Nfac[i], 
                                     DIF = 0, 
                                     DIFsizeload = 0, 
                                     DIFsizeThr = 0, 
                                     sample = simdes$sample[i], 
                                     scale = simdes$scale[i],
                                     nitems = simdes$j[i],
                                     cat = simdes$c[i], 
                                     ARS = simdes$ARS[i], 
                                     nrep = 400)
  
  
  # Create CCFA scale-level Models 
  Models.scale <- Model_Creation(Nfac = simdes$Nfac[i],
                           nitems = simdes$j[i], 
                           scale = T)
  
  # Create CCFA item-level Models 
  Models.item <- Model_Creation.item(Nfac = simdes$Nfac[i],
                                     nitems = simdes$j[i], 
                                     cat = simdes$c[i])
  
  
# Lists to store results
Scale_level.models <- list()  
Item_level.models <- list()  
    
  # Estimate CCFA models (scale and item level)
  tic()
  for(j in 1:150){
    Scale_level.models[[j]] <-  Estimate.MI.Models(Data = SimulatedSet$Data[[j]],
                                              Models = Models.scale)    
    Item_level.models[[j]] <-  Estimate.MI.Models.item(Data = SimulatedSet$Data[[j]],
                                                  Models = Models.item,
                                                  Nfac = simdes$Nfac[i])
  }
  toc()
  
  # Save results
  save(Scale_level.models, Item_level.models, Models.scale, Models.item, SimulatedSet,
       file =paste0(#"C:/Users/...",
                    "ResultsSim_",i, ".RData"))
  
  
}





