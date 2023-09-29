########################################################################################################################
#
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
library(tictoc)

source("230607-Subroutines_Int_DIFARS.r")

#Factors simulation study
samplesize <- c(250,1000)
categories <- c(5)
scale <- c('balanced','semi-balanced','unbalanced')
nitems <- c(12, 24)
Nfac <- c(1,2)
ARS <- c(0,.3,.6)
DIFARS <- c(T, F)

Simulation <- expand.grid(sample = samplesize, 
                          scale = scale, 
                          j = nitems, 
                          c = categories,
                          ARS = ARS,
                          Nfac = Nfac,
                          DIFARS = DIFARS
)

simdes <- cbind.data.frame(seed=c((200802):(200801+nrow(Simulation))),Simulation) 

i <- 30
simdes[i,]
SimulatedSet <- GenSimulationStudywithDIF(seed = simdes$seed[i], 
                                   Nfac = simdes$Nfac[i], 
                                   DIF = 0,
                                   DIFARS = simdes$DIFARS[i], 
                                   DIFsizeload = 0, 
                                   DIFsizeThr = 0, 
                                   sample = simdes$sample[i], 
                                   scale = simdes$scale[i],
                                   nitems = simdes$j[i],
                                   cat = simdes$c[i], 
                                   ARS = simdes$ARS[i], 
                                   nrep = 150)
length(SimulatedSet$Data)
SimulatedSet$loadingsG2
SimulatedSet$thresholdsG2
Data <- SimulatedSet$Data[[98]]
#lavInspect(aba$Mod.Conf, what = "thresholds")

#Create CCFA scale-level Models 
Models.scale <- Model_Creation(Nfac = simdes$Nfac[i],
                         nitems = simdes$j[i], 
                         scale = T)

#Create CCFA item-level Models 
Models.item <- Model_Creation.item(Nfac = simdes$Nfac[i],
                         nitems = simdes$j[i], 
                         cat = simdes$c[i])

#Estimate CCFA models
tic()
aba <-  Estimate.MI.Models.ARS(Data = SimulatedSet$Data[[1]],
                          Models = Models.scale)
toc()

aba$alphas$Int.fit[[2]]
aba$alphas$Int.fitARS[[2]]

#modindices(aba$Models.fit$Mod.Conf$value, sort = T)
#lavInspect(aba$Models.fit$Mod.Conf$value, "std")$`1`$theta
#lavInspect(aba$Models.fit$Mod.Conf$value, "std")$`2`$theta
lavInspect(aba$Models.fit$Mod.Conf$value, "std")$`1`$alpha
lavInspect(aba$Models.fit$Mod.Conf$value, "std")$`2`$alpha

lavInspect(aba$Models.fit$Mod.ThrARS$value, "std")$`1`$alpha
lavInspect(aba$Models.fit$Mod.ThrARS$value, "std")$`2`$alpha

#lavInspect(aba$Models.fit$Mod.Conf$value, "std")$`2`$lambda


tic()
aba.item <-  Estimate.MI.Models.item(Data = SimulatedSet$Data[[1]],
                           Models = Models.item,
                           Nfac = Nfac)
toc()





lavTestLRT(aba$Mod.Conf$value, aba$Mod.ConfARS$value)
round(aba$Conf.fit[c("rmsea.scaled", "cfi.scaled")],3)
round(aba$Conf.fitARS[c("rmsea.scaled", "cfi.scaled")],3)

lavTestLRT(aba$Mod.Conf$value, aba$Mod.Thr$value, aba$Mod.Load$value)
round(aba$Thr.fit[c("rmsea.scaled", "cfi.scaled")] - aba$Conf.fit[c("rmsea.scaled", "cfi.scaled")],3)
round(aba$Load.fit[c("rmsea.scaled", "cfi.scaled")] - aba$Thr.fit[c("rmsea.scaled", "cfi.scaled")],3)

lavTestLRT(aba$Mod.ConfARS$value, aba$Mod.ThrARS$value, aba$Mod.LoadARS$value)
round(aba$Thr.fitARS[c("rmsea.scaled", "cfi.scaled")] - aba$Conf.fitARS[c("rmsea.scaled", "cfi.scaled")],3)
round(aba$Load.fitARS[c("rmsea.scaled", "cfi.scaled")] - aba$Thr.fitARS[c("rmsea.scaled", "cfi.scaled")],3)


############################################################################################################
# Check item results using the chi^2 test, since common cut-offs for fit measures are sub-optimal when evaluating
# MI at the item-level. 


for(i in 3:13){
  print(aba.item$Thr.fit[[i]]$`Pr(>Chisq)`[2])
}


############################################################################################################

Models.item <- Model_Creation.item(Nfac = Nfac,
                         nitems = nitems)


test.item.constrained <- cfa(as.character(Models.item$LoadModnoARS[[1]]), data = Data, group = "group")
test.item.constrained.thr <- cfa(as.character(Models.item$ThrModnoARS[[1]]), data = Data, group = "group")
test.item.unconstrained.thr <- cfa(as.character(Models.item$ThrModnoARS[[11]]), data = Data, group = "group")
test.item.unconstrained <- cfa(as.character(Models.item$LoadModARS[[11]]), data = Data, group = "group")


which((round(lavInspect(test.item.constrained, "est")$`1`$lambda,4) == round(lavInspect(test.item.constrained, "est")$`2`$lambda,4)) == F)
which((round(lavInspect(test.item.unconstrained, "est")$`1`$lambda,4) == round(lavInspect(test.item.unconstrained, "est")$`2`$lambda,4))==F)
(round(lavInspect(test.item.unconstrained, "est")$`1`$lambda,4) == round(lavInspect(test.item.unconstrained, "est")$`2`$lambda,4))
which((round(lavInspect(test.item.constrained, "est")$`1`$tau,4) == round(lavInspect(test.item.constrained, "est")$`2`$tau,4)) == F)
(round(lavInspect(test.item.unconstrained.thr, "est")$`1`$tau,4) == round(lavInspect(test.item.unconstrained.thr, "est")$`2`$tau,4))

test.item.constrained
test.item.unconstrained
summary(test.item.constrained)
summary(test.item.unconstrained)
summary(test.item.unconstrained.thr)


lavTestLRT(test.item.constrained, test.item.unconstrained)
lavTestLRT(test.item.constrained.thr, test.item.unconstrained.thr)
lavTestLRT(test.item.constrained.thr, test.item.unconstrained.thr)

#Estimate CCFA models
aba <-  Estimate.MI.Models(Data = SimulatedSet$Data[[1]],
                           Models = Models)


lavInspect(test.item.unconstrained, "std")$`2`$lambda