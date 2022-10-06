##########################################################################################
#
#
#              Extract Results and make tables D'Urso et al. 2022
#
#

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


#Attach results columns to simulation study
#NO-ARS RESULTS
NoARS_Conf_chi <- NA    
NoARS_Conf_df <- NA    
NoARS_Conf_RMSEA <- NA
NoARS_Conf_cfi <- NA

NoARS_Thr_chi <- NA
NoARS_Thr_df <- NA
NoARS_Thr_RMSEA <- NA
NoARS_Thr_cfi <- NA
NoARS_Thr_comb <- NA

NoARS_Load_chi <- NA
NoARS_Load_df <- NA
NoARS_Load_RMSEA <- NA
NoARS_Load_cfi <- NA
NoARS_Load_comb <- NA


Fit.NoARS_Thr_chi <- NA
Fit.NoARS_Thr_df <- NA
Fit.NoARS_Thr_RMSEA <- NA
Fit.NoARS_Thr_cfi <- NA
Fit.NoARS_Thr_comb <- NA

Fit.NoARS_Load_chi <- NA
Fit.NoARS_Load_df <- NA
Fit.NoARS_Load_RMSEA <- NA
Fit.NoARS_Load_cfi <- NA
Fit.NoARS_Load_comb <- NA

#ARS RESULTS
ARS_Conf_chi <- NA
ARS_Conf_df <- NA
ARS_Conf_RMSEA <- NA
ARS_Conf_cfi <- NA

ARS_Thr_chi <- NA
ARS_Thr_df <- NA
ARS_Thr_RMSEA <- NA
ARS_Thr_cfi <- NA
ARS_Thr_comb <- NA

ARS_Load_chi <- NA
ARS_Load_df <- NA
ARS_Load_RMSEA <- NA
ARS_Load_cfi <- NA
ARS_Load_comb <- NA

Fit.ARS_Thr_chi <- NA
Fit.ARS_Thr_df <- NA
Fit.ARS_Thr_RMSEA <- NA
Fit.ARS_Thr_cfi <- NA
Fit.ARS_Thr_comb <- NA

Fit.ARS_Load_chi <- NA
Fit.ARS_Load_df <- NA
Fit.ARS_Load_RMSEA <- NA
Fit.ARS_Load_cfi <- NA
Fit.ARS_Load_comb <- NA


#ARS VS NO ars

ARS_vs_noARS <- NA

# Converged models

Conv.Conf.ARS <- NA
Conv.Thr.ARS <- NA
Conv.Load.ARS <- NA

Conv.Conf <- NA
Conv.Thr <- NA
Conv.Load <- NA


Results.scale <- cbind(simdes, NoARS_Conf_RMSEA, NoARS_Conf_cfi,
                       NoARS_Thr_chi,NoARS_Thr_RMSEA, NoARS_Thr_cfi, NoARS_Thr_comb,
                       NoARS_Load_chi, NoARS_Load_RMSEA, NoARS_Load_cfi,NoARS_Load_comb,
                       ARS_Conf_RMSEA, ARS_Conf_cfi, 
                       ARS_Thr_chi, ARS_Thr_RMSEA, ARS_Thr_cfi, ARS_Thr_comb,
                       ARS_Load_chi, ARS_Load_RMSEA, ARS_Load_cfi, ARS_Load_comb,
                       ARS_vs_noARS, Conv.Conf.ARS, Conv.Thr.ARS, Conv.Load.ARS, Conv.Conf,
                       Conv.Thr, Conv.Load
)

#results for average difference in relative fit (i.e., loadings and thresholds)
Results.scale.diff <- cbind(simdes,
                       NoARS_Thr_chi,NoARS_Thr_RMSEA, NoARS_Thr_cfi, 
                       NoARS_Load_chi, NoARS_Load_RMSEA, NoARS_Load_cfi,
                       ARS_Thr_chi, ARS_Thr_RMSEA, ARS_Thr_cfi, 
                       ARS_Load_chi, ARS_Load_RMSEA, ARS_Load_cfi
)

Results.scale.mean <- cbind(simdes, NoARS_Conf_chi, NoARS_Conf_df, NoARS_Conf_RMSEA, NoARS_Conf_cfi,
                       NoARS_Thr_chi,NoARS_Thr_df,NoARS_Thr_RMSEA, NoARS_Thr_cfi, 
                       NoARS_Load_chi,NoARS_Load_df, NoARS_Load_RMSEA, NoARS_Load_cfi,
                       ARS_Conf_chi,ARS_Conf_df,ARS_Conf_RMSEA, ARS_Conf_cfi, 
                       ARS_Thr_chi,ARS_Thr_df, ARS_Thr_RMSEA, ARS_Thr_cfi, 
                       ARS_Load_chi,ARS_Load_df, ARS_Load_RMSEA, ARS_Load_cfi
)

setwd("C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Results/Scale/")

for(k in 61:nrow(simdes)){
  load(paste0("ResultsSimScale_",k,".RData"))  
  
for(i in 1:100){
  #NOARS results
  NoARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["chisq.scaled"]
  NoARS_Conf_df[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["df.scaled"]
  NoARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["rmsea.scaled"]# > 0.08
  NoARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fit["cfi.scaled"]# < .95
  
  NoARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2]# < .05
  NoARS_Thr_df[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["df.scaled"]# < .05
  NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"]# > 0.01
  NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["cfi.scaled"]# > .01
  NoARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                           (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] > .01,
                                Scale_level.models[[i]]$Comparative.fit$NoARSModels[[4]]["rmsea.scaled"] < -.01)))
  
  NoARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3]# < .05
  NoARS_Load_df[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["df.scaled"]# < .05
  NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"]# > 0.01
  NoARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["cfi.scaled"]# > .01
  NoARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$NoARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                             (any(Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] > .01,
                                  Scale_level.models[[i]]$Comparative.fit$NoARSModels[[7]]["rmsea.scaled"] < -.01)))

  #NOARS results absolute fit
  Fit.NoARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["chisq.scaled"] 
  Fit.NoARS_Thr_df[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["df.scaled"] 
  Fit.NoARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["rmsea.scaled"] 
  Fit.NoARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fit["cfi.scaled"]# > .01

  Fit.NoARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["chisq.scaled"]# > 0.01
  Fit.NoARS_Load_df[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["df.scaled"]# > 0.01
  Fit.NoARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["rmsea.scaled"]# > 0.01
  Fit.NoARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fit["cfi.scaled"]# > .01

    
  #ARS RESULTS
  ARS_Conf_chi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["chisq.scaled"]
  ARS_Conf_df[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["df.scaled"]
  ARS_Conf_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["rmsea.scaled"]# > 0.08
  ARS_Conf_cfi[i] <- Scale_level.models[[i]]$fit.measures$Conf.fitARS["cfi.scaled"]# < .95
  
  if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
    ARS_Thr_chi[i] <- NA# < .05
  } else {
    ARS_Thr_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2]# < .05
  }
  ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"]# > 0.01
  ARS_Thr_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["cfi.scaled"]# > 0.01
  
  if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none" | Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]] != "none" ){
    ARS_Thr_comb[i] <- NA# < .05
  } else {
    ARS_Thr_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[2] < .05) & 
                             (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] > .01,
                                  Scale_level.models[[i]]$Comparative.fit$ARSModels[[4]]["rmsea.scaled"] < -.01)))
  }

  if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none" | Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]] != "none"){
    ARS_Load_chi[i] <- NA# < .05
  } else {
    ARS_Load_chi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3]# < .05
  }

  ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"]# > 0.01
  ARS_Load_cfi[i] <- Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["cfi.scaled"]# > 0.01
  if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
    ARS_Load_comb[i] <- NA# < .05
  } else {
    ARS_Load_comb[i] <- all((Scale_level.models[[i]]$Comparative.fit$ARSModels$value$`Pr(>Chisq)`[3] < .05) & 
                              (any(Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] > .01,
                                   Scale_level.models[[i]]$Comparative.fit$ARSModels[[7]]["rmsea.scaled"] < -.01)))
  }
  


# ARS Absolute fit
  Fit.ARS_Thr_chi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["chisq.scaled"]# > 0.01
  Fit.ARS_Thr_df[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["df.scaled"]# > 0.01
  Fit.ARS_Thr_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["rmsea.scaled"]# > 0.01
  Fit.ARS_Thr_cfi[i] <- Scale_level.models[[i]]$fit.measures$Thr.fitARS["cfi.scaled"]# > 0.01
  

  Fit.ARS_Load_chi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["chisq.scaled"]# > 0.01
  Fit.ARS_Load_df[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["df.scaled"]# > 0.01
  Fit.ARS_Load_RMSEA[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["rmsea.scaled"]# > 0.01
  Fit.ARS_Load_cfi[i] <- Scale_level.models[[i]]$fit.measures$Load.fitARS["cfi.scaled"]# > 0.01

    
  #ARS VS NO ars
  if(Scale_level.models[[i]]$Comparative.fit$ARSModels$error[1] != "none"){
    ARS_vs_noARS[i] <- NA# < .05
  } else {
    ARS_vs_noARS[i] <- Scale_level.models[[i]]$Comparative.fit$ARSvsNoArs$value$`Pr(>Chisq)`[2]# < .05
  }
  
  
  if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fitARS)){
    Conv.Conf.ARS[i] <- "Yes"
  } else {
    Conv.Conf.ARS[i] <- "No"
  }
  
  if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fitARS)){
    Conv.Thr.ARS[i] <- "Yes"
  } else {
    Conv.Thr.ARS[i] <- "No"
  }
  
  if(is.double(Scale_level.models[[i]]$fit.measures$Load.fitARS)){
    Conv.Load.ARS[i] <- "Yes"
  } else {
    Conv.Load.ARS[i] <- "No"
  }
  
  if(is.double(Scale_level.models[[i]]$fit.measures$Conf.fit)){
    Conv.Conf[i] <- "Yes"
  } else {
    Conv.Conf[i] <- "No"
  }
  
  if(is.double(Scale_level.models[[i]]$fit.measures$Thr.fit)){
    Conv.Thr[i] <- "Yes"
  } else {
    Conv.Thr[i] <- "No"
  }
  
  if(is.double(Scale_level.models[[i]]$fit.measures$Load.fit)){
    Conv.Load[i] <- "Yes"
  } else {
    Conv.Load[i] <- "No"
  }
  
} 
  #Dichotomous decision
  Results.scale$NoARS_Conf_RMSEA[k] <-  length(which(NoARS_Conf_RMSEA >= .08))/length(NoARS_Conf_RMSEA)
  Results.scale$NoARS_Conf_cfi[k] <-  length(which(NoARS_Conf_cfi <= .95))/length(NoARS_Conf_cfi)
  Results.scale$NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi <= .05))/length(NoARS_Thr_chi))  
  Results.scale$NoARS_Thr_RMSEA[k] <- length(which(NoARS_Thr_RMSEA >= .01))/length(NoARS_Thr_RMSEA)
  Results.scale$NoARS_Thr_cfi[k] <- length(which(NoARS_Thr_cfi <= -.01))/length(NoARS_Thr_cfi)
  Results.scale$NoARS_Thr_comb[k] <- length(which(NoARS_Thr_comb == T))/length(NoARS_Thr_comb)
  Results.scale$NoARS_Load_chi[k] <- length(which(NoARS_Load_chi < .05))/length(NoARS_Load_chi)
  Results.scale$NoARS_Load_RMSEA[k] <- length(which(NoARS_Load_RMSEA >= .01))/length(NoARS_Load_RMSEA)
  Results.scale$NoARS_Load_cfi[k] <- length(which(NoARS_Load_cfi <= -.01))/length(NoARS_Load_cfi)
  Results.scale$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale$ARS_Conf_RMSEA[k] <-  length(which(unlist(ARS_Conf_RMSEA) >= .08))/length(unlist(ARS_Conf_RMSEA))
  Results.scale$ARS_Conf_cfi[k] <-  length(which(unlist(ARS_Conf_cfi) <= .95))/length(unlist(ARS_Conf_cfi))
  Results.scale$ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi) <= .05))/length(unlist(ARS_Thr_chi))
  Results.scale$ARS_Thr_RMSEA[k] <- length(which(unlist(ARS_Thr_RMSEA) >= .01))/length(unlist(ARS_Thr_RMSEA))
  Results.scale$ARS_Thr_cfi[k] <- length(which(unlist(ARS_Thr_cfi) <= -.01))/length(unlist(ARS_Thr_cfi))
  Results.scale$ARS_Thr_comb[k] <- length(which(unlist(ARS_Thr_comb) == T))/length(unlist(ARS_Thr_comb))
  Results.scale$ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi) < .05))/length(unlist(ARS_Load_chi))
  Results.scale$ARS_Load_RMSEA[k] <- length(which(unlist(ARS_Load_RMSEA) >= .01))/length(unlist(ARS_Load_RMSEA))
  Results.scale$ARS_Load_cfi[k] <- length(which(unlist(ARS_Load_cfi) <= -.01))/length(unlist(ARS_Load_cfi))
  Results.scale$ARS_Load_comb[k] <- length(which(unlist(ARS_Load_comb) == T))/length(unlist(ARS_Load_comb))
  
  #Difference in relative fit
  Results.scale.diff$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi - NoARS_Conf_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi - NoARS_Conf_chi)),3), ")" ) 
  Results.scale.diff$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA - NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA - NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.diff$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi - NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi - NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.diff$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi - Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi - Fit.NoARS_Thr_chi)),3), ")" )
  Results.scale.diff$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA - Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA - Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.diff$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi - Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi - Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )

  Results.scale.diff$ARS_Thr_chi[k] <- paste0(round(mean(unlist(mapply('-',Fit.ARS_Thr_chi, ARS_Conf_chi))),3), " (", round(sd(unlist(mapply('-',Fit.ARS_Thr_chi, ARS_Conf_chi))),3), ")" ) 
  Results.scale.diff$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(mapply('-',Fit.ARS_Thr_RMSEA, ARS_Conf_RMSEA))),3),nsmall=3), " (", format(round(sd(unlist(mapply('-',Fit.ARS_Thr_RMSEA, ARS_Conf_RMSEA))),3),nsmall=3), ")" )
  Results.scale.diff$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(mapply('-',Fit.ARS_Thr_cfi, ARS_Conf_cfi))),3),nsmall=3), " (", format(round(sd(unlist(mapply('-',Fit.ARS_Thr_cfi, ARS_Conf_cfi))),3),nsmall=3), ")" )
  Results.scale.diff$ARS_Load_chi[k] <- paste0(round(mean(unlist(mapply('-',Fit.ARS_Load_chi, Fit.ARS_Thr_chi))),3), " (", round(sd(unlist(mapply('-',Fit.ARS_Load_chi, Fit.ARS_Thr_chi))),3), ")" )
  Results.scale.diff$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(mapply('-',Fit.ARS_Load_RMSEA, Fit.ARS_Thr_RMSEA))),3),nsmall=3), " (", format(round(sd(unlist(mapply('-',Fit.ARS_Load_RMSEA, Fit.ARS_Thr_RMSEA))),3),nsmall=3), ")" )
  Results.scale.diff$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(mapply('-',Fit.ARS_Load_cfi, Fit.ARS_Thr_cfi))),3),nsmall=3), " (", format(round(sd(unlist(mapply('-',Fit.ARS_Load_cfi, Fit.ARS_Thr_cfi))),3),nsmall=3), ")" )
  
  Results.scale$ARS_vs_noARS[k] <- length(which(ARS_vs_noARS < .05))/100
   
  #Mean goodness-of-fit measures
  Results.scale.mean$NoARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_df[k] <-    mean(NoARS_Conf_df)
  Results.scale.mean$NoARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(NoARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(NoARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Thr_chi)),3), " (", round(sd(unlist(Fit.NoARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$NoARS_Thr_df[k] <- mean(NoARS_Thr_df)
  Results.scale.mean$NoARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Thr_cfi)),3),nsmall=3), ")" )
#  Results.scale.mean$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$NoARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.NoARS_Load_chi)),3), " (", round(sd(unlist(Fit.NoARS_Load_chi)),3), ")" )
  Results.scale.mean$NoARS_Load_df[k] <- mean(NoARS_Load_df)
    Results.scale.mean$NoARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$NoARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.NoARS_Load_cfi)),3),nsmall=3), ")" )
#  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  Results.scale.mean$ARS_Conf_chi[k] <-    paste0(format(round(mean(unlist(ARS_Conf_chi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_chi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_df[k] <-    mean(unlist(ARS_Conf_df))
  Results.scale.mean$ARS_Conf_RMSEA[k] <-    paste0(format(round(mean(unlist(ARS_Conf_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Conf_cfi[k] <-  paste0(format(round(mean(unlist(ARS_Conf_cfi)),3),nsmall=3), " (", format(round(sd(unlist(ARS_Conf_cfi)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Thr_chi)),3), " (", round(sd(unlist(Fit.ARS_Thr_chi)),3), ")" ) 
  Results.scale.mean$ARS_Thr_df[k] <-  mean(unlist(Fit.ARS_Thr_df))
  Results.scale.mean$ARS_Thr_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Thr_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Thr_cfi)),3),nsmall=3), ")" )
  #  Results.scale.me$NoARS_Thr_comb[k] <- paste0(round(mean(unlist(NoARS_Thr_comb)),3), " (", round(sd(unlist(NoARS_Thr_thrco)),3), ")" )
  Results.scale.mean$ARS_Load_chi[k] <- paste0(round(mean(unlist(Fit.ARS_Load_chi)),3), " (", round(sd(unlist(Fit.ARS_Load_chi)),3), ")" )
  Results.scale.mean$ARS_Load_df[k] <- mean(unlist(Fit.ARS_Load_df))
  Results.scale.mean$ARS_Load_RMSEA[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_RMSEA)),3),nsmall=3), ")" )
  Results.scale.mean$ARS_Load_cfi[k] <- paste0(format(round(mean(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), " (", format(round(sd(unlist(Fit.ARS_Load_cfi)),3),nsmall=3), ")" )
  #  Results.scale.mean$NoARS_Load_comb[k] <- length(which(NoARS_Load_comb == T))/length(NoARS_Load_comb)
  
  
  
#Convergence results  
  Results.scale$Conv.Conf.ARS[k] <- length(unlist(which(Conv.Conf.ARS == "Yes")))/100
  Results.scale$Conv.Thr.ARS[k] <- length(unlist(which(Conv.Thr.ARS == "Yes")))/100
  Results.scale$Conv.Load.ARS[k] <- length(unlist(which(Conv.Load.ARS == "Yes")))/100
  Results.scale$Conv.Conf[k] <- length(unlist(which(Conv.Conf == "Yes")))/100
  Results.scale$Conv.Thr[k] <- length(unlist(which(Conv.Thr == "Yes")))/100
  Results.scale$Conv.Load[k] <- length(unlist(which(Conv.Load == "Yes")))/100
  
#  print(hist(NoARS_Conf_RMSEA, main = paste0 ("Condition", k), xlim = c(0, .15)))
#  print(hist(NoARS_Conf_cfi, main = paste0 ("Condition", k), xlim = c(.87, 1)))
  
}

#----------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------- Make results tables ---------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------#

#Null Results MI Convergence
Conv.Res_test_null <- subset(Results.scale, Results.scale$ARS == 0 )
Conv.Res_test_null <- cbind(Conv.Res_test_null[,1:7], round(Conv.Res_test_null[,32:34],3))

#ARS Results MI Convergence
Conv.Res_test <- subset(Results.scale, Results.scale$ARS != 0 )
Conv.Res_test <- cbind(Conv.Res_test[,1:7], round(Conv.Res_test[,29:31],3))

Conv.Res_test_NoARS <- Conv.Res_test
#  cbind(Conv.Res_test[,1:7], round(Conv.Res_test[,32:34],3))



#Null Results MI
Res_test_null <- subset(Results.scale, Results.scale$ARS == 0 )
Res_test_null <- cbind(Res_test_null[,1:7], round(Res_test_null[,8:28],3))

#ARS Results MI
Res_test <- subset(Results.scale, Results.scale$ARS != 0 )
Res_test <- cbind(Res_test[,1:7], round(Res_test[,8:28],3))


#--------------------------------------------------------------------------------------------------------------#
# Create Results Tables for Convergence

#Create results for different levels of ARS
Conv.Res_test_small <- subset(Conv.Res_test, Res_test$ARS == 0.3)
Conv.Res_test_large <- subset(Conv.Res_test, Res_test$ARS == 0.6)

Conv.Res_test_small_1fac <- subset(Conv.Res_test_small, Conv.Res_test_small$Nfac == 1)
Conv.Res_test_small_2fac <- subset(Conv.Res_test_small, Conv.Res_test_small$Nfac == 2)

Conv.Res_test_large_1fac <- subset(Conv.Res_test_large, Conv.Res_test_large$Nfac == 1)
Conv.Res_test_large_2fac <- subset(Conv.Res_test_large, Conv.Res_test_large$Nfac == 2)

Conv.Res_1fac_ARS <- rbind(Conv.Res_test_small_1fac[order(Conv.Res_test_small_1fac$scale, Conv.Res_test_small_1fac$sample, Conv.Res_test_small_1fac$j),],
                           Conv.Res_test_large_1fac[order(Conv.Res_test_large_1fac$scale, Conv.Res_test_large_1fac$sample, Conv.Res_test_large_1fac$j),])


Conv.Res_2fac_ARS <- rbind(Conv.Res_test_small_2fac[order(Conv.Res_test_small_2fac$scale, Conv.Res_test_small_2fac$sample, Conv.Res_test_small_2fac$j),],
                      Conv.Res_test_large_2fac[order(Conv.Res_test_large_2fac$scale, Conv.Res_test_large_2fac$sample, Conv.Res_test_large_2fac$j),])

Conv.Res_NoArs <- Conv.Res_test_NoARS[order(Conv.Res_test_NoARS$Nfac, 
                                                 Conv.Res_test_NoARS$ARS,
                                                 Conv.Res_test_NoARS$scale,
                                                 Conv.Res_test_NoARS$sample,
                                                 Conv.Res_test_NoARS$j),]
  
Conv.Res_NoArs.fin <- cbind(subset(Conv.Res_NoArs, Conv.Res_NoArs$Nfac==1), subset(Conv.Res_NoArs, Conv.Res_NoArs$Nfac==2)[8:10])
  
####################################################################################################################################
# NULL Convergence

Conv.Res_test_null.final <- Conv.Res_test_null[order(Conv.Res_test_null$Nfac,Conv.Res_test_null$scale, Conv.Res_test_null$sample, Conv.Res_test_null$scale, Conv.Res_test_null$j),]


#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
# Create Results Tables for MI

#Create results for different levels of ARS
Res_test_small <- subset(Res_test, Res_test$ARS == 0.3)
Res_test_large <- subset(Res_test, Res_test$ARS == 0.6)

Res_test_small_1fac <- subset(Res_test_small, Res_test_small$Nfac == 1)
Res_test_small_2fac <- subset(Res_test_small, Res_test_small$Nfac == 2)

Res_test_large_1fac <- subset(Res_test_large, Res_test_large$Nfac == 1)
Res_test_large_2fac <- subset(Res_test_large, Res_test_large$Nfac == 2)

Res_1fac_ARS <- rbind(Res_test_small_1fac[order(Res_test_small_1fac$scale, Res_test_small_1fac$sample, Res_test_small_1fac$j),-c(8:17,28:29)],
      Res_test_large_1fac[order(Res_test_large_1fac$scale, Res_test_large_1fac$sample, Res_test_large_1fac$j),-c(8:17,28:29)])
      
      
Res_1fac_NoARS <- rbind(Res_test_small_1fac[order(Res_test_small_1fac$scale, Res_test_small_1fac$sample, Res_test_small_1fac$j),-c(18:29)],
                    Res_test_large_1fac[order(Res_test_large_1fac$scale, Res_test_large_1fac$sample, Res_test_large_1fac$j),-c(18:29)])

####################################################################################################################################
# 2 FACTORS
Res_2fac_ARS <- rbind(Res_test_small_2fac[order(Res_test_small_2fac$scale, Res_test_small_2fac$sample, Res_test_small_2fac$j),-c(8:17,28:29)],
                    Res_test_large_2fac[order(Res_test_large_2fac$scale, Res_test_large_2fac$sample, Res_test_large_2fac$j),-c(8:17,28:29)])


Res_2fac_NoARS <- rbind(Res_test_small_2fac[order(Res_test_small_2fac$scale, Res_test_small_2fac$sample, Res_test_small_2fac$j),-c(18:29)],
                    Res_test_large_2fac[order(Res_test_large_2fac$scale, Res_test_large_2fac$sample, Res_test_large_2fac$j),-c(18:29)])


####################################################################################################################################
# Null

Res_test_null.final <- Res_test_null[order(Res_test_null$Nfac, Res_test_null$scale, Res_test_null$sample, Res_test_null$j),c(1:7,8:17)]

####################################################################################################################################

#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
# Create Results Tables for MI with mean fit measures

#Null Results MI
Mean.Res_test_null <- subset(Results.scale.mean, Results.scale.mean$ARS == 0 )
Mean.Res_test_null <- cbind(Mean.Res_test_null[,1:7], Mean.Res_test_null[,8:19])

Diff.Res_test_null <- subset(Results.scale.diff, Results.scale.diff$ARS == 0 )
Diff.Res_test_null <- cbind(Diff.Res_test_null[,1:7], Diff.Res_test_null[,8:19])

#ARS Results MI
Mean.Res_test <- subset(Results.scale.mean, Results.scale.mean$ARS != 0 )
Mean.Res_test <- cbind(Mean.Res_test[,1:7], Mean.Res_test[,8:31])

Diff.Res_test <- subset(Results.scale.diff, Results.scale.diff$ARS != 0 )
Diff.Res_test <- cbind(Diff.Res_test[,1:7], Diff.Res_test[,8:19])

#Create results for different levels of ARS
Mean.Res_test_small <- subset(Mean.Res_test, Mean.Res_test$ARS == 0.3)
Mean.Res_test_large <- subset(Mean.Res_test, Mean.Res_test$ARS == 0.6)

Mean.Res_test_small_1fac <- subset(Mean.Res_test_small, Mean.Res_test_small$Nfac == 1)
Mean.Res_test_small_2fac <- subset(Mean.Res_test_small, Mean.Res_test_small$Nfac == 2)

Mean.Res_test_large_1fac <- subset(Mean.Res_test_large, Mean.Res_test_large$Nfac == 1)
Mean.Res_test_large_2fac <- subset(Mean.Res_test_large, Mean.Res_test_large$Nfac == 2)

Mean.Res_1fac_ARS <- rbind(Mean.Res_test_small_1fac[order(Mean.Res_test_small_1fac$scale, Mean.Res_test_small_1fac$sample, Mean.Res_test_small_1fac$j),-c(8:19)],
                      Mean.Res_test_large_1fac[order(Mean.Res_test_large_1fac$scale, Mean.Res_test_large_1fac$sample, Mean.Res_test_large_1fac$j),-c(8:19)])


Mean.Res_1fac_NoARS <- rbind(Mean.Res_test_small_1fac[order(Mean.Res_test_small_1fac$scale, Mean.Res_test_small_1fac$sample, Mean.Res_test_small_1fac$j),-c(20:31)],
                        Mean.Res_test_large_1fac[order(Mean.Res_test_large_1fac$scale, Mean.Res_test_large_1fac$sample, Mean.Res_test_large_1fac$j),-c(20:31)])

# AND FOR THE RELATIVE FIT
Diff.Res_test_small <- subset(Diff.Res_test, Diff.Res_test$ARS == 0.3)
Diff.Res_test_large <- subset(Diff.Res_test, Diff.Res_test$ARS == 0.6)

Diff.Res_test_small_1fac <- subset(Diff.Res_test_small, Diff.Res_test_small$Nfac == 1)
Diff.Res_test_small_2fac <- subset(Diff.Res_test_small, Diff.Res_test_small$Nfac == 2)

Diff.Res_test_large_1fac <- subset(Diff.Res_test_large, Diff.Res_test_large$Nfac == 1)
Diff.Res_test_large_2fac <- subset(Diff.Res_test_large, Diff.Res_test_large$Nfac == 2)

Diff.Res_1fac_ARS <- rbind(Diff.Res_test_small_1fac[order(Diff.Res_test_small_1fac$scale, Diff.Res_test_small_1fac$sample, Diff.Res_test_small_1fac$j),-c(8:13)],
                           Diff.Res_test_large_1fac[order(Diff.Res_test_large_1fac$scale, Diff.Res_test_large_1fac$sample, Diff.Res_test_large_1fac$j),-c(8:13)])


Diff.Res_1fac_NoARS <- rbind(Diff.Res_test_small_1fac[order(Diff.Res_test_small_1fac$scale, Diff.Res_test_small_1fac$sample, Diff.Res_test_small_1fac$j),-c(14:25)],
                             Diff.Res_test_large_1fac[order(Diff.Res_test_large_1fac$scale, Diff.Res_test_large_1fac$sample, Diff.Res_test_large_1fac$j),-c(14:25)])

####################################################################################################################################
# 2 FACTORS
Mean.Res_2fac_ARS <- rbind(Mean.Res_test_small_2fac[order(Mean.Res_test_small_2fac$scale, Mean.Res_test_small_2fac$sample, Mean.Res_test_small_2fac$j),-c(8:19)],
                      Mean.Res_test_large_2fac[order(Mean.Res_test_large_2fac$scale, Mean.Res_test_large_2fac$sample, Mean.Res_test_large_2fac$j),-c(8:19)])


Mean.Res_2fac_NoARS <- rbind(Mean.Res_test_small_2fac[order(Mean.Res_test_small_2fac$scale, Mean.Res_test_small_2fac$sample, Mean.Res_test_small_2fac$j),-c(20:31)],
                        Mean.Res_test_large_2fac[order(Mean.Res_test_large_2fac$scale, Mean.Res_test_large_2fac$sample, Mean.Res_test_large_2fac$j),-c(20:31)])


####################################################################################################################################
# Null

Mean.Res_test_null.final <- Mean.Res_test_null[order(Mean.Res_test_null$Nfac, Mean.Res_test_null$scale, Mean.Res_test_null$sample, Mean.Res_test_null$j),c(1:7,8:19)]

####################################################################################################################################



####################################################################################################################################
#Store files

write.csv(format(Conv.Res_NoArs.fin, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Conv.Scale_NoARS.csv", quote = F)
write.csv(format(Conv.Res_1fac_ARS, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Conv.Scale_ARS_1FAC.csv", quote = F)
write.csv(format(Conv.Res_2fac_ARS,nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Conv.Scale_ARS_2FAC.csv", quote = F)
write.csv(format(cbind(subset(Conv.Res_test_null.final, Conv.Res_test_null.final$Nfac==1), subset(Conv.Res_test_null.final, Conv.Res_test_null.final$Nfac==2)[,8:10])
                 , nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Conv.Scale_Null.csv", quote = F)
write.csv(format(Res_1fac_ARS, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Scale_ARS_1FAC.csv", quote = F)
write.csv(format(Res_2fac_ARS,nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Scale_ARS_2FAC.csv", quote = F)
write.csv(format(Res_1fac_NoARS, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Scale_NoARS_1FAC.csv", quote = F)
write.csv(format(Res_2fac_NoARS, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Scale_NoARS_2FAC.csv", quote = F)
write.csv(format(Res_test_null.final, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Scale_Null.csv", quote = F)

#Mean results
write.csv(format(Mean.Res_1fac_ARS, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Mean.Scale_ARS_1FAC.csv", quote = F)
write.csv(format(Mean.Res_2fac_ARS,nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Mean.Scale_ARS_2FAC.csv", quote = F)
write.csv(format(Mean.Res_1fac_NoARS, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Mean.Scale_NoARS_1FAC.csv", quote = F)
write.csv(format(Mean.Res_2fac_NoARS, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Mean.Scale_NoARS_2FAC.csv", quote = F)
write.csv(format(Mean.Res_test_null.final, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Mean.Scale_Null.csv", quote = F)

