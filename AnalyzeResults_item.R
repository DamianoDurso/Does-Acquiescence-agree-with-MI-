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


# Converged models

item_NoARS_Thr_chi <- NA
item_NoARS_Load_chi <- NA

item_ARS_Thr_chi <- NA
item_ARS_Load_chi <- NA

Conv_item_NoARS_Thr_chi <- NA
Conv_item_NoARS_Load_chi <- NA
Conv_item_ARS_Thr_chi <- NA
Conv_item_ARS_Load_chi <- NA


Results.item <- cbind(simdes, #NoARS_Conf_RMSEA, NoARS_Conf_cfi,
                       item_NoARS_Thr_chi,#NoARS_Thr_RMSEA, NoARS_Thr_cfi, NoARS_Thr_comb,
                       item_NoARS_Load_chi, #NoARS_Load_RMSEA, NoARS_Load_cfi,NoARS_Load_comb,
                       #ARS_Conf_RMSEA, ARS_Conf_cfi, 
                       item_ARS_Thr_chi, #ARS_Thr_RMSEA, ARS_Thr_cfi, ARS_Thr_comb,
                       item_ARS_Load_chi, #ARS_Load_RMSEA, ARS_Load_cfi, ARS_Load_comb,
                      # ARS_vs_noARS, Conv,
                       Conv_item_NoARS_Thr_chi,
                       Conv_item_ARS_Thr_chi,
                       Conv_item_NoARS_Load_chi,
                       Conv_item_ARS_Load_chi
                      )

#simdes12 <- subset(simdes, simdes$j == 12)

for(k in 1:nrow(simdes)){
  load(paste0("ResultsSim_",k,".RData"))  
  
  NoARS_Thr_chi <- matrix(NA, ncol = simdes$j[k]+1, nrow = 100)    
  
  NoARS_Load_chi <- matrix(NA, ncol = simdes$j[k]+1, nrow = 100)    
  
  #ARS RESULTS
  
  ARS_Thr_chi <- matrix(NA, ncol = simdes$j[k]+1, nrow = 100)    
  
  ARS_Load_chi <- matrix(NA, ncol = simdes$j[k]+1, nrow = 100)    
  
########
#Convergence
  Conv_NoARS_Thr_chi <- matrix(NA, ncol = simdes$j[k]+1, nrow = 100)    
  
  Conv_NoARS_Load_chi <- matrix(NA, ncol = simdes$j[k]+1, nrow = 100)    
  
  #ARS RESULTS
  
  Conv_ARS_Thr_chi <- matrix(NA, ncol = simdes$j[k]+1, nrow = 100)    
  
  Conv_ARS_Load_chi <- matrix(NA, ncol = simdes$j[k]+1, nrow = 100)    
  
  
  

  if(simdes$Nfac[k] == 1){ start <- 3 }  else { start <- 4}
  end <- simdes$j[k]+1
  
  for(i in 1:100){

    for(a in start:end){
      
      if(Item_level.models[[i]]$Thr.fit[[a]]$error == "none"){
        NoARS_Thr_chi[i,a] <- Item_level.models[[i]]$Thr.fit[[a]]$value$`Pr(>Chisq)`[2]
      } else {
        NoARS_Thr_chi[i,a] <- NA  
      }
    
      if(Item_level.models[[i]]$Load.fit[[a]]$error == "none"){
        NoARS_Load_chi[i,a] <- Item_level.models[[i]]$Load.fit[[a]]$value$`Pr(>Chisq)`[2]
      } else {
        NoARS_Load_chi[i,a] <- NA  
      }
      
      if(Item_level.models[[i]]$Thr.fit.ARS[[a]]$error == "none"){
        ARS_Thr_chi[i,a] <- Item_level.models[[i]]$Thr.fit.ARS[[a]]$value$`Pr(>Chisq)`[2]
      } else {
        ARS_Thr_chi[i,a] <- NA  
      }
      
      if(Item_level.models[[i]]$Load.fit.ARS[[a]]$error == "none"){
        ARS_Load_chi[i,a] <- Item_level.models[[i]]$Load.fit.ARS[[a]]$value$`Pr(>Chisq)`[2]
      } else {
        ARS_Load_chi[i,a] <- NA  
      }
      
     
      if(Item_level.models[[i]]$Thr.fit[[a]]$error == "none"){
        Conv_NoARS_Thr_chi[i,a] <- "Yes"
      } else {
        Conv_NoARS_Thr_chi[i,a] <- "No"
      }
      
      if(Item_level.models[[i]]$Thr.fit.ARS[[a]]$error == "none"){
        Conv_ARS_Thr_chi [i,a] <- "Yes"
      } else {
        Conv_ARS_Thr_chi[i,a] <- "No"
      }
      
      if(Item_level.models[[i]]$Load.fit[[a]]$error == "none"){
        Conv_NoARS_Load_chi[i,a] <- "Yes"
      } else {
        Conv_NoARS_Load_chi[i,a] <- "No"
      }
      
      if(Item_level.models[[i]]$Load.fit.ARS[[a]]$error == "none"){
        Conv_ARS_Load_chi[i,a] <- "Yes"
      } else {
        Conv_ARS_Load_chi[i,a] <- "No"
      }
      
       
    }   


    #ARS RESULTS

    
    
  } 
  
  
  
  Results.item$item_NoARS_Thr_chi[k] <- c(length(which(NoARS_Thr_chi[,start:end] <= .05))/length(NoARS_Thr_chi[,start:end]))  
  Results.item$item_NoARS_Load_chi[k] <- length(which(NoARS_Load_chi[,start:end] <= .05))/length(NoARS_Load_chi[,start:end])

  Results.item$item_ARS_Thr_chi[k] <- length(which(unlist(ARS_Thr_chi[,start:end]) <= .05))/length(unlist(ARS_Thr_chi[,start:end]))
  Results.item$item_ARS_Load_chi[k] <- length(which(unlist(ARS_Load_chi[,start:end]) <= .05))/length(unlist(ARS_Load_chi[,start:end]))
  
  Results.item$Conv_item_NoARS_Thr_chi[k] <- length(which(Conv_NoARS_Thr_chi[,start:end] == "Yes"))/length(Conv_NoARS_Thr_chi[, start:end])
  Results.item$Conv_item_ARS_Thr_chi[k] <- length(which(Conv_ARS_Thr_chi[,start:end] == "Yes"))/length(Conv_ARS_Thr_chi[, start:end])
  Results.item$Conv_item_NoARS_Load_chi[k] <- length(which(Conv_NoARS_Load_chi[,start:end] == "Yes"))/length(Conv_NoARS_Load_chi[, start:end])
  Results.item$Conv_item_ARS_Load_chi[k] <- length(which(Conv_ARS_Load_chi[,start:end] == "Yes"))/length(Conv_NoARS_Load_chi[, start:end])
  
}


#############################################################################################################
# Create tables

#Results Null MI
item_Res_test_null <- subset(Results.item, Results.item$ARS == 0 )
item_Res_test_null <- cbind(item_Res_test_null[,1:7], round(item_Res_test_null[,8:9],3))

#Results Null convergence
Conv_item_Res_test_null <- subset(Results.item, Results.item$ARS == 0 )
Conv_item_Res_test_null <- cbind(Conv_item_Res_test_null[,1:7], round(Conv_item_Res_test_null[,c(12,14)],3))



# Results MI
item_Res_test <- subset(Results.item, Results.item$ARS != 0 )
item_Res_test <- cbind(item_Res_test[,1:7], round(item_Res_test[,8:11],3))

# Results MI convergence
Conv_item_Res_test <- subset(Results.item, Results.item$ARS != 0 )
Conv_item_Res_test <- cbind(Conv_item_Res_test[,1:7], round(Conv_item_Res_test[,c(12,14,13,15)],3))

#################################################################################################################
# Order tables columns

#Convergence
Conv_Null_item <- Conv_item_Res_test_null[order(Conv_item_Res_test_null$Nfac,
                              Conv_item_Res_test_null$scale, 
                              Conv_item_Res_test_null$sample, 
                              Conv_item_Res_test_null$scale, 
                              Conv_item_Res_test_null$j),]


Conv_ARS_item <- Conv_item_Res_test[order(
                                          Conv_item_Res_test$Nfac,
                                          Conv_item_Res_test$ARS,
                                                Conv_item_Res_test$scale, 
                                                Conv_item_Res_test$sample, 
                                                Conv_item_Res_test$scale, 
                                                Conv_item_Res_test$j),]

Conv_ARS_item_1fac <- subset(Conv_ARS_item, Conv_ARS_item$Nfac == 1)
Conv_ARS_item_2fac <- subset(Conv_ARS_item, Conv_ARS_item$Nfac == 2)


#----------------------------------------------------------------------------------------------------------------
# MI Results

Null_MI_item <- item_Res_test_null[order(item_Res_test_null$Nfac,
                                         item_Res_test_null$scale, 
                                         item_Res_test_null$sample, 
                                         item_Res_test_null$scale, 
                                        item_Res_test_null$j),]


MI_item <-     item_Res_test[order(
                                    item_Res_test$Nfac,
                                    item_Res_test$ARS,
                                    item_Res_test$scale, 
                                    item_Res_test$sample, 
                                    item_Res_test$scale, 
                                    item_Res_test$j),]
                                      

MI_item_1fac <- subset(MI_item, MI_item$Nfac == 1)
MI_item_2fac <- subset(MI_item, MI_item$Nfac == 2)

################################################################################################################
# Save results

write.csv(format(Conv_Null_item , nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Conv.Item_Null.csv", quote = F)
write.csv(format(Conv_ARS_item_1fac,nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Conv.Item_ARS_1FAC.csv", quote = F)
write.csv(format(Conv_ARS_item_2fac, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Conv.Item_ARS_2FAC.csv", quote = F)
write.csv(format(Null_MI_item, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Item_Null.csv", quote = F)
write.csv(format(MI_item_1fac,nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Item_ARS_1FAC.csv", quote = F)
write.csv(format(MI_item_2fac, nsmall=3), "C:/Users/eddurso/Desktop/Projects/D'Urso MI and RS/Response styles and MI/Scripts/Tables/Item_ARS_2FAC.csv", quote = F)



  

