#test values
#Nfac <- 1
#seed <- 090493
#DIF <- F
#DIFsize <- 0
#sample <- 250
#nitems <- 12
#cat <- 3
#ARS <- 0
#nrep <- 1
#scale <- "balanced"
#######################################################################################################
#
#
#                                          Functions file                 
# 
#######################################################################################################

##########################################################################################################
myTryCatch <- function(expr) {
  warn <- err <- "none"                                                     # if no errors occur, let warnings and errors be "none"
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e                                                             # if an error occurs, save it in err
    }), warning=function(w) {
      warn <<- w                                                            # if a warning occurs, save it in warn
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)                                
}



# Data cut
#FUNCTION TO CUT THE DATA AND MAKE THEM ORDINAL

Cuts_Data <- function(x,thresh){
  
  Datacut <- list()
  xordered <- x
  for (i in 1:ncol(x)){
    x[,i] <- as.numeric(cut(x[,i], breaks = c(-Inf, thresh[i,], +Inf))) 
    xordered[,i] <- ordered(x[,i])
  }
  Datacut$numeric <- x
  Datacut$ordered <- xordered
  return(Datacut)  
}



################################################# Data Generation ##########################################
GenSimulationStudy <- function(seed, Nfac, DIF, DIFsizeload, DIFsizeThr, sample, scale, nitems, cat, ARS, nrep){

  
#Generate parameters G1
# Generate factor loadings
  if(Nfac == 1){
    #First Generate the matrix of factor loadings/discrimnation parameter  
    if(ARS == 0){
      if(scale == "balanced"){
        a <- matrix(c(rep(.6,nitems/2), rep(-.6,nitems/2)), ncol = 1, nrow = nitems)
      } else if(scale == "unbalanced") {
        a <- matrix(c(rep(.6,nitems/2), rep(.6,nitems/2)), ncol = 1, nrow = nitems)
        } else {
        a <- matrix(c(rep(.6,nitems/2), rep(-.6,nitems/2)), ncol = 1, nrow = nitems)
        a[7:8,] <- .6
      }
      
    }  else {
      if(scale == "balanced"){
        a <- matrix(c(rep(.6,nitems/2), rep(-.6, nitems/2), 
                      rep(ARS,nitems)), 
                    ncol = 2, nrow = nitems)
      } else if (scale == "unbalanced") {
            a <- matrix(c(rep(.6,nitems/2), rep(.6, nitems/2), 
                      rep(ARS,nitems)), 
                    ncol = 2, nrow = nitems)
        
      } else  {
        a <- matrix(c(rep(.6,nitems/2), rep(-.6, nitems/2), 
                      rep(ARS,nitems)), 
                    ncol = 2, nrow = nitems)
        a[7:8,1] <- .6
      }
    }
    
  } else {
    
  if(ARS == 0){
    if(nitems == 12){
      if(scale == "balanced"){
        a <- matrix(c(rep(c(.6,0,-.6,0),3), rep(c(0,.6,0,-.6),3)),
                    ncol=2)
      } else if (scale == "unbalanced") {
        a <- matrix(c(rep(c(.6,0,.6,0),3), rep(c(0,.6,0,.6),3)),
                    ncol=2)
        
      }else{
        a <- matrix(c(rep(c(.6,0,-.6,0),3), rep(c(0,.6,0,-.6),3)),
                    ncol=2)
        a[3,1] <- .6
        a[4,2] <- .6
      }
      
    } else {
      if(scale == "balanced"){
        a <- matrix(c(rep(c(.6,0,-.6,0),6), rep(c(0,.6,0,-.6),6)), 
                    ncol=2)
      } else if (scale == "unbalanced") {
        a <- matrix(c(rep(c(.6,0,.6,0),6), rep(c(0,.6,0,.6),6)), 
                    ncol=2)
      } else {
        a <- matrix(c(rep(c(.6,0,-.6,0),6), rep(c(0,.6,0,-.6),6)), 
                    ncol=2)
        a[c(3,15),1] <- .6
        a[c(4,16),2] <- .6
      }
    }
    
  }  else {
    if(nitems == 12){
      if(scale == "balanced"){
        a <- matrix(c(rep(c(.6,0,-.6,0),3), rep(c(0,.6,0,-.6),3), rep(ARS, nitems)),
                    ncol=3)
      } else if (scale == "unbalanced"){
        a <- matrix(c(rep(c(.6,0,.6,0),3), rep(c(0,.6,0,.6),3), rep(ARS, nitems)),
                    ncol=3)
        
      } else {
        a <- matrix(c(rep(c(.6,0,-.6,0),3), rep(c(0,.6,0,-.6),3), rep(ARS, nitems)),
                    ncol=3)
        a[3,1] <- .6
        a[4,2] <- .6
      }
      
    } else {
      if(scale == "balanced"){
        a <- matrix(c(rep(c(.6,0,-.6,0),6), rep(c(0,.6,0,-.6),6), rep(ARS, nitems)), 
                    ncol=3)
      } else if (scale == "unbalanced") {
        a <- matrix(c(rep(c(.6,0,.6,0),6), rep(c(0,.6,0,.6),6), rep(ARS, nitems)), 
                    ncol=3)
        
      } else {
        a <- matrix(c(rep(c(.6,0,-.6,0),6), rep(c(0,.6,0,-.6),6), rep(ARS, nitems)), 
                    ncol=3)
        a[c(3,15),1] <- .6
        a[c(4,16),2] <- .6
      }
    }  
  }
}  

  


# Generate thresholds
  
  #Distance across categories 
  if(cat==3){
    distance <- 2
  } else if(cat==5){
    distance <- 0.5
  } else {
    distance <- 1.25
  }
  
  diffs <- t(apply(matrix(runif(nitems*(cat-1), distance, distance),
                          nitems), 1, cumsum))
  diffs <- -(diffs - rowMeans(diffs)) #thresholds without shifts
  d <- diffs + c(seq(-1, 1, (2/11)))  #add shift to thresholds
  
  
  #Generate parameters for group 2
  
  #If there is no DIF
  if( DIF == "none"){
    a1 <- a
    d1 <- d
    #If there is DIF on thresholds
  } else if (DIF == "thresholds") {
    if(nitems == 12){
      d1 <- d
      d1[c(1,2,9,10),] <- d1[c(1,2,9,10),] + DIFsizeThr
    } else {
      d1 <- d
      d1[c(1,2,9,10,13,14,21,22),] <- d1[c(1,2,9,10,13,14,21,22),] + DIFsizeThr
    }
    #If there is DIF on loadings
  } else if (DIF == "loadings") { 
    if(Nfac == 1){
      if(nitems == 12){
        a1 <- a
        a1[c(1,2),] <- a[c(1,2),] - DIFsizeload
        
        if(scale == "balanced"){
          a1[c(9,10),] <- a[c(9,10),] + DIFsizeload
        } else {
          a1[c(9,10),] <- a[c(9,10),] - DIFsizeload
        }
        d1 <- d
      } else {
        a1 <- a
        a1[c(1,2,13,14),] <- a[c(1,2,13,14),] - DIFsizeload
        if(scale == "balanced"){
          a1[c(9,10,21,22),] <- a[c(9,10,21,22),] + DIFsizeload
        } else {
          a1[c(9,10,21,22),] <- a[c(9,10,21,22),] - DIFsizeload
        }
        d1 <- d
      }
    } else {
      if(nitems == 12){
        a1 <- a
        a1[1,1] <- a[1,1] - DIFsizeload
        a1[2,2] <- a[2,2] - DIFsizeload
        if(scale == "balanced") {
          a1[9,1] <- a[9,1] + DIFsizeload
          a1[10,2] <- a[10,2] + DIFsizeload
        } else {
          a1[9,1] <- a[9,1] - DIFsizeload
          a1[10,2] <- a[10,2] - DIFsizeload
        }
        d1 <- d
      } else {
        a1 <- a
        a1[c(1,13),1] <- a[c(1,13),1] - DIFsizeload
        a1[c(2,14),2] <- a[c(2,14),2] - DIFsizeload
        
        if(scale == "balanced"){ 
          a1[c(9,21),1] <- a[c(9,21),1] + DIFsizeload
          a1[c(10,22),2] <- a[c(10,22),2] + DIFsizeload
        } else {
          a1[c(9,21),1] <- a[c(9,21),1] - DIFsizeload
          a1[c(10,22),2] <- a[c(10,22),2] - DIFsizeload
        }
        d1 <- d
      }
    }
    #If there is DIF on thresholds and loadings
  } else {
    if(Nfac == 1){
      if(nitems == 12){
        a1 <- a
        a1[c(1,2),] <- a[c(1,2),] - DIFsizeload
        
        if(scale == "balanced"){
          a1[c(9,10),] <- a[c(9,10),] + DIFsizeload
        } else {
          a1[c(9,10),] <- a[c(9,10),] - DIFsizeload
        }
        d1 <- d
        d1[c(1,2,9,10),] <- d1[c(1,2,9,10),] + DIFsizeThr
        
      } else {
        a1 <- a
        a1[c(1,2,13,14),] <- a[c(1,2,13,14),] - DIFsizeload
        if(scale == "balanced"){
          a1[c(9,10,21,22),] <- a[c(9,10,21,22),] + DIFsizeload
        } else {
          a1[c(9,10,21,22),] <- a[c(9,10,21,22),] - DIFsizeload
        }
        d1 <- d
        d1[c(1,2,9,10,13,14,21,22),] <- d1[c(1,2,9,10,13,14,21,22),] + DIFsizeThr
        
      }
    } else {
      if(nitems == 12){
        a1 <- a
        a1[1,1] <- a[1,1] - DIFsizeload
        a1[2,2] <- a[2,2] - DIFsizeload
        if(scale == "balanced") {
          a1[9,1] <- a[9,1] + DIFsizeload
          a1[10,2] <- a[10,2] + DIFsizeload
        } else {
          a1[9,1] <- a[9,1] - DIFsizeload
          a1[10,2] <- a[10,2] - DIFsizeload
        }
        d1 <- d
        d1[c(1,2,9,10),] <- d1[c(1,2,9,10),] + DIFsizeThr
        
      } else {
        a1 <- a
        a1[c(1,13),1] <- a[c(1,13),1] - DIFsizeload
        a1[c(2,14),2] <- a[c(2,14),2] - DIFsizeload
        
        if(scale == "balanced"){ 
          a1[c(9,21),1] <- a[c(9,21),1] + DIFsizeload
          a1[c(10,22),2] <- a[c(10,22),2] + DIFsizeload
        } else {
          a1[c(9,21),1] <- a[c(9,21),1] - DIFsizeload
          a1[c(10,22),2] <- a[c(10,22),2] - DIFsizeload
        }
        d1 <- d
        d1[c(1,2,9,10,13,14,21,22),] <- d1[c(1,2,9,10,13,14,21,22),] + DIFsizeThr
      }
    }
  }
  

  
  
  

#Generate factor scores  
set.seed(seed)  
dat <- list()
for(k in 1:nrep){  
  #create the factor scores and unique variances
  if(ARS == 0){
  #  set.seed(seed)
    theta.1 <- rmvnorm(sample, mean = rep(0, Nfac), sigma = diag(Nfac))  
    theta.2 <- rmvnorm(sample, mean = rep(0, Nfac), sigma = diag(Nfac))  
    errorvar <- 1- rowSums(a^2) # error variance 
    unique <- diag(errorvar) # matrix of error variances
    epsilon.1 <- mvrnorm(sample, rep(0, nitems), Sigma = unique)						          # residual scores group 1 and 2
    epsilon.2 <- mvrnorm(sample, rep(0, nitems), Sigma = unique)						          # residual scores group 1 and 2
    
  } else {
  #  set.seed(seed)
    theta.1 <- rmvnorm(sample, mean = rep(0, Nfac), sigma = diag(Nfac))  
    theta.2 <- rmvnorm(sample, mean = rep(0, Nfac), sigma = diag(Nfac))  
    #ARS distribution right-censored
    thetaRS <- rnorm(sample, 0, 1)
    for(j in 1:length(thetaRS)){
      if (thetaRS[j] <= 0){
        thetaRS[j] <- 0
      } else {
        NULL
      }
    }
    #hist(thetaRS)
    theta.2 <- cbind(theta.2, thetaRS)
    if(Nfac == 1){
      errorvar <- 1- (a[,1]^2) # error variance 
    } else {
      errorvar <- 1- rowSums(a[,1:Nfac]^2) # error variance 
    }

    unique <- diag(errorvar) # matrix of error variances
    epsilon.1 <- mvrnorm(sample, rep(0, nitems), Sigma = unique)						          # residual scores group 1 and 2
    
    errorvarARS <- 1- rowSums(a1^2) # error variance 
    uniqueARS <- diag(errorvarARS) # matrix of error variances
    epsilon.2 <- mvrnorm(sample, rep(0, nitems), Sigma = uniqueARS)						          # residual scores group 1 and 2
  }
  
  #Generate data 
  if(ARS == 0){
    df    <- as.data.frame(rep(0, nitems) + theta.1  %*% t(a[,1:Nfac]) + epsilon.1) 					    # measurement equations group 1 and 2
    dfARS <- as.data.frame(rep(0, nitems) + theta.2  %*% t(a1[,1:Nfac]) + epsilon.2)
  } else {
    df    <- as.data.frame(rep(0, nitems) + theta.1  %*% t(a[,1:Nfac]) + epsilon.1) 					    # measurement equations group 1 and 2
    dfARS <- as.data.frame(rep(0, nitems) + theta.2  %*% t(a1) + epsilon.2)
  }
  
  
  #make data ordinal 
  df    <- Cuts_Data(df, d)
  dfARS <- Cuts_Data(dfARS, d1)
  
  group <- c(rep(1, nrow(df$numeric)), rep(2,nrow(dfARS$numeric)))
  dat[[k]] <- rbind(df$numeric,dfARS$numeric)
  dat[[k]]$group <- group
}


#apply(dat[[1]], 2, function(y) length(unique(y[1:250])) == length(unique(y[(sample+1):nrow(dat[[1]])])))

remove <- NULL
for(b in 1:length(dat)){
  eq.unique <- NA
  for(j in 1:nitems){
  eq.unique[j] <- length(unique(dat[[b]][1:sample, j])) ==  length(unique(dat[[b]][(sample+1):nrow(dat[[b]]),j]))
  }  
  if (all(eq.unique == T)){
    NULL
  } else {
    remove[b] <- b
  }
}


#remove <- NULL    
#for(b in 1:length(dat)){
#  if(all(apply(dat[[b]], 2, function(y) length(unique(y[1:250])) == length(unique(y[(sample+1):nrow(dat[[b]])]))))){
#    NULL
#  } else {
#    remove[b] <- b 
#  }
#}


if(is.null(remove)){
  NULL
} else {
  nas <- which(is.na(remove))
  remove <- remove[-c(nas)]
  dat <- dat[-c(remove)]
}

results <- list(dat, a[,1:Nfac], a1, d, d1)
names(results) <- c('Data', "loadingsG1", "loadingsG2", "thresholdsG1", "thresholdsG2")

return(results)
}
  
  
##-------------------------------------------------------------------------------------------------------##  


################################################# Model Creation ##########################################

library(semTools)

MI_Models <- function(Nfac, nitems, ARS, Data){
  
  
#Create the models 
  
 if(Nfac == 1){
   if(nitems == 12){
     if( ARS == 0){
       Mod <- 'F1 =~ V1 + V2 + V3 + V4 +  V5 + V6 +  V7 + V8 +  V9 +  V10 +  V11 +  V12'    
     } else {
       Mod <- 'F1 =~ V1 + V2 + V3 + V4 +  V5 + V6 +  V7 + V8 +  V9 +  V10 +  V11 +  V12
               ARS =~ 1*V1 + 1*V2 + 1*V3 + 1*V4 + 1*V5 + 1*V6 +  1*V7 + 1*V8 +  1*V9 +  1*V10 +  1*V11 +  1*V12
       '  
     }
   } else {
     if(ARS == 0){
       Mod <- 'F1 =~ V1 + V2 + V3 + V4 +  V5 + V6 +  V7 + V8 +  V9 +  V10 +  V11 +  V12+ 
                     V13 + V14 + V15 + V16 +  V17 + V18 +  V19 + V20 +  V21 +  V22 +  V23 +  V24
       '    
     } else {
       Mod <- 'F1 =~ F1 =~ V1 + V2 + V3 + V4 +  V5 + V6 +  V7 + V8 +  V9 +  V10 +  V11 +  V12+ 
                     V13 + V14 + V15 + V16 +  V17 + V18 +  V19 + V20 +  V21 +  V22 +  V23 +  V24
               ARS =~ 1*V1 + 1*V2 + 1*V3 + 1*V4 + 1*V5 + 1*V6 +  1*V7 + 1*V8 +  1*V9 +  1*V10 +  1*V11 +  1*V12
                      1*V13 + 1*V14 + 1*V15 + 1*V16 + 1*V17 + 1*V18 +  1*V19 + 1*V20 +  1*V21 +  1*V22 +  1*V23 +  1*V24
       '  
     }
   }
 } else {
   if(nitems == 12){
     if( ARS == 0){
       Mod <- 'F1 =~ V1 +  V3  +  V5 +  V7 + V9 + V11 
               F2 =~ V2 +  V4  +  V6 +  V8 + V10 + V12
       '    
     } else {
       Mod <- 'F1 =~ V1 +  V3  +  V5 +  V7 + V9 + V11 
               F2 =~ V2 +  V4  +  V6 +  V8 + V10 + V12
               ARS =~ 1*V1 + 1*V2 + 1*V3 + 1*V4 + 1*V5 + 1*V6 +  1*V7 + 1*V8 +  1*V9 +  1*V10 +  1*V11 +  1*V12
       '  
     }
   } else {
     if(ARS == 0){
       Mod <- 'F1 =~ V1 +  V3  +  V5 +  V7 + V9 + V11 + V13 + V15 + V17 + V19 + V21 + V23
               F2 =~ V2 +  V4  +  v6 +  V8 + V10 + V12 + V14 + V16 + V18 + V20 + V22 + V24
       '    
     } else {
       Mod <- 'F1 =~ V1 +  V3  +  V5 +  V7 + V9 + V11 + V13 + V15 + V17 + V19 + V21 + V23
               F2 =~ V2 +  V4  +  v6 +  V8 + V10 + V12 + V14 + V16 + V18 + V20 + V22 + V24
               ARS =~ 1*V1 + 1*V2 + 1*V3 + 1*V4 + 1*V5 + 1*V6 +  1*V7 + 1*V8 +  1*V9 +  1*V10 +  1*V11 +  1*V12
                      1*V13 + 1*V14 + 1*V15 + 1*V16 + 1*V17 + 1*V18 +  1*V19 + 1*V20 +  1*V21 +  1*V22 +  1*V23 +  1*V24
       '  
     }
   }
}   
  
#Create lavaan syntax and estimate  

#Configural
  
  Conf.mod <- measEq.syntax(configural.model =  Mod,
                            dat = Data,
                            parameterization = "delta",
                            ordered = colnames(Data[,-c(dim(Data)[2])]),
                            ID.fac = "std.lv",
                            ID.cat = "Wu",
                            group = "group",
                            group.equal = "configural")
 
   
  fit.conf <- cfa(as.character(Conf.mod), 
                  data = Data, 
                  group = "group", 
                  ordered = colnames(Data[,-c(dim(Data)[2])]))  
  

#Thresholds invariant (prop. 4 Wu and Estabrook, 2016)    
 Thr.Mod <- measEq.syntax(configural.model =  Mod,
                            dat = Data,
                            parameterization = "delta",
                            ordered = colnames(Data[,-c(dim(Data)[2])]),
                            ID.fac = "std.lv",
                            ID.cat = "Wu",
                            group = "group",
                            group.equal = c("thresholds"))

 fit.thr <- cfa(as.character(Thr.Mod), 
                 data = Data, 
                 group = "group", 
                 ordered = colnames(Data[,-c(dim(Data)[2])]))  
 
#Thresholds and loadings invariant (prop. 7 Wu and Estabrook, 2016)    
 Load.Mod <- measEq.syntax(configural.model =  Mod,
                          dat = Data,
                          parameterization = "delta",
                          ordered = colnames(Data[,-c(dim(Data)[2])]),
                          ID.fac = "std.lv",
                          ID.cat = "Wu",
                          group = "group",
                          group.equal = c("thresholds", "loadings"))
 
 fit.load <- cfa(as.character(Load.Mod), 
                data = Data, 
                group = "group", 
                ordered = colnames(Data[,-c(dim(Data)[2])]))  

 
 
 
 Results.fit <- list()
 
Results.fit[[1]] <- fitmeasures(fit.conf)
Results.fit[[2]] <- fitmeasures(fit.thr)
Results.fit[[3]] <- fitmeasures(fit.load)



names(Results.fit) <- c('Conf.fit', 'Thr.fit', 'Load.fit')

return(Results.fit)
}

MI_Models.ARS <- function(Nfac, nitems, ARS, Data){
  
  
  #Create the models 
  
  if(Nfac == 1){
    if(nitems == 12){
        Mod <- 'F1 =~ V1 + V2 + V3 + V4 +  V5 + V6 +  V7 + V8 +  V9 +  V10 +  V11 +  V12'    
        ModARS <- 'F1 =~ V1 + V2 + V3 + V4 +  V5 + V6 +  V7 + V8 +  V9 +  V10 +  V11 +  V12
               ARS =~ 1*V1 + 1*V2 + 1*V3 + 1*V4 + 1*V5 + 1*V6 +  1*V7 + 1*V8 +  1*V9 +  1*V10 +  1*V11 +  1*V12
       '  
      
    } else {
        Mod <- 'F1 =~ V1 + V2 + V3 + V4 +  V5 + V6 +  V7 + V8 +  V9 +  V10 +  V11 +  V12+ 
                     V13 + V14 + V15 + V16 +  V17 + V18 +  V19 + V20 +  V21 +  V22 +  V23 +  V24
       '    
        ModARS <- 'F1 =~ F1 =~ V1 + V2 + V3 + V4 +  V5 + V6 +  V7 + V8 +  V9 +  V10 +  V11 +  V12+ 
                     V13 + V14 + V15 + V16 +  V17 + V18 +  V19 + V20 +  V21 +  V22 +  V23 +  V24
               ARS =~ 1*V1 + 1*V2 + 1*V3 + 1*V4 + 1*V5 + 1*V6 +  1*V7 + 1*V8 +  1*V9 +  1*V10 +  1*V11 +  1*V12
                      1*V13 + 1*V14 + 1*V15 + 1*V16 + 1*V17 + 1*V18 +  1*V19 + 1*V20 +  1*V21 +  1*V22 +  1*V23 +  1*V24
       '  
      }
  } else {
    if(nitems == 12){
        Mod <- 'F1 =~ V1 +  V3  +  V5 +  V7 + V9 + V11 
               F2 =~ V2 +  V4  +  V6 +  V8 + V10 + V12
       '    
        ModARS <- 'F1 =~ V1 +  V3  +  V5 +  V7 + V9 + V11 
               F2 =~ V2 +  V4  +  V6 +  V8 + V10 + V12
               ARS =~ 1*V1 + 1*V2 + 1*V3 + 1*V4 + 1*V5 + 1*V6 +  1*V7 + 1*V8 +  1*V9 +  1*V10 +  1*V11 +  1*V12
       '  
    } else {
        Mod <- 'F1 =~ V1 +  V3  +  V5 +  V7 + V9 + V11 + V13 + V15 + V17 + V19 + V21 + V23
               F2 =~ V2 +  V4  +  v6 +  V8 + V10 + V12 + V14 + V16 + V18 + V20 + V22 + V24
       '    
        ModARS <- 'F1 =~ V1 +  V3  +  V5 +  V7 + V9 + V11 + V13 + V15 + V17 + V19 + V21 + V23
               F2 =~ V2 +  V4  +  v6 +  V8 + V10 + V12 + V14 + V16 + V18 + V20 + V22 + V24
               ARS =~ 1*V1 + 1*V2 + 1*V3 + 1*V4 + 1*V5 + 1*V6 +  1*V7 + 1*V8 +  1*V9 +  1*V10 +  1*V11 +  1*V12
                      1*V13 + 1*V14 + 1*V15 + 1*V16 + 1*V17 + 1*V18 +  1*V19 + 1*V20 +  1*V21 +  1*V22 +  1*V23 +  1*V24

       '  
      }
    }

  
  #Create lavaan syntax and estimate  
  
  #Configural
  
  Conf.mod <- measEq.syntax(configural.model =  Mod,
                            dat = Data,
                            parameterization = "delta",
                            ordered = colnames(Data[,-c(dim(Data)[2])]),
                            ID.fac = "std.lv",
                            ID.cat = "Wu",
                            group = "group",
                            group.equal = "configural")

  Conf.modARS <- measEq.syntax(configural.model =  ModARS,
                            dat = Data,
                            parameterization = "delta",
                            ordered = colnames(Data[,-c(dim(Data)[2])]),
                            ID.fac = "std.lv",
                            ID.cat = "Wu",
                            group = "group",
                            group.equal = "configural")
  
  
  fit.conf <- cfa(as.character(Conf.mod), 
                  data = Data, 
                  group = "group", 
                  ordered = colnames(Data[,-c(dim(Data)[2])]),
                  estimator = "WLSMV")  
  
  fit.confARS <- cfa(as.character(Conf.modARS), 
                  data = Data, 
                  group = "group", 
                  ordered = colnames(Data[,-c(dim(Data)[2])]),
                  estimator = "WLSMV")  
  
  #Thresholds invariant (prop. 4 Wu and Estabrook, 2016)    
  Thr.Mod <- measEq.syntax(configural.model =  Mod,
                           dat = Data,
                           parameterization = "delta",
                           ordered = colnames(Data[,-c(dim(Data)[2])]),
                           ID.fac = "std.lv",
                           ID.cat = "Wu",
                           group = "group",
                           group.equal = c("thresholds"))

  Thr.ModARS <- measEq.syntax(configural.model =  ModARS,
                           dat = Data,
                           parameterization = "delta",
                           ordered = colnames(Data[,-c(dim(Data)[2])]),
                           ID.fac = "std.lv",
                           ID.cat = "Wu",
                           group = "group",
                           group.equal = c("thresholds"))
  
  fit.thr <- cfa(as.character(Thr.Mod), 
                 data = Data, 
                 group = "group", 
                 ordered = colnames(Data[,-c(dim(Data)[2])]),
                 estimator = "WLSMV")  
  fit.thrARS <- cfa(as.character(Thr.ModARS), 
                 data = Data, 
                 group = "group", 
                 ordered = colnames(Data[,-c(dim(Data)[2])]),
                 estimator = "WLSMV")  
  
  
  #Thresholds and loadings invariant (prop. 7 Wu and Estabrook, 2016)    
  Load.Mod <- measEq.syntax(configural.model =  Mod,
                            dat = Data,
                            parameterization = "delta",
                            ordered = colnames(Data[,-c(dim(Data)[2])]),
                            ID.fac = "std.lv",
                            ID.cat = "Wu",
                            group = "group",
                            group.equal = c("thresholds", "loadings"))

  Load.ModARS <- measEq.syntax(configural.model =  ModARS,
                            dat = Data,
                            parameterization = "delta",
                            ordered = colnames(Data[,-c(dim(Data)[2])]),
                            ID.fac = "std.lv",
                            ID.cat = "Wu",
                            group = "group",
                            group.equal = c("thresholds", "loadings"))
  
  fit.load <- cfa(as.character(Load.Mod), 
                  data = Data, 
                  group = "group", 
                  ordered = colnames(Data[,-c(dim(Data)[2])]),
                  estimator = "WLSMV")  
  
  fit.loadARS <- cfa(as.character(Load.ModARS), 
                  data = Data, 
                  group = "group", 
                  ordered = colnames(Data[,-c(dim(Data)[2])]),
                  estimator = "WLSMV")  
  
  
  
  Models <- list()
  Models[[1]] <- fit.conf
  Models[[2]] <- fit.load
  Models[[3]] <- fit.thr
  Models[[4]] <- fit.confARS
  Models[[5]] <- fit.loadARS
  Models[[6]] <- fit.thrARS
  
  names(Models) <- c('Mod.Conf', 'Mod.Thr', 'Mod.Load', 
                          'Mod.ConfARS', 'Mod.ThrARS', 'Mod.LoadARS')
  
  Results.fit <- list()
  
  
  
  Results.fit[[1]] <- fitmeasures(fit.conf)
  Results.fit[[2]] <- fitmeasures(fit.thr)
  Results.fit[[3]] <- fitmeasures(fit.load)
  Results.fit[[4]] <- fitmeasures(fit.confARS)
  Results.fit[[5]] <- fitmeasures(fit.thrARS)
  Results.fit[[6]] <- fitmeasures(fit.loadARS)
  
  
  
  names(Results.fit) <- c('Conf.fit', 'Thr.fit', 'Load.fit', 
                          'Conf.fitARS', 'Thr.fitARS', 'Load.fitARS')
  
  
  results <- c(Models, Results.fit)
  
  return(results)
}

# Model generatio code for the D'Urso et al. 2022 simulation study

# In this simulation study, the ARS factor scores are generated from a right-censored normal distribution
# to only captures subjects with an agreeeing tendency (values > 0 on the factor scores distribution).
# Therefore, fixing the factor variance to 1 when identifying the variance of a latent factor will strongly 
# detriment the fit of the model, since the variance of a right-censored distribution << 1. A possible solution
# is to fix the loading of a certain item to 1.

# In Billiet and McClendon's Model for ARS (2000), the authors suggest to identify the ARS factor
# by fixing all its loadings to be 1 and equal. However, when using the meas.eqSyntax function to 
# generate constraints according to Wu and Estabrook (2016) to test for MI in ordered-categorical 
# data, fixing a loading to 1 and identify the factor variance to 1 is not possible because the following error occurs:
# "The intercept of indicator "V1" can only be fixed to zero in order to identify one latent mean, 
# but it is specified as the first indicator of the following factors: F1, ARS". The reason is that this function identifies
# 1*(loading) as using that loading to identify a latent factor, whereas, in our case, this is the actual loading value
# that we are imputing. 

# Therefore, to estimate the model, it is necessary to specify the model, and the Meas.Eq syntax function cannot be used.



#------------------------------------------------------------------------------------------------------------------#

# Generate Models according to Wu and Estabrook constraints
#First we will specify the general model 
# The we will create the syntax for a (i) configural invariant, (ii) thresholds invariant and (iii) loadings invariant
# model, and finally we will change the identification constraint on the variance from the factor to the loading of 1 item

Model_Creation <- function(Nfac, nitems, scale = T){
  
  if(scale == T){
    if(Nfac == 1){
      if(nitems == 12){
        ConfModnoARS <- '
       ## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1.g1, lambda.2_1.g2)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1.g1, lambda.3_1.g2)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1.g1, lambda.4_1.g2)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1.g1, lambda.5_1.g2)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1.g1, lambda.6_1.g2)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1.g1, lambda.7_1.g2)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1.g1, lambda.8_1.g2)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1.g1, lambda.9_1.g2)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1.g1, lambda.10_1.g2)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1.g1, lambda.11_1.g2)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1.g1, lambda.12_1.g2)*V12

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1.g1, V1.thr1.g2)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2.g1, V1.thr2.g2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3.g1, V1.thr3.g2)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4.g1, V1.thr4.g2)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1.g1, V2.thr1.g2)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2.g1, V2.thr2.g2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3.g1, V2.thr3.g2)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4.g1, V2.thr4.g2)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1.g1, V3.thr1.g2)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2.g1, V3.thr2.g2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3.g1, V3.thr3.g2)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4.g1, V3.thr4.g2)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1.g1, V4.thr1.g2)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2.g1, V4.thr2.g2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3.g1, V4.thr3.g2)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4.g1, V4.thr4.g2)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1.g1, V5.thr1.g2)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2.g1, V5.thr2.g2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3.g1, V5.thr3.g2)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4.g1, V5.thr4.g2)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1.g1, V6.thr1.g2)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2.g1, V6.thr2.g2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3.g1, V6.thr3.g2)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4.g1, V6.thr4.g2)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1.g1, V7.thr1.g2)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2.g1, V7.thr2.g2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3.g1, V7.thr3.g2)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4.g1, V7.thr4.g2)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1.g1, V8.thr1.g2)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2.g1, V8.thr2.g2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3.g1, V8.thr3.g2)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4.g1, V8.thr4.g2)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1.g1, V9.thr1.g2)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2.g1, V9.thr2.g2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3.g1, V9.thr3.g2)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4.g1, V9.thr4.g2)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1.g1, V10.thr1.g2)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2.g1, V10.thr2.g2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3.g1, V10.thr3.g2)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4.g1, V10.thr4.g2)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1.g1, V11.thr1.g2)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2.g1, V11.thr2.g2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3.g1, V11.thr3.g2)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4.g1, V11.thr4.g2)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1.g1, V12.thr1.g2)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2.g1, V12.thr2.g2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3.g1, V12.thr3.g2)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4.g1, V12.thr4.g2)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, 0)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, 0)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, 0)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, 0)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, 0)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, 0)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, 0)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, 0)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, 0)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, 0)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, 0)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, 0)*1

## SCALING FACTORS:

V1 ~*~ c(1, 1)*V1
V2 ~*~ c(1, 1)*V2
V3 ~*~ c(1, 1)*V3
V4 ~*~ c(1, 1)*V4
V5 ~*~ c(1, 1)*V5
V6 ~*~ c(1, 1)*V6
V7 ~*~ c(1, 1)*V7
V8 ~*~ c(1, 1)*V8
V9 ~*~ c(1, 1)*V9
V10 ~*~ c(1, 1)*V10
V11 ~*~ c(1, 1)*V11
V12 ~*~ c(1, 1)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

       ' 
        ThrModnoARS <- '
 ## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1.g1, lambda.2_1.g2)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1.g1, lambda.3_1.g2)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1.g1, lambda.4_1.g2)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1.g1, lambda.5_1.g2)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1.g1, lambda.6_1.g2)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1.g1, lambda.7_1.g2)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1.g1, lambda.8_1.g2)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1.g1, lambda.9_1.g2)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1.g1, lambda.10_1.g2)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1.g1, lambda.11_1.g2)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1.g1, lambda.12_1.g2)*V12

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V2 ~*~ c(1, NA)*V2
V3 ~*~ c(1, NA)*V3
V4 ~*~ c(1, NA)*V4
V5 ~*~ c(1, NA)*V5
V6 ~*~ c(1, NA)*V6
V7 ~*~ c(1, NA)*V7
V8 ~*~ c(1, NA)*V8
V9 ~*~ c(1, NA)*V9
V10 ~*~ c(1, NA)*V10
V11 ~*~ c(1, NA)*V11
V12 ~*~ c(1, NA)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:
 '       
        
        LoadModnoARS <- '
## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1, lambda.2_1)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1, lambda.3_1)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1, lambda.4_1)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1, lambda.5_1)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1, lambda.6_1)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1, lambda.7_1)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1, lambda.8_1)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1, lambda.9_1)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1, lambda.10_1)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1, lambda.11_1)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1, lambda.12_1)*V12

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V2 ~*~ c(1, NA)*V2
V3 ~*~ c(1, NA)*V3
V4 ~*~ c(1, NA)*V4
V5 ~*~ c(1, NA)*V5
V6 ~*~ c(1, NA)*V6
V7 ~*~ c(1, NA)*V7
V8 ~*~ c(1, NA)*V8
V9 ~*~ c(1, NA)*V9
V10 ~*~ c(1, NA)*V10
V11 ~*~ c(1, NA)*V11
V12 ~*~ c(1, NA)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:
'       
        
        ConfModARS <- '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1.g1, lambda.2_1.g2)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1.g1, lambda.3_1.g2)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1.g1, lambda.4_1.g2)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1.g1, lambda.5_1.g2)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1.g1, lambda.6_1.g2)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1.g1, lambda.7_1.g2)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1.g1, lambda.8_1.g2)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1.g1, lambda.9_1.g2)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1.g1, lambda.10_1.g2)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1.g1, lambda.11_1.g2)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1.g1, lambda.12_1.g2)*V12
ARS =~ c(1, 1)*V1 + c(lambda.1_2.g1, lambda.1_2.g2)*V1
ARS =~ c(1, 1)*V2 + c(lambda.2_2.g1, lambda.2_2.g2)*V2
ARS =~ c(1, 1)*V3 + c(lambda.3_2.g1, lambda.3_2.g2)*V3
ARS =~ c(1, 1)*V4 + c(lambda.4_2.g1, lambda.4_2.g2)*V4
ARS =~ c(1, 1)*V5 + c(lambda.5_2.g1, lambda.5_2.g2)*V5
ARS =~ c(1, 1)*V6 + c(lambda.6_2.g1, lambda.6_2.g2)*V6
ARS =~ c(1, 1)*V7 + c(lambda.7_2.g1, lambda.7_2.g2)*V7
ARS =~ c(1, 1)*V8 + c(lambda.8_2.g1, lambda.8_2.g2)*V8
ARS =~ c(1, 1)*V9 + c(lambda.9_2.g1, lambda.9_2.g2)*V9
ARS =~ c(1, 1)*V10 + c(lambda.10_2.g1, lambda.10_2.g2)*V10
ARS =~ c(1, 1)*V11 + c(lambda.11_2.g1, lambda.11_2.g2)*V11
ARS =~ c(1, 1)*V12 + c(lambda.12_2.g1, lambda.12_2.g2)*V12

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1.g1, V1.thr1.g2)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2.g1, V1.thr2.g2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3.g1, V1.thr3.g2)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4.g1, V1.thr4.g2)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1.g1, V2.thr1.g2)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2.g1, V2.thr2.g2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3.g1, V2.thr3.g2)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4.g1, V2.thr4.g2)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1.g1, V3.thr1.g2)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2.g1, V3.thr2.g2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3.g1, V3.thr3.g2)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4.g1, V3.thr4.g2)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1.g1, V4.thr1.g2)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2.g1, V4.thr2.g2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3.g1, V4.thr3.g2)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4.g1, V4.thr4.g2)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1.g1, V5.thr1.g2)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2.g1, V5.thr2.g2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3.g1, V5.thr3.g2)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4.g1, V5.thr4.g2)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1.g1, V6.thr1.g2)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2.g1, V6.thr2.g2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3.g1, V6.thr3.g2)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4.g1, V6.thr4.g2)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1.g1, V7.thr1.g2)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2.g1, V7.thr2.g2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3.g1, V7.thr3.g2)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4.g1, V7.thr4.g2)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1.g1, V8.thr1.g2)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2.g1, V8.thr2.g2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3.g1, V8.thr3.g2)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4.g1, V8.thr4.g2)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1.g1, V9.thr1.g2)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2.g1, V9.thr2.g2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3.g1, V9.thr3.g2)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4.g1, V9.thr4.g2)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1.g1, V10.thr1.g2)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2.g1, V10.thr2.g2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3.g1, V10.thr3.g2)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4.g1, V10.thr4.g2)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1.g1, V11.thr1.g2)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2.g1, V11.thr2.g2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3.g1, V11.thr3.g2)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4.g1, V11.thr4.g2)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1.g1, V12.thr1.g2)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2.g1, V12.thr2.g2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3.g1, V12.thr3.g2)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4.g1, V12.thr4.g2)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, 0)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, 0)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, 0)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, 0)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, 0)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, 0)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, 0)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, 0)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, 0)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, 0)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, 0)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, 0)*1

## SCALING FACTORS:

V1 ~*~ c(1, 1)*V1
V2 ~*~ c(1, 1)*V2
V3 ~*~ c(1, 1)*V3
V4 ~*~ c(1, 1)*V4
V5 ~*~ c(1, 1)*V5
V6 ~*~ c(1, 1)*V6
V7 ~*~ c(1, 1)*V7
V8 ~*~ c(1, 1)*V8
V9 ~*~ c(1, 1)*V9
V10 ~*~ c(1, 1)*V10
V11 ~*~ c(1, 1)*V11
V12 ~*~ c(1, 1)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(0, 0)*ARS + c(psi.2_1.g1, psi.2_1.g2)*ARS
'
        
        ThrModARS <- '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1.g1, lambda.2_1.g2)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1.g1, lambda.3_1.g2)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1.g1, lambda.4_1.g2)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1.g1, lambda.5_1.g2)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1.g1, lambda.6_1.g2)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1.g1, lambda.7_1.g2)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1.g1, lambda.8_1.g2)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1.g1, lambda.9_1.g2)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1.g1, lambda.10_1.g2)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1.g1, lambda.11_1.g2)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1.g1, lambda.12_1.g2)*V12
ARS =~ c(1, 1)*V1 + c(lambda.1_2.g1, lambda.1_2.g2)*V1
ARS =~ c(1, 1)*V2 + c(lambda.2_2.g1, lambda.2_2.g2)*V2
ARS =~ c(1, 1)*V3 + c(lambda.3_2.g1, lambda.3_2.g2)*V3
ARS =~ c(1, 1)*V4 + c(lambda.4_2.g1, lambda.4_2.g2)*V4
ARS =~ c(1, 1)*V5 + c(lambda.5_2.g1, lambda.5_2.g2)*V5
ARS =~ c(1, 1)*V6 + c(lambda.6_2.g1, lambda.6_2.g2)*V6
ARS =~ c(1, 1)*V7 + c(lambda.7_2.g1, lambda.7_2.g2)*V7
ARS =~ c(1, 1)*V8 + c(lambda.8_2.g1, lambda.8_2.g2)*V8
ARS =~ c(1, 1)*V9 + c(lambda.9_2.g1, lambda.9_2.g2)*V9
ARS =~ c(1, 1)*V10 + c(lambda.10_2.g1, lambda.10_2.g2)*V10
ARS =~ c(1, 1)*V11 + c(lambda.11_2.g1, lambda.11_2.g2)*V11
ARS =~ c(1, 1)*V12 + c(lambda.12_2.g1, lambda.12_2.g2)*V12

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V2 ~*~ c(1, NA)*V2
V3 ~*~ c(1, NA)*V3
V4 ~*~ c(1, NA)*V4
V5 ~*~ c(1, NA)*V5
V6 ~*~ c(1, NA)*V6
V7 ~*~ c(1, NA)*V7
V8 ~*~ c(1, NA)*V8
V9 ~*~ c(1, NA)*V9
V10 ~*~ c(1, NA)*V10
V11 ~*~ c(1, NA)*V11
V12 ~*~ c(1, NA)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(0, 0)*ARS + c(psi.2_1.g1, psi.2_1.g2)*ARS
'
        
        LoadModARS <- '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1, lambda.2_1)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1, lambda.3_1)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1, lambda.4_1)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1, lambda.5_1)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1, lambda.6_1)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1, lambda.7_1)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1, lambda.8_1)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1, lambda.9_1)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1, lambda.10_1)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1, lambda.11_1)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1, lambda.12_1)*V12
ARS =~ c(1, 1)*V1 + c(lambda.1_2, lambda.1_2)*V1
ARS =~ c(1, 1)*V2 + c(lambda.2_2, lambda.2_2)*V2
ARS =~ c(1, 1)*V3 + c(lambda.3_2, lambda.3_2)*V3
ARS =~ c(1, 1)*V4 + c(lambda.4_2, lambda.4_2)*V4
ARS =~ c(1, 1)*V5 + c(lambda.5_2, lambda.5_2)*V5
ARS =~ c(1, 1)*V6 + c(lambda.6_2, lambda.6_2)*V6
ARS =~ c(1, 1)*V7 + c(lambda.7_2, lambda.7_2)*V7
ARS =~ c(1, 1)*V8 + c(lambda.8_2, lambda.8_2)*V8
ARS =~ c(1, 1)*V9 + c(lambda.9_2, lambda.9_2)*V9
ARS =~ c(1, 1)*V10 + c(lambda.10_2, lambda.10_2)*V10
ARS =~ c(1, 1)*V11 + c(lambda.11_2, lambda.11_2)*V11
ARS =~ c(1, 1)*V12 + c(lambda.12_2, lambda.12_2)*V12

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V2 ~*~ c(1, NA)*V2
V3 ~*~ c(1, NA)*V3
V4 ~*~ c(1, NA)*V4
V5 ~*~ c(1, NA)*V5
V6 ~*~ c(1, NA)*V6
V7 ~*~ c(1, NA)*V7
V8 ~*~ c(1, NA)*V8
V9 ~*~ c(1, NA)*V9
V10 ~*~ c(1, NA)*V10
V11 ~*~ c(1, NA)*V11
V12 ~*~ c(1, NA)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(0, 0)*ARS + c(psi.2_1.g1, psi.2_1.g2)*ARS
'
        
      } 
      else {
        ConfModnoARS <- '
        ## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1.g1, lambda.2_1.g2)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1.g1, lambda.3_1.g2)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1.g1, lambda.4_1.g2)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1.g1, lambda.5_1.g2)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1.g1, lambda.6_1.g2)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1.g1, lambda.7_1.g2)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1.g1, lambda.8_1.g2)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1.g1, lambda.9_1.g2)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1.g1, lambda.10_1.g2)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1.g1, lambda.11_1.g2)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1.g1, lambda.12_1.g2)*V12
F1 =~ c(NA, NA)*V13 + c(lambda.13_1.g1, lambda.13_1.g2)*V13
F1 =~ c(NA, NA)*V14 + c(lambda.14_1.g1, lambda.14_1.g2)*V14
F1 =~ c(NA, NA)*V15 + c(lambda.15_1.g1, lambda.15_1.g2)*V15
F1 =~ c(NA, NA)*V16 + c(lambda.16_1.g1, lambda.16_1.g2)*V16
F1 =~ c(NA, NA)*V17 + c(lambda.17_1.g1, lambda.17_1.g2)*V17
F1 =~ c(NA, NA)*V18 + c(lambda.18_1.g1, lambda.18_1.g2)*V18
F1 =~ c(NA, NA)*V19 + c(lambda.19_1.g1, lambda.19_1.g2)*V19
F1 =~ c(NA, NA)*V20 + c(lambda.20_1.g1, lambda.20_1.g2)*V20
F1 =~ c(NA, NA)*V21 + c(lambda.21_1.g1, lambda.21_1.g2)*V21
F1 =~ c(NA, NA)*V22 + c(lambda.22_1.g1, lambda.22_1.g2)*V22
F1 =~ c(NA, NA)*V23 + c(lambda.23_1.g1, lambda.23_1.g2)*V23
F1 =~ c(NA, NA)*V24 + c(lambda.24_1.g1, lambda.24_1.g2)*V24

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1.g1, V1.thr1.g2)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2.g1, V1.thr2.g2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3.g1, V1.thr3.g2)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4.g1, V1.thr4.g2)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1.g1, V2.thr1.g2)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2.g1, V2.thr2.g2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3.g1, V2.thr3.g2)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4.g1, V2.thr4.g2)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1.g1, V3.thr1.g2)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2.g1, V3.thr2.g2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3.g1, V3.thr3.g2)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4.g1, V3.thr4.g2)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1.g1, V4.thr1.g2)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2.g1, V4.thr2.g2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3.g1, V4.thr3.g2)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4.g1, V4.thr4.g2)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1.g1, V5.thr1.g2)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2.g1, V5.thr2.g2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3.g1, V5.thr3.g2)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4.g1, V5.thr4.g2)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1.g1, V6.thr1.g2)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2.g1, V6.thr2.g2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3.g1, V6.thr3.g2)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4.g1, V6.thr4.g2)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1.g1, V7.thr1.g2)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2.g1, V7.thr2.g2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3.g1, V7.thr3.g2)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4.g1, V7.thr4.g2)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1.g1, V8.thr1.g2)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2.g1, V8.thr2.g2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3.g1, V8.thr3.g2)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4.g1, V8.thr4.g2)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1.g1, V9.thr1.g2)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2.g1, V9.thr2.g2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3.g1, V9.thr3.g2)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4.g1, V9.thr4.g2)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1.g1, V10.thr1.g2)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2.g1, V10.thr2.g2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3.g1, V10.thr3.g2)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4.g1, V10.thr4.g2)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1.g1, V11.thr1.g2)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2.g1, V11.thr2.g2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3.g1, V11.thr3.g2)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4.g1, V11.thr4.g2)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1.g1, V12.thr1.g2)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2.g1, V12.thr2.g2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3.g1, V12.thr3.g2)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4.g1, V12.thr4.g2)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1.g1, V13.thr1.g2)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2.g1, V13.thr2.g2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3.g1, V13.thr3.g2)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4.g1, V13.thr4.g2)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1.g1, V14.thr1.g2)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2.g1, V14.thr2.g2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3.g1, V14.thr3.g2)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4.g1, V14.thr4.g2)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1.g1, V15.thr1.g2)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2.g1, V15.thr2.g2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3.g1, V15.thr3.g2)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4.g1, V15.thr4.g2)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1.g1, V16.thr1.g2)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2.g1, V16.thr2.g2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3.g1, V16.thr3.g2)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4.g1, V16.thr4.g2)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1.g1, V17.thr1.g2)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2.g1, V17.thr2.g2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3.g1, V17.thr3.g2)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4.g1, V17.thr4.g2)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1.g1, V18.thr1.g2)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2.g1, V18.thr2.g2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3.g1, V18.thr3.g2)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4.g1, V18.thr4.g2)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1.g1, V19.thr1.g2)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2.g1, V19.thr2.g2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3.g1, V19.thr3.g2)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4.g1, V19.thr4.g2)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1.g1, V20.thr1.g2)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2.g1, V20.thr2.g2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3.g1, V20.thr3.g2)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4.g1, V20.thr4.g2)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1.g1, V21.thr1.g2)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2.g1, V21.thr2.g2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3.g1, V21.thr3.g2)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4.g1, V21.thr4.g2)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1.g1, V22.thr1.g2)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2.g1, V22.thr2.g2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3.g1, V22.thr3.g2)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4.g1, V22.thr4.g2)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1.g1, V23.thr1.g2)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2.g1, V23.thr2.g2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3.g1, V23.thr3.g2)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4.g1, V23.thr4.g2)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1.g1, V24.thr1.g2)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2.g1, V24.thr2.g2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3.g1, V24.thr3.g2)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4.g1, V24.thr4.g2)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, 0)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, 0)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, 0)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, 0)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, 0)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, 0)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, 0)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, 0)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, 0)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, 0)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, 0)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, 0)*1
V13 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, 0)*1
V14 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, 0)*1
V15 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, 0)*1
V16 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, 0)*1
V17 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, 0)*1
V18 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, 0)*1
V19 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, 0)*1
V20 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, 0)*1
V21 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, 0)*1
V22 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, 0)*1
V23 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, 0)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, 0)*1

## SCALING FACTORS:

V1 ~*~ c(1, 1)*V1
V2 ~*~ c(1, 1)*V2
V3 ~*~ c(1, 1)*V3
V4 ~*~ c(1, 1)*V4
V5 ~*~ c(1, 1)*V5
V6 ~*~ c(1, 1)*V6
V7 ~*~ c(1, 1)*V7
V8 ~*~ c(1, 1)*V8
V9 ~*~ c(1, 1)*V9
V10 ~*~ c(1, 1)*V10
V11 ~*~ c(1, 1)*V11
V12 ~*~ c(1, 1)*V12
V13 ~*~ c(1, 1)*V13
V14 ~*~ c(1, 1)*V14
V15 ~*~ c(1, 1)*V15
V16 ~*~ c(1, 1)*V16
V17 ~*~ c(1, 1)*V17
V18 ~*~ c(1, 1)*V18
V19 ~*~ c(1, 1)*V19
V20 ~*~ c(1, 1)*V20
V21 ~*~ c(1, 1)*V21
V22 ~*~ c(1, 1)*V22
V23 ~*~ c(1, 1)*V23
V24 ~*~ c(1, 1)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:
'
        
        ThrModnoARS <- '
          ## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1.g1, lambda.2_1.g2)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1.g1, lambda.3_1.g2)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1.g1, lambda.4_1.g2)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1.g1, lambda.5_1.g2)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1.g1, lambda.6_1.g2)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1.g1, lambda.7_1.g2)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1.g1, lambda.8_1.g2)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1.g1, lambda.9_1.g2)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1.g1, lambda.10_1.g2)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1.g1, lambda.11_1.g2)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1.g1, lambda.12_1.g2)*V12
F1 =~ c(NA, NA)*V13 + c(lambda.13_1.g1, lambda.13_1.g2)*V13
F1 =~ c(NA, NA)*V14 + c(lambda.14_1.g1, lambda.14_1.g2)*V14
F1 =~ c(NA, NA)*V15 + c(lambda.15_1.g1, lambda.15_1.g2)*V15
F1 =~ c(NA, NA)*V16 + c(lambda.16_1.g1, lambda.16_1.g2)*V16
F1 =~ c(NA, NA)*V17 + c(lambda.17_1.g1, lambda.17_1.g2)*V17
F1 =~ c(NA, NA)*V18 + c(lambda.18_1.g1, lambda.18_1.g2)*V18
F1 =~ c(NA, NA)*V19 + c(lambda.19_1.g1, lambda.19_1.g2)*V19
F1 =~ c(NA, NA)*V20 + c(lambda.20_1.g1, lambda.20_1.g2)*V20
F1 =~ c(NA, NA)*V21 + c(lambda.21_1.g1, lambda.21_1.g2)*V21
F1 =~ c(NA, NA)*V22 + c(lambda.22_1.g1, lambda.22_1.g2)*V22
F1 =~ c(NA, NA)*V23 + c(lambda.23_1.g1, lambda.23_1.g2)*V23
F1 =~ c(NA, NA)*V24 + c(lambda.24_1.g1, lambda.24_1.g2)*V24

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1, V13.thr1)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2, V13.thr2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3, V13.thr3)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4, V13.thr4)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1, V14.thr1)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2, V14.thr2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3, V14.thr3)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4, V14.thr4)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1, V15.thr1)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2, V15.thr2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3, V15.thr3)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4, V15.thr4)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1, V16.thr1)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2, V16.thr2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3, V16.thr3)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4, V16.thr4)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1, V17.thr1)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2, V17.thr2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3, V17.thr3)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4, V17.thr4)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1, V18.thr1)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2, V18.thr2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3, V18.thr3)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4, V18.thr4)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1, V19.thr1)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2, V19.thr2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3, V19.thr3)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4, V19.thr4)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1, V20.thr1)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2, V20.thr2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3, V20.thr3)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4, V20.thr4)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1, V21.thr1)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2, V21.thr2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3, V21.thr3)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4, V21.thr4)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1, V22.thr1)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2, V22.thr2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3, V22.thr3)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4, V22.thr4)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1, V23.thr1)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2, V23.thr2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3, V23.thr3)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4, V23.thr4)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1, V24.thr1)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2, V24.thr2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3, V24.thr3)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4, V24.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1
V13 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, NA)*1
V14 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, NA)*1
V15 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, NA)*1
V16 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, NA)*1
V17 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, NA)*1
V18 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, NA)*1
V19 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, NA)*1
V20 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, NA)*1
V21 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, NA)*1
V22 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, NA)*1
V23 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, NA)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V2 ~*~ c(1, NA)*V2
V3 ~*~ c(1, NA)*V3
V4 ~*~ c(1, NA)*V4
V5 ~*~ c(1, NA)*V5
V6 ~*~ c(1, NA)*V6
V7 ~*~ c(1, NA)*V7
V8 ~*~ c(1, NA)*V8
V9 ~*~ c(1, NA)*V9
V10 ~*~ c(1, NA)*V10
V11 ~*~ c(1, NA)*V11
V12 ~*~ c(1, NA)*V12
V13 ~*~ c(1, NA)*V13
V14 ~*~ c(1, NA)*V14
V15 ~*~ c(1, NA)*V15
V16 ~*~ c(1, NA)*V16
V17 ~*~ c(1, NA)*V17
V18 ~*~ c(1, NA)*V18
V19 ~*~ c(1, NA)*V19
V20 ~*~ c(1, NA)*V20
V21 ~*~ c(1, NA)*V21
V22 ~*~ c(1, NA)*V22
V23 ~*~ c(1, NA)*V23
V24 ~*~ c(1, NA)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

          '
        
        LoadModnoARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1, lambda.2_1)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1, lambda.3_1)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1, lambda.4_1)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1, lambda.5_1)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1, lambda.6_1)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1, lambda.7_1)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1, lambda.8_1)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1, lambda.9_1)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1, lambda.10_1)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1, lambda.11_1)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1, lambda.12_1)*V12
F1 =~ c(NA, NA)*V13 + c(lambda.13_1, lambda.13_1)*V13
F1 =~ c(NA, NA)*V14 + c(lambda.14_1, lambda.14_1)*V14
F1 =~ c(NA, NA)*V15 + c(lambda.15_1, lambda.15_1)*V15
F1 =~ c(NA, NA)*V16 + c(lambda.16_1, lambda.16_1)*V16
F1 =~ c(NA, NA)*V17 + c(lambda.17_1, lambda.17_1)*V17
F1 =~ c(NA, NA)*V18 + c(lambda.18_1, lambda.18_1)*V18
F1 =~ c(NA, NA)*V19 + c(lambda.19_1, lambda.19_1)*V19
F1 =~ c(NA, NA)*V20 + c(lambda.20_1, lambda.20_1)*V20
F1 =~ c(NA, NA)*V21 + c(lambda.21_1, lambda.21_1)*V21
F1 =~ c(NA, NA)*V22 + c(lambda.22_1, lambda.22_1)*V22
F1 =~ c(NA, NA)*V23 + c(lambda.23_1, lambda.23_1)*V23
F1 =~ c(NA, NA)*V24 + c(lambda.24_1, lambda.24_1)*V24

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1, V13.thr1)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2, V13.thr2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3, V13.thr3)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4, V13.thr4)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1, V14.thr1)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2, V14.thr2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3, V14.thr3)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4, V14.thr4)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1, V15.thr1)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2, V15.thr2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3, V15.thr3)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4, V15.thr4)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1, V16.thr1)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2, V16.thr2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3, V16.thr3)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4, V16.thr4)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1, V17.thr1)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2, V17.thr2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3, V17.thr3)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4, V17.thr4)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1, V18.thr1)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2, V18.thr2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3, V18.thr3)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4, V18.thr4)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1, V19.thr1)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2, V19.thr2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3, V19.thr3)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4, V19.thr4)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1, V20.thr1)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2, V20.thr2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3, V20.thr3)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4, V20.thr4)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1, V21.thr1)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2, V21.thr2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3, V21.thr3)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4, V21.thr4)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1, V22.thr1)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2, V22.thr2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3, V22.thr3)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4, V22.thr4)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1, V23.thr1)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2, V23.thr2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3, V23.thr3)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4, V23.thr4)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1, V24.thr1)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2, V24.thr2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3, V24.thr3)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4, V24.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1
V13 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, NA)*1
V14 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, NA)*1
V15 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, NA)*1
V16 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, NA)*1
V17 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, NA)*1
V18 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, NA)*1
V19 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, NA)*1
V20 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, NA)*1
V21 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, NA)*1
V22 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, NA)*1
V23 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, NA)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V2 ~*~ c(1, NA)*V2
V3 ~*~ c(1, NA)*V3
V4 ~*~ c(1, NA)*V4
V5 ~*~ c(1, NA)*V5
V6 ~*~ c(1, NA)*V6
V7 ~*~ c(1, NA)*V7
V8 ~*~ c(1, NA)*V8
V9 ~*~ c(1, NA)*V9
V10 ~*~ c(1, NA)*V10
V11 ~*~ c(1, NA)*V11
V12 ~*~ c(1, NA)*V12
V13 ~*~ c(1, NA)*V13
V14 ~*~ c(1, NA)*V14
V15 ~*~ c(1, NA)*V15
V16 ~*~ c(1, NA)*V16
V17 ~*~ c(1, NA)*V17
V18 ~*~ c(1, NA)*V18
V19 ~*~ c(1, NA)*V19
V20 ~*~ c(1, NA)*V20
V21 ~*~ c(1, NA)*V21
V22 ~*~ c(1, NA)*V22
V23 ~*~ c(1, NA)*V23
V24 ~*~ c(1, NA)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:
          '
        
        ConfModARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1.g1, lambda.2_1.g2)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1.g1, lambda.3_1.g2)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1.g1, lambda.4_1.g2)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1.g1, lambda.5_1.g2)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1.g1, lambda.6_1.g2)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1.g1, lambda.7_1.g2)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1.g1, lambda.8_1.g2)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1.g1, lambda.9_1.g2)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1.g1, lambda.10_1.g2)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1.g1, lambda.11_1.g2)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1.g1, lambda.12_1.g2)*V12
F1 =~ c(NA, NA)*V13 + c(lambda.13_1.g1, lambda.13_1.g2)*V13
F1 =~ c(NA, NA)*V14 + c(lambda.14_1.g1, lambda.14_1.g2)*V14
F1 =~ c(NA, NA)*V15 + c(lambda.15_1.g1, lambda.15_1.g2)*V15
F1 =~ c(NA, NA)*V16 + c(lambda.16_1.g1, lambda.16_1.g2)*V16
F1 =~ c(NA, NA)*V17 + c(lambda.17_1.g1, lambda.17_1.g2)*V17
F1 =~ c(NA, NA)*V18 + c(lambda.18_1.g1, lambda.18_1.g2)*V18
F1 =~ c(NA, NA)*V19 + c(lambda.19_1.g1, lambda.19_1.g2)*V19
F1 =~ c(NA, NA)*V20 + c(lambda.20_1.g1, lambda.20_1.g2)*V20
F1 =~ c(NA, NA)*V21 + c(lambda.21_1.g1, lambda.21_1.g2)*V21
F1 =~ c(NA, NA)*V22 + c(lambda.22_1.g1, lambda.22_1.g2)*V22
F1 =~ c(NA, NA)*V23 + c(lambda.23_1.g1, lambda.23_1.g2)*V23
F1 =~ c(NA, NA)*V24 + c(lambda.24_1.g1, lambda.24_1.g2)*V24
ARS =~ c(1, 1)*V1 + c(lambda.1_2.g1, lambda.1_2.g2)*V1
ARS =~ c(1, 1)*V2 + c(lambda.2_2.g1, lambda.2_2.g2)*V2
ARS =~ c(1, 1)*V3 + c(lambda.3_2.g1, lambda.3_2.g2)*V3
ARS =~ c(1, 1)*V4 + c(lambda.4_2.g1, lambda.4_2.g2)*V4
ARS =~ c(1, 1)*V5 + c(lambda.5_2.g1, lambda.5_2.g2)*V5
ARS =~ c(1, 1)*V6 + c(lambda.6_2.g1, lambda.6_2.g2)*V6
ARS =~ c(1, 1)*V7 + c(lambda.7_2.g1, lambda.7_2.g2)*V7
ARS =~ c(1, 1)*V8 + c(lambda.8_2.g1, lambda.8_2.g2)*V8
ARS =~ c(1, 1)*V9 + c(lambda.9_2.g1, lambda.9_2.g2)*V9
ARS =~ c(1, 1)*V10 + c(lambda.10_2.g1, lambda.10_2.g2)*V10
ARS =~ c(1, 1)*V11 + c(lambda.11_2.g1, lambda.11_2.g2)*V11
ARS =~ c(1, 1)*V12 + c(lambda.12_2.g1, lambda.12_2.g2)*V12
ARS =~ c(1, 1)*V13 + c(lambda.13_2.g1, lambda.13_2.g2)*V13
ARS =~ c(1, 1)*V14 + c(lambda.14_2.g1, lambda.14_2.g2)*V14
ARS =~ c(1, 1)*V15 + c(lambda.15_2.g1, lambda.15_2.g2)*V15
ARS =~ c(1, 1)*V16 + c(lambda.16_2.g1, lambda.16_2.g2)*V16
ARS =~ c(1, 1)*V17 + c(lambda.17_2.g1, lambda.17_2.g2)*V17
ARS =~ c(1, 1)*V18 + c(lambda.18_2.g1, lambda.18_2.g2)*V18
ARS =~ c(1, 1)*V19 + c(lambda.19_2.g1, lambda.19_2.g2)*V19
ARS =~ c(1, 1)*V20 + c(lambda.20_2.g1, lambda.20_2.g2)*V20
ARS =~ c(1, 1)*V21 + c(lambda.21_2.g1, lambda.21_2.g2)*V21
ARS =~ c(1, 1)*V22 + c(lambda.22_2.g1, lambda.22_2.g2)*V22
ARS =~ c(1, 1)*V23 + c(lambda.23_2.g1, lambda.23_2.g2)*V23
ARS =~ c(1, 1)*V24 + c(lambda.24_2.g1, lambda.24_2.g2)*V24

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1.g1, V1.thr1.g2)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2.g1, V1.thr2.g2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3.g1, V1.thr3.g2)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4.g1, V1.thr4.g2)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1.g1, V2.thr1.g2)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2.g1, V2.thr2.g2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3.g1, V2.thr3.g2)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4.g1, V2.thr4.g2)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1.g1, V3.thr1.g2)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2.g1, V3.thr2.g2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3.g1, V3.thr3.g2)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4.g1, V3.thr4.g2)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1.g1, V4.thr1.g2)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2.g1, V4.thr2.g2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3.g1, V4.thr3.g2)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4.g1, V4.thr4.g2)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1.g1, V5.thr1.g2)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2.g1, V5.thr2.g2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3.g1, V5.thr3.g2)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4.g1, V5.thr4.g2)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1.g1, V6.thr1.g2)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2.g1, V6.thr2.g2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3.g1, V6.thr3.g2)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4.g1, V6.thr4.g2)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1.g1, V7.thr1.g2)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2.g1, V7.thr2.g2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3.g1, V7.thr3.g2)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4.g1, V7.thr4.g2)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1.g1, V8.thr1.g2)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2.g1, V8.thr2.g2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3.g1, V8.thr3.g2)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4.g1, V8.thr4.g2)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1.g1, V9.thr1.g2)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2.g1, V9.thr2.g2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3.g1, V9.thr3.g2)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4.g1, V9.thr4.g2)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1.g1, V10.thr1.g2)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2.g1, V10.thr2.g2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3.g1, V10.thr3.g2)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4.g1, V10.thr4.g2)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1.g1, V11.thr1.g2)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2.g1, V11.thr2.g2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3.g1, V11.thr3.g2)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4.g1, V11.thr4.g2)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1.g1, V12.thr1.g2)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2.g1, V12.thr2.g2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3.g1, V12.thr3.g2)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4.g1, V12.thr4.g2)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1.g1, V13.thr1.g2)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2.g1, V13.thr2.g2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3.g1, V13.thr3.g2)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4.g1, V13.thr4.g2)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1.g1, V14.thr1.g2)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2.g1, V14.thr2.g2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3.g1, V14.thr3.g2)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4.g1, V14.thr4.g2)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1.g1, V15.thr1.g2)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2.g1, V15.thr2.g2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3.g1, V15.thr3.g2)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4.g1, V15.thr4.g2)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1.g1, V16.thr1.g2)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2.g1, V16.thr2.g2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3.g1, V16.thr3.g2)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4.g1, V16.thr4.g2)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1.g1, V17.thr1.g2)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2.g1, V17.thr2.g2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3.g1, V17.thr3.g2)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4.g1, V17.thr4.g2)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1.g1, V18.thr1.g2)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2.g1, V18.thr2.g2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3.g1, V18.thr3.g2)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4.g1, V18.thr4.g2)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1.g1, V19.thr1.g2)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2.g1, V19.thr2.g2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3.g1, V19.thr3.g2)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4.g1, V19.thr4.g2)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1.g1, V20.thr1.g2)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2.g1, V20.thr2.g2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3.g1, V20.thr3.g2)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4.g1, V20.thr4.g2)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1.g1, V21.thr1.g2)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2.g1, V21.thr2.g2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3.g1, V21.thr3.g2)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4.g1, V21.thr4.g2)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1.g1, V22.thr1.g2)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2.g1, V22.thr2.g2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3.g1, V22.thr3.g2)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4.g1, V22.thr4.g2)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1.g1, V23.thr1.g2)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2.g1, V23.thr2.g2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3.g1, V23.thr3.g2)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4.g1, V23.thr4.g2)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1.g1, V24.thr1.g2)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2.g1, V24.thr2.g2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3.g1, V24.thr3.g2)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4.g1, V24.thr4.g2)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, 0)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, 0)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, 0)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, 0)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, 0)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, 0)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, 0)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, 0)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, 0)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, 0)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, 0)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, 0)*1
V13 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, 0)*1
V14 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, 0)*1
V15 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, 0)*1
V16 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, 0)*1
V17 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, 0)*1
V18 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, 0)*1
V19 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, 0)*1
V20 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, 0)*1
V21 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, 0)*1
V22 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, 0)*1
V23 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, 0)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, 0)*1

## SCALING FACTORS:

V1 ~*~ c(1, 1)*V1
V2 ~*~ c(1, 1)*V2
V3 ~*~ c(1, 1)*V3
V4 ~*~ c(1, 1)*V4
V5 ~*~ c(1, 1)*V5
V6 ~*~ c(1, 1)*V6
V7 ~*~ c(1, 1)*V7
V8 ~*~ c(1, 1)*V8
V9 ~*~ c(1, 1)*V9
V10 ~*~ c(1, 1)*V10
V11 ~*~ c(1, 1)*V11
V12 ~*~ c(1, 1)*V12
V13 ~*~ c(1, 1)*V13
V14 ~*~ c(1, 1)*V14
V15 ~*~ c(1, 1)*V15
V16 ~*~ c(1, 1)*V16
V17 ~*~ c(1, 1)*V17
V18 ~*~ c(1, 1)*V18
V19 ~*~ c(1, 1)*V19
V20 ~*~ c(1, 1)*V20
V21 ~*~ c(1, 1)*V21
V22 ~*~ c(1, 1)*V22
V23 ~*~ c(1, 1)*V23
V24 ~*~ c(1, 1)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(0, 0)*ARS + c(psi.2_1.g1, psi.2_1.g2)*ARS
          '
        
        ThrModARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1.g1, lambda.2_1.g2)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1.g1, lambda.3_1.g2)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1.g1, lambda.4_1.g2)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1.g1, lambda.5_1.g2)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1.g1, lambda.6_1.g2)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1.g1, lambda.7_1.g2)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1.g1, lambda.8_1.g2)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1.g1, lambda.9_1.g2)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1.g1, lambda.10_1.g2)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1.g1, lambda.11_1.g2)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1.g1, lambda.12_1.g2)*V12
F1 =~ c(NA, NA)*V13 + c(lambda.13_1.g1, lambda.13_1.g2)*V13
F1 =~ c(NA, NA)*V14 + c(lambda.14_1.g1, lambda.14_1.g2)*V14
F1 =~ c(NA, NA)*V15 + c(lambda.15_1.g1, lambda.15_1.g2)*V15
F1 =~ c(NA, NA)*V16 + c(lambda.16_1.g1, lambda.16_1.g2)*V16
F1 =~ c(NA, NA)*V17 + c(lambda.17_1.g1, lambda.17_1.g2)*V17
F1 =~ c(NA, NA)*V18 + c(lambda.18_1.g1, lambda.18_1.g2)*V18
F1 =~ c(NA, NA)*V19 + c(lambda.19_1.g1, lambda.19_1.g2)*V19
F1 =~ c(NA, NA)*V20 + c(lambda.20_1.g1, lambda.20_1.g2)*V20
F1 =~ c(NA, NA)*V21 + c(lambda.21_1.g1, lambda.21_1.g2)*V21
F1 =~ c(NA, NA)*V22 + c(lambda.22_1.g1, lambda.22_1.g2)*V22
F1 =~ c(NA, NA)*V23 + c(lambda.23_1.g1, lambda.23_1.g2)*V23
F1 =~ c(NA, NA)*V24 + c(lambda.24_1.g1, lambda.24_1.g2)*V24
ARS =~ c(1, 1)*V1 + c(lambda.1_2.g1, lambda.1_2.g2)*V1
ARS =~ c(1, 1)*V2 + c(lambda.2_2.g1, lambda.2_2.g2)*V2
ARS =~ c(1, 1)*V3 + c(lambda.3_2.g1, lambda.3_2.g2)*V3
ARS =~ c(1, 1)*V4 + c(lambda.4_2.g1, lambda.4_2.g2)*V4
ARS =~ c(1, 1)*V5 + c(lambda.5_2.g1, lambda.5_2.g2)*V5
ARS =~ c(1, 1)*V6 + c(lambda.6_2.g1, lambda.6_2.g2)*V6
ARS =~ c(1, 1)*V7 + c(lambda.7_2.g1, lambda.7_2.g2)*V7
ARS =~ c(1, 1)*V8 + c(lambda.8_2.g1, lambda.8_2.g2)*V8
ARS =~ c(1, 1)*V9 + c(lambda.9_2.g1, lambda.9_2.g2)*V9
ARS =~ c(1, 1)*V10 + c(lambda.10_2.g1, lambda.10_2.g2)*V10
ARS =~ c(1, 1)*V11 + c(lambda.11_2.g1, lambda.11_2.g2)*V11
ARS =~ c(1, 1)*V12 + c(lambda.12_2.g1, lambda.12_2.g2)*V12
ARS =~ c(1, 1)*V13 + c(lambda.13_2.g1, lambda.13_2.g2)*V13
ARS =~ c(1, 1)*V14 + c(lambda.14_2.g1, lambda.14_2.g2)*V14
ARS =~ c(1, 1)*V15 + c(lambda.15_2.g1, lambda.15_2.g2)*V15
ARS =~ c(1, 1)*V16 + c(lambda.16_2.g1, lambda.16_2.g2)*V16
ARS =~ c(1, 1)*V17 + c(lambda.17_2.g1, lambda.17_2.g2)*V17
ARS =~ c(1, 1)*V18 + c(lambda.18_2.g1, lambda.18_2.g2)*V18
ARS =~ c(1, 1)*V19 + c(lambda.19_2.g1, lambda.19_2.g2)*V19
ARS =~ c(1, 1)*V20 + c(lambda.20_2.g1, lambda.20_2.g2)*V20
ARS =~ c(1, 1)*V21 + c(lambda.21_2.g1, lambda.21_2.g2)*V21
ARS =~ c(1, 1)*V22 + c(lambda.22_2.g1, lambda.22_2.g2)*V22
ARS =~ c(1, 1)*V23 + c(lambda.23_2.g1, lambda.23_2.g2)*V23
ARS =~ c(1, 1)*V24 + c(lambda.24_2.g1, lambda.24_2.g2)*V24

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1, V13.thr1)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2, V13.thr2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3, V13.thr3)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4, V13.thr4)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1, V14.thr1)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2, V14.thr2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3, V14.thr3)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4, V14.thr4)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1, V15.thr1)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2, V15.thr2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3, V15.thr3)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4, V15.thr4)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1, V16.thr1)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2, V16.thr2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3, V16.thr3)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4, V16.thr4)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1, V17.thr1)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2, V17.thr2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3, V17.thr3)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4, V17.thr4)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1, V18.thr1)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2, V18.thr2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3, V18.thr3)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4, V18.thr4)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1, V19.thr1)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2, V19.thr2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3, V19.thr3)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4, V19.thr4)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1, V20.thr1)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2, V20.thr2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3, V20.thr3)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4, V20.thr4)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1, V21.thr1)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2, V21.thr2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3, V21.thr3)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4, V21.thr4)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1, V22.thr1)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2, V22.thr2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3, V22.thr3)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4, V22.thr4)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1, V23.thr1)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2, V23.thr2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3, V23.thr3)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4, V23.thr4)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1, V24.thr1)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2, V24.thr2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3, V24.thr3)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4, V24.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1
V13 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, NA)*1
V14 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, NA)*1
V15 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, NA)*1
V16 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, NA)*1
V17 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, NA)*1
V18 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, NA)*1
V19 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, NA)*1
V20 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, NA)*1
V21 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, NA)*1
V22 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, NA)*1
V23 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, NA)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V2 ~*~ c(1, NA)*V2
V3 ~*~ c(1, NA)*V3
V4 ~*~ c(1, NA)*V4
V5 ~*~ c(1, NA)*V5
V6 ~*~ c(1, NA)*V6
V7 ~*~ c(1, NA)*V7
V8 ~*~ c(1, NA)*V8
V9 ~*~ c(1, NA)*V9
V10 ~*~ c(1, NA)*V10
V11 ~*~ c(1, NA)*V11
V12 ~*~ c(1, NA)*V12
V13 ~*~ c(1, NA)*V13
V14 ~*~ c(1, NA)*V14
V15 ~*~ c(1, NA)*V15
V16 ~*~ c(1, NA)*V16
V17 ~*~ c(1, NA)*V17
V18 ~*~ c(1, NA)*V18
V19 ~*~ c(1, NA)*V19
V20 ~*~ c(1, NA)*V20
V21 ~*~ c(1, NA)*V21
V22 ~*~ c(1, NA)*V22
V23 ~*~ c(1, NA)*V23
V24 ~*~ c(1, NA)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(0, 0)*ARS + c(psi.2_1.g1, psi.2_1.g2)*ARS
'
        
        LoadModARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1, lambda.2_1)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1, lambda.3_1)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1, lambda.4_1)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1, lambda.5_1)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1, lambda.6_1)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1, lambda.7_1)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1, lambda.8_1)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1, lambda.9_1)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1, lambda.10_1)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1, lambda.11_1)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1, lambda.12_1)*V12
F1 =~ c(NA, NA)*V13 + c(lambda.13_1, lambda.13_1)*V13
F1 =~ c(NA, NA)*V14 + c(lambda.14_1, lambda.14_1)*V14
F1 =~ c(NA, NA)*V15 + c(lambda.15_1, lambda.15_1)*V15
F1 =~ c(NA, NA)*V16 + c(lambda.16_1, lambda.16_1)*V16
F1 =~ c(NA, NA)*V17 + c(lambda.17_1, lambda.17_1)*V17
F1 =~ c(NA, NA)*V18 + c(lambda.18_1, lambda.18_1)*V18
F1 =~ c(NA, NA)*V19 + c(lambda.19_1, lambda.19_1)*V19
F1 =~ c(NA, NA)*V20 + c(lambda.20_1, lambda.20_1)*V20
F1 =~ c(NA, NA)*V21 + c(lambda.21_1, lambda.21_1)*V21
F1 =~ c(NA, NA)*V22 + c(lambda.22_1, lambda.22_1)*V22
F1 =~ c(NA, NA)*V23 + c(lambda.23_1, lambda.23_1)*V23
F1 =~ c(NA, NA)*V24 + c(lambda.24_1, lambda.24_1)*V24
ARS =~ c(1, 1)*V1 + c(lambda.1_2, lambda.1_2)*V1
ARS =~ c(1, 1)*V2 + c(lambda.2_2, lambda.2_2)*V2
ARS =~ c(1, 1)*V3 + c(lambda.3_2, lambda.3_2)*V3
ARS =~ c(1, 1)*V4 + c(lambda.4_2, lambda.4_2)*V4
ARS =~ c(1, 1)*V5 + c(lambda.5_2, lambda.5_2)*V5
ARS =~ c(1, 1)*V6 + c(lambda.6_2, lambda.6_2)*V6
ARS =~ c(1, 1)*V7 + c(lambda.7_2, lambda.7_2)*V7
ARS =~ c(1, 1)*V8 + c(lambda.8_2, lambda.8_2)*V8
ARS =~ c(1, 1)*V9 + c(lambda.9_2, lambda.9_2)*V9
ARS =~ c(1, 1)*V10 + c(lambda.10_2, lambda.10_2)*V10
ARS =~ c(1, 1)*V11 + c(lambda.11_2, lambda.11_2)*V11
ARS =~ c(1, 1)*V12 + c(lambda.12_2, lambda.12_2)*V12
ARS =~ c(1, 1)*V13 + c(lambda.13_2, lambda.13_2)*V13
ARS =~ c(1, 1)*V14 + c(lambda.14_2, lambda.14_2)*V14
ARS =~ c(1, 1)*V15 + c(lambda.15_2, lambda.15_2)*V15
ARS =~ c(1, 1)*V16 + c(lambda.16_2, lambda.16_2)*V16
ARS =~ c(1, 1)*V17 + c(lambda.17_2, lambda.17_2)*V17
ARS =~ c(1, 1)*V18 + c(lambda.18_2, lambda.18_2)*V18
ARS =~ c(1, 1)*V19 + c(lambda.19_2, lambda.19_2)*V19
ARS =~ c(1, 1)*V20 + c(lambda.20_2, lambda.20_2)*V20
ARS =~ c(1, 1)*V21 + c(lambda.21_2, lambda.21_2)*V21
ARS =~ c(1, 1)*V22 + c(lambda.22_2, lambda.22_2)*V22
ARS =~ c(1, 1)*V23 + c(lambda.23_2, lambda.23_2)*V23
ARS =~ c(1, 1)*V24 + c(lambda.24_2, lambda.24_2)*V24

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1, V13.thr1)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2, V13.thr2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3, V13.thr3)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4, V13.thr4)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1, V14.thr1)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2, V14.thr2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3, V14.thr3)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4, V14.thr4)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1, V15.thr1)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2, V15.thr2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3, V15.thr3)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4, V15.thr4)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1, V16.thr1)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2, V16.thr2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3, V16.thr3)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4, V16.thr4)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1, V17.thr1)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2, V17.thr2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3, V17.thr3)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4, V17.thr4)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1, V18.thr1)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2, V18.thr2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3, V18.thr3)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4, V18.thr4)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1, V19.thr1)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2, V19.thr2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3, V19.thr3)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4, V19.thr4)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1, V20.thr1)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2, V20.thr2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3, V20.thr3)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4, V20.thr4)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1, V21.thr1)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2, V21.thr2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3, V21.thr3)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4, V21.thr4)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1, V22.thr1)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2, V22.thr2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3, V22.thr3)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4, V22.thr4)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1, V23.thr1)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2, V23.thr2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3, V23.thr3)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4, V23.thr4)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1, V24.thr1)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2, V24.thr2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3, V24.thr3)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4, V24.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1
V13 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, NA)*1
V14 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, NA)*1
V15 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, NA)*1
V16 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, NA)*1
V17 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, NA)*1
V18 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, NA)*1
V19 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, NA)*1
V20 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, NA)*1
V21 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, NA)*1
V22 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, NA)*1
V23 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, NA)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V2 ~*~ c(1, NA)*V2
V3 ~*~ c(1, NA)*V3
V4 ~*~ c(1, NA)*V4
V5 ~*~ c(1, NA)*V5
V6 ~*~ c(1, NA)*V6
V7 ~*~ c(1, NA)*V7
V8 ~*~ c(1, NA)*V8
V9 ~*~ c(1, NA)*V9
V10 ~*~ c(1, NA)*V10
V11 ~*~ c(1, NA)*V11
V12 ~*~ c(1, NA)*V12
V13 ~*~ c(1, NA)*V13
V14 ~*~ c(1, NA)*V14
V15 ~*~ c(1, NA)*V15
V16 ~*~ c(1, NA)*V16
V17 ~*~ c(1, NA)*V17
V18 ~*~ c(1, NA)*V18
V19 ~*~ c(1, NA)*V19
V20 ~*~ c(1, NA)*V20
V21 ~*~ c(1, NA)*V21
V22 ~*~ c(1, NA)*V22
V23 ~*~ c(1, NA)*V23
V24 ~*~ c(1, NA)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(0, 0)*ARS + c(psi.2_1.g1, psi.2_1.g2)*ARS
'
        
      }
      
    } else {
      if(nitems== 12){
        ConfModnoARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1.g1, lambda.2_1.g2)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1.g1, lambda.3_1.g2)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1.g1, lambda.4_1.g2)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1.g1, lambda.5_1.g2)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1.g1, lambda.6_1.g2)*V11
F2 =~ c(1, 1)*V2 + c(lambda.7_2.g1, lambda.7_2.g2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.8_2.g1, lambda.8_2.g2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.9_2.g1, lambda.9_2.g2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.10_2.g1, lambda.10_2.g2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.11_2.g1, lambda.11_2.g2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.12_2.g1, lambda.12_2.g2)*V12

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1.g1, V1.thr1.g2)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2.g1, V1.thr2.g2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3.g1, V1.thr3.g2)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4.g1, V1.thr4.g2)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1.g1, V3.thr1.g2)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2.g1, V3.thr2.g2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3.g1, V3.thr3.g2)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4.g1, V3.thr4.g2)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1.g1, V5.thr1.g2)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2.g1, V5.thr2.g2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3.g1, V5.thr3.g2)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4.g1, V5.thr4.g2)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1.g1, V7.thr1.g2)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2.g1, V7.thr2.g2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3.g1, V7.thr3.g2)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4.g1, V7.thr4.g2)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1.g1, V9.thr1.g2)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2.g1, V9.thr2.g2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3.g1, V9.thr3.g2)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4.g1, V9.thr4.g2)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1.g1, V11.thr1.g2)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2.g1, V11.thr2.g2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3.g1, V11.thr3.g2)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4.g1, V11.thr4.g2)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1.g1, V2.thr1.g2)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2.g1, V2.thr2.g2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3.g1, V2.thr3.g2)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4.g1, V2.thr4.g2)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1.g1, V4.thr1.g2)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2.g1, V4.thr2.g2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3.g1, V4.thr3.g2)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4.g1, V4.thr4.g2)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1.g1, V6.thr1.g2)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2.g1, V6.thr2.g2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3.g1, V6.thr3.g2)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4.g1, V6.thr4.g2)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1.g1, V8.thr1.g2)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2.g1, V8.thr2.g2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3.g1, V8.thr3.g2)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4.g1, V8.thr4.g2)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1.g1, V10.thr1.g2)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2.g1, V10.thr2.g2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3.g1, V10.thr3.g2)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4.g1, V10.thr4.g2)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1.g1, V12.thr1.g2)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2.g1, V12.thr2.g2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3.g1, V12.thr3.g2)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4.g1, V12.thr4.g2)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, 0)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, 0)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, 0)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, 0)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, 0)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, 0)*1
V2 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, 0)*1
V4 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, 0)*1
V6 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, 0)*1
V8 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, 0)*1
V10 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, 0)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, 0)*1

## SCALING FACTORS:

V1 ~*~ c(1, 1)*V1
V3 ~*~ c(1, 1)*V3
V5 ~*~ c(1, 1)*V5
V7 ~*~ c(1, 1)*V7
V9 ~*~ c(1, 1)*V9
V11 ~*~ c(1, 1)*V11
V2 ~*~ c(1, 1)*V2
V4 ~*~ c(1, 1)*V4
V6 ~*~ c(1, 1)*V6
V8 ~*~ c(1, 1)*V8
V10 ~*~ c(1, 1)*V10
V12 ~*~ c(1, 1)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:
F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
'
        
        ThrModnoARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1.g1, lambda.2_1.g2)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1.g1, lambda.3_1.g2)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1.g1, lambda.4_1.g2)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1.g1, lambda.5_1.g2)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1.g1, lambda.6_1.g2)*V11
F2 =~ c(1, 1)*V2 + c(lambda.7_2.g1, lambda.7_2.g2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.8_2.g1, lambda.8_2.g2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.9_2.g1, lambda.9_2.g2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.10_2.g1, lambda.10_2.g2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.11_2.g1, lambda.11_2.g2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.12_2.g1, lambda.12_2.g2)*V12

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V2 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V4 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V6 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V8 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V10 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V3 ~*~ c(1, NA)*V3
V5 ~*~ c(1, NA)*V5
V7 ~*~ c(1, NA)*V7
V9 ~*~ c(1, NA)*V9
V11 ~*~ c(1, NA)*V11
V2 ~*~ c(1, NA)*V2
V4 ~*~ c(1, NA)*V4
V6 ~*~ c(1, NA)*V6
V8 ~*~ c(1, NA)*V8
V10 ~*~ c(1, NA)*V10
V12 ~*~ c(1, NA)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:


## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
'
        
        LoadModnoARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1, lambda.2_1)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1, lambda.3_1)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1, lambda.4_1)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1, lambda.5_1)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1, lambda.6_1)*V11
F2 =~ c(1, 1)*V2 + c(lambda.7_2, lambda.7_2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.8_2, lambda.8_2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.9_2, lambda.9_2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.10_2, lambda.10_2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.11_2, lambda.11_2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.12_2, lambda.12_2)*V12

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V2 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V4 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V6 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V8 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V10 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V3 ~*~ c(1, NA)*V3
V5 ~*~ c(1, NA)*V5
V7 ~*~ c(1, NA)*V7
V9 ~*~ c(1, NA)*V9
V11 ~*~ c(1, NA)*V11
V2 ~*~ c(1, NA)*V2
V4 ~*~ c(1, NA)*V4
V6 ~*~ c(1, NA)*V6
V8 ~*~ c(1, NA)*V8
V10 ~*~ c(1, NA)*V10
V12 ~*~ c(1, NA)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:


## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
'
        
        ConfModARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1.g1, lambda.2_1.g2)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1.g1, lambda.3_1.g2)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1.g1, lambda.4_1.g2)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1.g1, lambda.5_1.g2)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1.g1, lambda.6_1.g2)*V11
F2 =~ c(1, 1)*V2 + c(lambda.7_2.g1, lambda.7_2.g2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.8_2.g1, lambda.8_2.g2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.9_2.g1, lambda.9_2.g2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.10_2.g1, lambda.10_2.g2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.11_2.g1, lambda.11_2.g2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.12_2.g1, lambda.12_2.g2)*V12
ARS =~ c(1, 1)*V1 + c(lambda.1_3.g1, lambda.1_3.g2)*V1
ARS =~ c(1, 1)*V3 + c(lambda.2_3.g1, lambda.2_3.g2)*V3
ARS =~ c(1, 1)*V5 + c(lambda.3_3.g1, lambda.3_3.g2)*V5
ARS =~ c(1, 1)*V7 + c(lambda.4_3.g1, lambda.4_3.g2)*V7
ARS =~ c(1, 1)*V9 + c(lambda.5_3.g1, lambda.5_3.g2)*V9
ARS =~ c(1, 1)*V11 + c(lambda.6_3.g1, lambda.6_3.g2)*V11
ARS =~ c(1, 1)*V2 + c(lambda.7_3.g1, lambda.7_3.g2)*V2
ARS =~ c(1, 1)*V4 + c(lambda.8_3.g1, lambda.8_3.g2)*V4
ARS =~ c(1, 1)*V6 + c(lambda.9_3.g1, lambda.9_3.g2)*V6
ARS =~ c(1, 1)*V8 + c(lambda.10_3.g1, lambda.10_3.g2)*V8
ARS =~ c(1, 1)*V10 + c(lambda.11_3.g1, lambda.11_3.g2)*V10
ARS =~ c(1, 1)*V12 + c(lambda.12_3.g1, lambda.12_3.g2)*V12

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1.g1, V1.thr1.g2)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2.g1, V1.thr2.g2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3.g1, V1.thr3.g2)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4.g1, V1.thr4.g2)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1.g1, V3.thr1.g2)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2.g1, V3.thr2.g2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3.g1, V3.thr3.g2)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4.g1, V3.thr4.g2)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1.g1, V5.thr1.g2)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2.g1, V5.thr2.g2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3.g1, V5.thr3.g2)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4.g1, V5.thr4.g2)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1.g1, V7.thr1.g2)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2.g1, V7.thr2.g2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3.g1, V7.thr3.g2)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4.g1, V7.thr4.g2)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1.g1, V9.thr1.g2)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2.g1, V9.thr2.g2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3.g1, V9.thr3.g2)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4.g1, V9.thr4.g2)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1.g1, V11.thr1.g2)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2.g1, V11.thr2.g2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3.g1, V11.thr3.g2)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4.g1, V11.thr4.g2)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1.g1, V2.thr1.g2)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2.g1, V2.thr2.g2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3.g1, V2.thr3.g2)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4.g1, V2.thr4.g2)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1.g1, V4.thr1.g2)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2.g1, V4.thr2.g2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3.g1, V4.thr3.g2)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4.g1, V4.thr4.g2)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1.g1, V6.thr1.g2)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2.g1, V6.thr2.g2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3.g1, V6.thr3.g2)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4.g1, V6.thr4.g2)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1.g1, V8.thr1.g2)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2.g1, V8.thr2.g2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3.g1, V8.thr3.g2)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4.g1, V8.thr4.g2)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1.g1, V10.thr1.g2)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2.g1, V10.thr2.g2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3.g1, V10.thr3.g2)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4.g1, V10.thr4.g2)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1.g1, V12.thr1.g2)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2.g1, V12.thr2.g2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3.g1, V12.thr3.g2)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4.g1, V12.thr4.g2)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, 0)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, 0)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, 0)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, 0)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, 0)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, 0)*1
V2 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, 0)*1
V4 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, 0)*1
V6 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, 0)*1
V8 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, 0)*1
V10 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, 0)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, 0)*1

## SCALING FACTORS:

V1 ~*~ c(1, 1)*V1
V3 ~*~ c(1, 1)*V3
V5 ~*~ c(1, 1)*V5
V7 ~*~ c(1, 1)*V7
V9 ~*~ c(1, 1)*V9
V11 ~*~ c(1, 1)*V11
V2 ~*~ c(1, 1)*V2
V4 ~*~ c(1, 1)*V4
V6 ~*~ c(1, 1)*V6
V8 ~*~ c(1, 1)*V8
V10 ~*~ c(1, 1)*V10
V12 ~*~ c(1, 1)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.3.g1, alpha.3.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
F1 ~~ c(0, 0)*ARS + c(psi.3_1.g1, psi.3_1.g2)*ARS
F2 ~~ c(0, 0)*ARS + c(psi.3_2.g1, psi.3_2.g2)*ARS'
        
        ThrModARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1.g1, lambda.2_1.g2)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1.g1, lambda.3_1.g2)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1.g1, lambda.4_1.g2)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1.g1, lambda.5_1.g2)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1.g1, lambda.6_1.g2)*V11
F2 =~ c(1, 1)*V2 + c(lambda.7_2.g1, lambda.7_2.g2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.8_2.g1, lambda.8_2.g2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.9_2.g1, lambda.9_2.g2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.10_2.g1, lambda.10_2.g2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.11_2.g1, lambda.11_2.g2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.12_2.g1, lambda.12_2.g2)*V12
ARS =~ c(1, 1)*V1 + c(lambda.1_3.g1, lambda.1_3.g2)*V1
ARS =~ c(1, 1)*V3 + c(lambda.2_3.g1, lambda.2_3.g2)*V3
ARS =~ c(1, 1)*V5 + c(lambda.3_3.g1, lambda.3_3.g2)*V5
ARS =~ c(1, 1)*V7 + c(lambda.4_3.g1, lambda.4_3.g2)*V7
ARS =~ c(1, 1)*V9 + c(lambda.5_3.g1, lambda.5_3.g2)*V9
ARS =~ c(1, 1)*V11 + c(lambda.6_3.g1, lambda.6_3.g2)*V11
ARS =~ c(1, 1)*V2 + c(lambda.7_3.g1, lambda.7_3.g2)*V2
ARS =~ c(1, 1)*V4 + c(lambda.8_3.g1, lambda.8_3.g2)*V4
ARS =~ c(1, 1)*V6 + c(lambda.9_3.g1, lambda.9_3.g2)*V6
ARS =~ c(1, 1)*V8 + c(lambda.10_3.g1, lambda.10_3.g2)*V8
ARS =~ c(1, 1)*V10 + c(lambda.11_3.g1, lambda.11_3.g2)*V10
ARS =~ c(1, 1)*V12 + c(lambda.12_3.g1, lambda.12_3.g2)*V12

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V2 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V4 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V6 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V8 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V10 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V3 ~*~ c(1, NA)*V3
V5 ~*~ c(1, NA)*V5
V7 ~*~ c(1, NA)*V7
V9 ~*~ c(1, NA)*V9
V11 ~*~ c(1, NA)*V11
V2 ~*~ c(1, NA)*V2
V4 ~*~ c(1, NA)*V4
V6 ~*~ c(1, NA)*V6
V8 ~*~ c(1, NA)*V8
V10 ~*~ c(1, NA)*V10
V12 ~*~ c(1, NA)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.3.g1, alpha.3.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
F1 ~~ c(0, 0)*ARS + c(psi.3_1.g1, psi.3_1.g2)*ARS
F2 ~~ c(0, 0)*ARS + c(psi.3_2.g1, psi.3_2.g2)*ARS
'
        
        LoadModARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1, lambda.2_1)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1, lambda.3_1)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1, lambda.4_1)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1, lambda.5_1)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1, lambda.6_1)*V11
F2 =~ c(1, 1)*V2 + c(lambda.7_2, lambda.7_2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.8_2, lambda.8_2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.9_2, lambda.9_2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.10_2, lambda.10_2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.11_2, lambda.11_2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.12_2, lambda.12_2)*V12
ARS =~ c(1, 1)*V1 + c(lambda.1_3, lambda.1_3)*V1
ARS =~ c(1, 1)*V3 + c(lambda.2_3, lambda.2_3)*V3
ARS =~ c(1, 1)*V5 + c(lambda.3_3, lambda.3_3)*V5
ARS =~ c(1, 1)*V7 + c(lambda.4_3, lambda.4_3)*V7
ARS =~ c(1, 1)*V9 + c(lambda.5_3, lambda.5_3)*V9
ARS =~ c(1, 1)*V11 + c(lambda.6_3, lambda.6_3)*V11
ARS =~ c(1, 1)*V2 + c(lambda.7_3, lambda.7_3)*V2
ARS =~ c(1, 1)*V4 + c(lambda.8_3, lambda.8_3)*V4
ARS =~ c(1, 1)*V6 + c(lambda.9_3, lambda.9_3)*V6
ARS =~ c(1, 1)*V8 + c(lambda.10_3, lambda.10_3)*V8
ARS =~ c(1, 1)*V10 + c(lambda.11_3, lambda.11_3)*V10
ARS =~ c(1, 1)*V12 + c(lambda.12_3, lambda.12_3)*V12

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V2 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V4 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V6 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V8 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V10 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V3 ~*~ c(1, NA)*V3
V5 ~*~ c(1, NA)*V5
V7 ~*~ c(1, NA)*V7
V9 ~*~ c(1, NA)*V9
V11 ~*~ c(1, NA)*V11
V2 ~*~ c(1, NA)*V2
V4 ~*~ c(1, NA)*V4
V6 ~*~ c(1, NA)*V6
V8 ~*~ c(1, NA)*V8
V10 ~*~ c(1, NA)*V10
V12 ~*~ c(1, NA)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.3.g1, alpha.3.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
F1 ~~ c(0, 0)*ARS + c(psi.3_1.g1, psi.3_1.g2)*ARS
F2 ~~ c(0, 0)*ARS + c(psi.3_2.g1, psi.3_2.g2)*ARS'
        
      } 
      else {
        ConfModnoARS <- '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1.g1, lambda.2_1.g2)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1.g1, lambda.3_1.g2)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1.g1, lambda.4_1.g2)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1.g1, lambda.5_1.g2)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1.g1, lambda.6_1.g2)*V11
F1 =~ c(NA, NA)*V13 + c(lambda.7_1.g1, lambda.7_1.g2)*V13
F1 =~ c(NA, NA)*V15 + c(lambda.8_1.g1, lambda.8_1.g2)*V15
F1 =~ c(NA, NA)*V17 + c(lambda.9_1.g1, lambda.9_1.g2)*V17
F1 =~ c(NA, NA)*V19 + c(lambda.10_1.g1, lambda.10_1.g2)*V19
F1 =~ c(NA, NA)*V21 + c(lambda.11_1.g1, lambda.11_1.g2)*V21
F1 =~ c(NA, NA)*V23 + c(lambda.12_1.g1, lambda.12_1.g2)*V23
F2 =~ c(1, 1)*V2 + c(lambda.13_2.g1, lambda.13_2.g2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.14_2.g1, lambda.14_2.g2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.15_2.g1, lambda.15_2.g2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.16_2.g1, lambda.16_2.g2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.17_2.g1, lambda.17_2.g2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.18_2.g1, lambda.18_2.g2)*V12
F2 =~ c(NA, NA)*V14 + c(lambda.19_2.g1, lambda.19_2.g2)*V14
F2 =~ c(NA, NA)*V16 + c(lambda.20_2.g1, lambda.20_2.g2)*V16
F2 =~ c(NA, NA)*V18 + c(lambda.21_2.g1, lambda.21_2.g2)*V18
F2 =~ c(NA, NA)*V20 + c(lambda.22_2.g1, lambda.22_2.g2)*V20
F2 =~ c(NA, NA)*V22 + c(lambda.23_2.g1, lambda.23_2.g2)*V22
F2 =~ c(NA, NA)*V24 + c(lambda.24_2.g1, lambda.24_2.g2)*V24

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1.g1, V1.thr1.g2)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2.g1, V1.thr2.g2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3.g1, V1.thr3.g2)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4.g1, V1.thr4.g2)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1.g1, V3.thr1.g2)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2.g1, V3.thr2.g2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3.g1, V3.thr3.g2)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4.g1, V3.thr4.g2)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1.g1, V5.thr1.g2)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2.g1, V5.thr2.g2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3.g1, V5.thr3.g2)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4.g1, V5.thr4.g2)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1.g1, V7.thr1.g2)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2.g1, V7.thr2.g2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3.g1, V7.thr3.g2)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4.g1, V7.thr4.g2)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1.g1, V9.thr1.g2)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2.g1, V9.thr2.g2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3.g1, V9.thr3.g2)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4.g1, V9.thr4.g2)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1.g1, V11.thr1.g2)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2.g1, V11.thr2.g2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3.g1, V11.thr3.g2)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4.g1, V11.thr4.g2)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1.g1, V13.thr1.g2)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2.g1, V13.thr2.g2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3.g1, V13.thr3.g2)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4.g1, V13.thr4.g2)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1.g1, V15.thr1.g2)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2.g1, V15.thr2.g2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3.g1, V15.thr3.g2)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4.g1, V15.thr4.g2)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1.g1, V17.thr1.g2)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2.g1, V17.thr2.g2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3.g1, V17.thr3.g2)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4.g1, V17.thr4.g2)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1.g1, V19.thr1.g2)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2.g1, V19.thr2.g2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3.g1, V19.thr3.g2)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4.g1, V19.thr4.g2)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1.g1, V21.thr1.g2)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2.g1, V21.thr2.g2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3.g1, V21.thr3.g2)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4.g1, V21.thr4.g2)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1.g1, V23.thr1.g2)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2.g1, V23.thr2.g2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3.g1, V23.thr3.g2)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4.g1, V23.thr4.g2)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1.g1, V2.thr1.g2)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2.g1, V2.thr2.g2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3.g1, V2.thr3.g2)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4.g1, V2.thr4.g2)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1.g1, V4.thr1.g2)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2.g1, V4.thr2.g2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3.g1, V4.thr3.g2)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4.g1, V4.thr4.g2)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1.g1, V6.thr1.g2)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2.g1, V6.thr2.g2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3.g1, V6.thr3.g2)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4.g1, V6.thr4.g2)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1.g1, V8.thr1.g2)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2.g1, V8.thr2.g2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3.g1, V8.thr3.g2)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4.g1, V8.thr4.g2)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1.g1, V10.thr1.g2)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2.g1, V10.thr2.g2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3.g1, V10.thr3.g2)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4.g1, V10.thr4.g2)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1.g1, V12.thr1.g2)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2.g1, V12.thr2.g2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3.g1, V12.thr3.g2)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4.g1, V12.thr4.g2)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1.g1, V14.thr1.g2)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2.g1, V14.thr2.g2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3.g1, V14.thr3.g2)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4.g1, V14.thr4.g2)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1.g1, V16.thr1.g2)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2.g1, V16.thr2.g2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3.g1, V16.thr3.g2)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4.g1, V16.thr4.g2)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1.g1, V18.thr1.g2)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2.g1, V18.thr2.g2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3.g1, V18.thr3.g2)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4.g1, V18.thr4.g2)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1.g1, V20.thr1.g2)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2.g1, V20.thr2.g2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3.g1, V20.thr3.g2)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4.g1, V20.thr4.g2)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1.g1, V22.thr1.g2)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2.g1, V22.thr2.g2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3.g1, V22.thr3.g2)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4.g1, V22.thr4.g2)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1.g1, V24.thr1.g2)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2.g1, V24.thr2.g2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3.g1, V24.thr3.g2)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4.g1, V24.thr4.g2)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, 0)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, 0)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, 0)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, 0)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, 0)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, 0)*1
V13 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, 0)*1
V15 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, 0)*1
V17 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, 0)*1
V19 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, 0)*1
V21 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, 0)*1
V23 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, 0)*1
V2 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, 0)*1
V4 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, 0)*1
V6 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, 0)*1
V8 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, 0)*1
V10 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, 0)*1
V12 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, 0)*1
V14 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, 0)*1
V16 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, 0)*1
V18 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, 0)*1
V20 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, 0)*1
V22 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, 0)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, 0)*1

## SCALING FACTORS:

V1 ~*~ c(1, 1)*V1
V3 ~*~ c(1, 1)*V3
V5 ~*~ c(1, 1)*V5
V7 ~*~ c(1, 1)*V7
V9 ~*~ c(1, 1)*V9
V11 ~*~ c(1, 1)*V11
V13 ~*~ c(1, 1)*V13
V15 ~*~ c(1, 1)*V15
V17 ~*~ c(1, 1)*V17
V19 ~*~ c(1, 1)*V19
V21 ~*~ c(1, 1)*V21
V23 ~*~ c(1, 1)*V23
V2 ~*~ c(1, 1)*V2
V4 ~*~ c(1, 1)*V4
V6 ~*~ c(1, 1)*V6
V8 ~*~ c(1, 1)*V8
V10 ~*~ c(1, 1)*V10
V12 ~*~ c(1, 1)*V12
V14 ~*~ c(1, 1)*V14
V16 ~*~ c(1, 1)*V16
V18 ~*~ c(1, 1)*V18
V20 ~*~ c(1, 1)*V20
V22 ~*~ c(1, 1)*V22
V24 ~*~ c(1, 1)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
'
        
        ThrModnoARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1.g1, lambda.2_1.g2)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1.g1, lambda.3_1.g2)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1.g1, lambda.4_1.g2)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1.g1, lambda.5_1.g2)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1.g1, lambda.6_1.g2)*V11
F1 =~ c(NA, NA)*V13 + c(lambda.7_1.g1, lambda.7_1.g2)*V13
F1 =~ c(NA, NA)*V15 + c(lambda.8_1.g1, lambda.8_1.g2)*V15
F1 =~ c(NA, NA)*V17 + c(lambda.9_1.g1, lambda.9_1.g2)*V17
F1 =~ c(NA, NA)*V19 + c(lambda.10_1.g1, lambda.10_1.g2)*V19
F1 =~ c(NA, NA)*V21 + c(lambda.11_1.g1, lambda.11_1.g2)*V21
F1 =~ c(NA, NA)*V23 + c(lambda.12_1.g1, lambda.12_1.g2)*V23
F2 =~ c(1, 1)*V2 + c(lambda.13_2.g1, lambda.13_2.g2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.14_2.g1, lambda.14_2.g2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.15_2.g1, lambda.15_2.g2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.16_2.g1, lambda.16_2.g2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.17_2.g1, lambda.17_2.g2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.18_2.g1, lambda.18_2.g2)*V12
F2 =~ c(NA, NA)*V14 + c(lambda.19_2.g1, lambda.19_2.g2)*V14
F2 =~ c(NA, NA)*V16 + c(lambda.20_2.g1, lambda.20_2.g2)*V16
F2 =~ c(NA, NA)*V18 + c(lambda.21_2.g1, lambda.21_2.g2)*V18
F2 =~ c(NA, NA)*V20 + c(lambda.22_2.g1, lambda.22_2.g2)*V20
F2 =~ c(NA, NA)*V22 + c(lambda.23_2.g1, lambda.23_2.g2)*V22
F2 =~ c(NA, NA)*V24 + c(lambda.24_2.g1, lambda.24_2.g2)*V24

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1, V13.thr1)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2, V13.thr2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3, V13.thr3)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4, V13.thr4)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1, V15.thr1)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2, V15.thr2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3, V15.thr3)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4, V15.thr4)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1, V17.thr1)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2, V17.thr2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3, V17.thr3)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4, V17.thr4)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1, V19.thr1)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2, V19.thr2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3, V19.thr3)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4, V19.thr4)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1, V21.thr1)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2, V21.thr2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3, V21.thr3)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4, V21.thr4)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1, V23.thr1)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2, V23.thr2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3, V23.thr3)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4, V23.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1, V14.thr1)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2, V14.thr2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3, V14.thr3)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4, V14.thr4)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1, V16.thr1)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2, V16.thr2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3, V16.thr3)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4, V16.thr4)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1, V18.thr1)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2, V18.thr2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3, V18.thr3)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4, V18.thr4)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1, V20.thr1)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2, V20.thr2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3, V20.thr3)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4, V20.thr4)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1, V22.thr1)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2, V22.thr2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3, V22.thr3)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4, V22.thr4)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1, V24.thr1)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2, V24.thr2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3, V24.thr3)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4, V24.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V13 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V15 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V17 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V19 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V21 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V23 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1
V2 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, NA)*1
V4 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, NA)*1
V6 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, NA)*1
V8 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, NA)*1
V10 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, NA)*1
V12 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, NA)*1
V14 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, NA)*1
V16 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, NA)*1
V18 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, NA)*1
V20 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, NA)*1
V22 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, NA)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V3 ~*~ c(1, NA)*V3
V5 ~*~ c(1, NA)*V5
V7 ~*~ c(1, NA)*V7
V9 ~*~ c(1, NA)*V9
V11 ~*~ c(1, NA)*V11
V13 ~*~ c(1, NA)*V13
V15 ~*~ c(1, NA)*V15
V17 ~*~ c(1, NA)*V17
V19 ~*~ c(1, NA)*V19
V21 ~*~ c(1, NA)*V21
V23 ~*~ c(1, NA)*V23
V2 ~*~ c(1, NA)*V2
V4 ~*~ c(1, NA)*V4
V6 ~*~ c(1, NA)*V6
V8 ~*~ c(1, NA)*V8
V10 ~*~ c(1, NA)*V10
V12 ~*~ c(1, NA)*V12
V14 ~*~ c(1, NA)*V14
V16 ~*~ c(1, NA)*V16
V18 ~*~ c(1, NA)*V18
V20 ~*~ c(1, NA)*V20
V22 ~*~ c(1, NA)*V22
V24 ~*~ c(1, NA)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2'
        
        LoadModnoARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1, lambda.2_1)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1, lambda.3_1)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1, lambda.4_1)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1, lambda.5_1)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1, lambda.6_1)*V11
F1 =~ c(NA, NA)*V13 + c(lambda.7_1, lambda.7_1)*V13
F1 =~ c(NA, NA)*V15 + c(lambda.8_1, lambda.8_1)*V15
F1 =~ c(NA, NA)*V17 + c(lambda.9_1, lambda.9_1)*V17
F1 =~ c(NA, NA)*V19 + c(lambda.10_1, lambda.10_1)*V19
F1 =~ c(NA, NA)*V21 + c(lambda.11_1, lambda.11_1)*V21
F1 =~ c(NA, NA)*V23 + c(lambda.12_1, lambda.12_1)*V23
F2 =~ c(1, 1)*V2 + c(lambda.13_2, lambda.13_2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.14_2, lambda.14_2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.15_2, lambda.15_2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.16_2, lambda.16_2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.17_2, lambda.17_2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.18_2, lambda.18_2)*V12
F2 =~ c(NA, NA)*V14 + c(lambda.19_2, lambda.19_2)*V14
F2 =~ c(NA, NA)*V16 + c(lambda.20_2, lambda.20_2)*V16
F2 =~ c(NA, NA)*V18 + c(lambda.21_2, lambda.21_2)*V18
F2 =~ c(NA, NA)*V20 + c(lambda.22_2, lambda.22_2)*V20
F2 =~ c(NA, NA)*V22 + c(lambda.23_2, lambda.23_2)*V22
F2 =~ c(NA, NA)*V24 + c(lambda.24_2, lambda.24_2)*V24

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1, V13.thr1)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2, V13.thr2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3, V13.thr3)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4, V13.thr4)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1, V15.thr1)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2, V15.thr2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3, V15.thr3)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4, V15.thr4)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1, V17.thr1)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2, V17.thr2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3, V17.thr3)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4, V17.thr4)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1, V19.thr1)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2, V19.thr2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3, V19.thr3)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4, V19.thr4)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1, V21.thr1)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2, V21.thr2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3, V21.thr3)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4, V21.thr4)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1, V23.thr1)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2, V23.thr2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3, V23.thr3)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4, V23.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1, V14.thr1)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2, V14.thr2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3, V14.thr3)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4, V14.thr4)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1, V16.thr1)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2, V16.thr2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3, V16.thr3)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4, V16.thr4)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1, V18.thr1)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2, V18.thr2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3, V18.thr3)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4, V18.thr4)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1, V20.thr1)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2, V20.thr2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3, V20.thr3)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4, V20.thr4)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1, V22.thr1)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2, V22.thr2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3, V22.thr3)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4, V22.thr4)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1, V24.thr1)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2, V24.thr2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3, V24.thr3)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4, V24.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V13 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V15 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V17 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V19 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V21 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V23 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1
V2 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, NA)*1
V4 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, NA)*1
V6 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, NA)*1
V8 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, NA)*1
V10 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, NA)*1
V12 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, NA)*1
V14 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, NA)*1
V16 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, NA)*1
V18 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, NA)*1
V20 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, NA)*1
V22 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, NA)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V3 ~*~ c(1, NA)*V3
V5 ~*~ c(1, NA)*V5
V7 ~*~ c(1, NA)*V7
V9 ~*~ c(1, NA)*V9
V11 ~*~ c(1, NA)*V11
V13 ~*~ c(1, NA)*V13
V15 ~*~ c(1, NA)*V15
V17 ~*~ c(1, NA)*V17
V19 ~*~ c(1, NA)*V19
V21 ~*~ c(1, NA)*V21
V23 ~*~ c(1, NA)*V23
V2 ~*~ c(1, NA)*V2
V4 ~*~ c(1, NA)*V4
V6 ~*~ c(1, NA)*V6
V8 ~*~ c(1, NA)*V8
V10 ~*~ c(1, NA)*V10
V12 ~*~ c(1, NA)*V12
V14 ~*~ c(1, NA)*V14
V16 ~*~ c(1, NA)*V16
V18 ~*~ c(1, NA)*V18
V20 ~*~ c(1, NA)*V20
V22 ~*~ c(1, NA)*V22
V24 ~*~ c(1, NA)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:


## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
'
        
        ConfModARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1.g1, lambda.2_1.g2)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1.g1, lambda.3_1.g2)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1.g1, lambda.4_1.g2)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1.g1, lambda.5_1.g2)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1.g1, lambda.6_1.g2)*V11
F1 =~ c(NA, NA)*V13 + c(lambda.7_1.g1, lambda.7_1.g2)*V13
F1 =~ c(NA, NA)*V15 + c(lambda.8_1.g1, lambda.8_1.g2)*V15
F1 =~ c(NA, NA)*V17 + c(lambda.9_1.g1, lambda.9_1.g2)*V17
F1 =~ c(NA, NA)*V19 + c(lambda.10_1.g1, lambda.10_1.g2)*V19
F1 =~ c(NA, NA)*V21 + c(lambda.11_1.g1, lambda.11_1.g2)*V21
F1 =~ c(NA, NA)*V23 + c(lambda.12_1.g1, lambda.12_1.g2)*V23
F2 =~ c(1, 1)*V2 + c(lambda.13_2.g1, lambda.13_2.g2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.14_2.g1, lambda.14_2.g2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.15_2.g1, lambda.15_2.g2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.16_2.g1, lambda.16_2.g2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.17_2.g1, lambda.17_2.g2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.18_2.g1, lambda.18_2.g2)*V12
F2 =~ c(NA, NA)*V14 + c(lambda.19_2.g1, lambda.19_2.g2)*V14
F2 =~ c(NA, NA)*V16 + c(lambda.20_2.g1, lambda.20_2.g2)*V16
F2 =~ c(NA, NA)*V18 + c(lambda.21_2.g1, lambda.21_2.g2)*V18
F2 =~ c(NA, NA)*V20 + c(lambda.22_2.g1, lambda.22_2.g2)*V20
F2 =~ c(NA, NA)*V22 + c(lambda.23_2.g1, lambda.23_2.g2)*V22
F2 =~ c(NA, NA)*V24 + c(lambda.24_2.g1, lambda.24_2.g2)*V24
ARS =~ c(1, 1)*V1 + c(lambda.1_3.g1, lambda.1_3.g2)*V1
ARS =~ c(1, 1)*V3 + c(lambda.2_3.g1, lambda.2_3.g2)*V3
ARS =~ c(1, 1)*V5 + c(lambda.3_3.g1, lambda.3_3.g2)*V5
ARS =~ c(1, 1)*V7 + c(lambda.4_3.g1, lambda.4_3.g2)*V7
ARS =~ c(1, 1)*V9 + c(lambda.5_3.g1, lambda.5_3.g2)*V9
ARS =~ c(1, 1)*V11 + c(lambda.6_3.g1, lambda.6_3.g2)*V11
ARS =~ c(1, 1)*V13 + c(lambda.7_3.g1, lambda.7_3.g2)*V13
ARS =~ c(1, 1)*V15 + c(lambda.8_3.g1, lambda.8_3.g2)*V15
ARS =~ c(1, 1)*V17 + c(lambda.9_3.g1, lambda.9_3.g2)*V17
ARS =~ c(1, 1)*V19 + c(lambda.10_3.g1, lambda.10_3.g2)*V19
ARS =~ c(1, 1)*V21 + c(lambda.11_3.g1, lambda.11_3.g2)*V21
ARS =~ c(1, 1)*V23 + c(lambda.12_3.g1, lambda.12_3.g2)*V23
ARS =~ c(1, 1)*V2 + c(lambda.13_3.g1, lambda.13_3.g2)*V2
ARS =~ c(1, 1)*V4 + c(lambda.14_3.g1, lambda.14_3.g2)*V4
ARS =~ c(1, 1)*V6 + c(lambda.15_3.g1, lambda.15_3.g2)*V6
ARS =~ c(1, 1)*V8 + c(lambda.16_3.g1, lambda.16_3.g2)*V8
ARS =~ c(1, 1)*V10 + c(lambda.17_3.g1, lambda.17_3.g2)*V10
ARS =~ c(1, 1)*V12 + c(lambda.18_3.g1, lambda.18_3.g2)*V12
ARS =~ c(1, 1)*V14 + c(lambda.19_3.g1, lambda.19_3.g2)*V14
ARS =~ c(1, 1)*V16 + c(lambda.20_3.g1, lambda.20_3.g2)*V16
ARS =~ c(1, 1)*V18 + c(lambda.21_3.g1, lambda.21_3.g2)*V18
ARS =~ c(1, 1)*V20 + c(lambda.22_3.g1, lambda.22_3.g2)*V20
ARS =~ c(1, 1)*V22 + c(lambda.23_3.g1, lambda.23_3.g2)*V22
ARS =~ c(1, 1)*V24 + c(lambda.24_3.g1, lambda.24_3.g2)*V24

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1.g1, V1.thr1.g2)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2.g1, V1.thr2.g2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3.g1, V1.thr3.g2)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4.g1, V1.thr4.g2)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1.g1, V3.thr1.g2)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2.g1, V3.thr2.g2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3.g1, V3.thr3.g2)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4.g1, V3.thr4.g2)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1.g1, V5.thr1.g2)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2.g1, V5.thr2.g2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3.g1, V5.thr3.g2)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4.g1, V5.thr4.g2)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1.g1, V7.thr1.g2)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2.g1, V7.thr2.g2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3.g1, V7.thr3.g2)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4.g1, V7.thr4.g2)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1.g1, V9.thr1.g2)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2.g1, V9.thr2.g2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3.g1, V9.thr3.g2)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4.g1, V9.thr4.g2)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1.g1, V11.thr1.g2)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2.g1, V11.thr2.g2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3.g1, V11.thr3.g2)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4.g1, V11.thr4.g2)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1.g1, V13.thr1.g2)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2.g1, V13.thr2.g2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3.g1, V13.thr3.g2)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4.g1, V13.thr4.g2)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1.g1, V15.thr1.g2)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2.g1, V15.thr2.g2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3.g1, V15.thr3.g2)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4.g1, V15.thr4.g2)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1.g1, V17.thr1.g2)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2.g1, V17.thr2.g2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3.g1, V17.thr3.g2)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4.g1, V17.thr4.g2)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1.g1, V19.thr1.g2)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2.g1, V19.thr2.g2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3.g1, V19.thr3.g2)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4.g1, V19.thr4.g2)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1.g1, V21.thr1.g2)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2.g1, V21.thr2.g2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3.g1, V21.thr3.g2)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4.g1, V21.thr4.g2)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1.g1, V23.thr1.g2)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2.g1, V23.thr2.g2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3.g1, V23.thr3.g2)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4.g1, V23.thr4.g2)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1.g1, V2.thr1.g2)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2.g1, V2.thr2.g2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3.g1, V2.thr3.g2)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4.g1, V2.thr4.g2)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1.g1, V4.thr1.g2)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2.g1, V4.thr2.g2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3.g1, V4.thr3.g2)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4.g1, V4.thr4.g2)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1.g1, V6.thr1.g2)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2.g1, V6.thr2.g2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3.g1, V6.thr3.g2)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4.g1, V6.thr4.g2)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1.g1, V8.thr1.g2)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2.g1, V8.thr2.g2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3.g1, V8.thr3.g2)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4.g1, V8.thr4.g2)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1.g1, V10.thr1.g2)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2.g1, V10.thr2.g2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3.g1, V10.thr3.g2)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4.g1, V10.thr4.g2)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1.g1, V12.thr1.g2)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2.g1, V12.thr2.g2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3.g1, V12.thr3.g2)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4.g1, V12.thr4.g2)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1.g1, V14.thr1.g2)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2.g1, V14.thr2.g2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3.g1, V14.thr3.g2)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4.g1, V14.thr4.g2)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1.g1, V16.thr1.g2)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2.g1, V16.thr2.g2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3.g1, V16.thr3.g2)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4.g1, V16.thr4.g2)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1.g1, V18.thr1.g2)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2.g1, V18.thr2.g2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3.g1, V18.thr3.g2)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4.g1, V18.thr4.g2)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1.g1, V20.thr1.g2)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2.g1, V20.thr2.g2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3.g1, V20.thr3.g2)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4.g1, V20.thr4.g2)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1.g1, V22.thr1.g2)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2.g1, V22.thr2.g2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3.g1, V22.thr3.g2)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4.g1, V22.thr4.g2)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1.g1, V24.thr1.g2)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2.g1, V24.thr2.g2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3.g1, V24.thr3.g2)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4.g1, V24.thr4.g2)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, 0)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, 0)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, 0)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, 0)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, 0)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, 0)*1
V13 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, 0)*1
V15 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, 0)*1
V17 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, 0)*1
V19 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, 0)*1
V21 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, 0)*1
V23 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, 0)*1
V2 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, 0)*1
V4 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, 0)*1
V6 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, 0)*1
V8 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, 0)*1
V10 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, 0)*1
V12 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, 0)*1
V14 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, 0)*1
V16 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, 0)*1
V18 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, 0)*1
V20 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, 0)*1
V22 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, 0)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, 0)*1

## SCALING FACTORS:

V1 ~*~ c(1, 1)*V1
V3 ~*~ c(1, 1)*V3
V5 ~*~ c(1, 1)*V5
V7 ~*~ c(1, 1)*V7
V9 ~*~ c(1, 1)*V9
V11 ~*~ c(1, 1)*V11
V13 ~*~ c(1, 1)*V13
V15 ~*~ c(1, 1)*V15
V17 ~*~ c(1, 1)*V17
V19 ~*~ c(1, 1)*V19
V21 ~*~ c(1, 1)*V21
V23 ~*~ c(1, 1)*V23
V2 ~*~ c(1, 1)*V2
V4 ~*~ c(1, 1)*V4
V6 ~*~ c(1, 1)*V6
V8 ~*~ c(1, 1)*V8
V10 ~*~ c(1, 1)*V10
V12 ~*~ c(1, 1)*V12
V14 ~*~ c(1, 1)*V14
V16 ~*~ c(1, 1)*V16
V18 ~*~ c(1, 1)*V18
V20 ~*~ c(1, 1)*V20
V22 ~*~ c(1, 1)*V22
V24 ~*~ c(1, 1)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.3.g1, alpha.3.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
F1 ~~ c(0, 0)*ARS + c(psi.3_1.g1, psi.3_1.g2)*ARS
F2 ~~ c(0, 0)*ARS + c(psi.3_2.g1, psi.3_2.g2)*ARS
'
        
        ThrModARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1.g1, lambda.2_1.g2)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1.g1, lambda.3_1.g2)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1.g1, lambda.4_1.g2)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1.g1, lambda.5_1.g2)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1.g1, lambda.6_1.g2)*V11
F1 =~ c(NA, NA)*V13 + c(lambda.7_1.g1, lambda.7_1.g2)*V13
F1 =~ c(NA, NA)*V15 + c(lambda.8_1.g1, lambda.8_1.g2)*V15
F1 =~ c(NA, NA)*V17 + c(lambda.9_1.g1, lambda.9_1.g2)*V17
F1 =~ c(NA, NA)*V19 + c(lambda.10_1.g1, lambda.10_1.g2)*V19
F1 =~ c(NA, NA)*V21 + c(lambda.11_1.g1, lambda.11_1.g2)*V21
F1 =~ c(NA, NA)*V23 + c(lambda.12_1.g1, lambda.12_1.g2)*V23
F2 =~ c(1, 1)*V2 + c(lambda.13_2.g1, lambda.13_2.g2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.14_2.g1, lambda.14_2.g2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.15_2.g1, lambda.15_2.g2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.16_2.g1, lambda.16_2.g2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.17_2.g1, lambda.17_2.g2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.18_2.g1, lambda.18_2.g2)*V12
F2 =~ c(NA, NA)*V14 + c(lambda.19_2.g1, lambda.19_2.g2)*V14
F2 =~ c(NA, NA)*V16 + c(lambda.20_2.g1, lambda.20_2.g2)*V16
F2 =~ c(NA, NA)*V18 + c(lambda.21_2.g1, lambda.21_2.g2)*V18
F2 =~ c(NA, NA)*V20 + c(lambda.22_2.g1, lambda.22_2.g2)*V20
F2 =~ c(NA, NA)*V22 + c(lambda.23_2.g1, lambda.23_2.g2)*V22
F2 =~ c(NA, NA)*V24 + c(lambda.24_2.g1, lambda.24_2.g2)*V24
ARS =~ c(1, 1)*V1 + c(lambda.1_3.g1, lambda.1_3.g2)*V1
ARS =~ c(1, 1)*V3 + c(lambda.2_3.g1, lambda.2_3.g2)*V3
ARS =~ c(1, 1)*V5 + c(lambda.3_3.g1, lambda.3_3.g2)*V5
ARS =~ c(1, 1)*V7 + c(lambda.4_3.g1, lambda.4_3.g2)*V7
ARS =~ c(1, 1)*V9 + c(lambda.5_3.g1, lambda.5_3.g2)*V9
ARS =~ c(1, 1)*V11 + c(lambda.6_3.g1, lambda.6_3.g2)*V11
ARS =~ c(1, 1)*V13 + c(lambda.7_3.g1, lambda.7_3.g2)*V13
ARS =~ c(1, 1)*V15 + c(lambda.8_3.g1, lambda.8_3.g2)*V15
ARS =~ c(1, 1)*V17 + c(lambda.9_3.g1, lambda.9_3.g2)*V17
ARS =~ c(1, 1)*V19 + c(lambda.10_3.g1, lambda.10_3.g2)*V19
ARS =~ c(1, 1)*V21 + c(lambda.11_3.g1, lambda.11_3.g2)*V21
ARS =~ c(1, 1)*V23 + c(lambda.12_3.g1, lambda.12_3.g2)*V23
ARS =~ c(1, 1)*V2 + c(lambda.13_3.g1, lambda.13_3.g2)*V2
ARS =~ c(1, 1)*V4 + c(lambda.14_3.g1, lambda.14_3.g2)*V4
ARS =~ c(1, 1)*V6 + c(lambda.15_3.g1, lambda.15_3.g2)*V6
ARS =~ c(1, 1)*V8 + c(lambda.16_3.g1, lambda.16_3.g2)*V8
ARS =~ c(1, 1)*V10 + c(lambda.17_3.g1, lambda.17_3.g2)*V10
ARS =~ c(1, 1)*V12 + c(lambda.18_3.g1, lambda.18_3.g2)*V12
ARS =~ c(1, 1)*V14 + c(lambda.19_3.g1, lambda.19_3.g2)*V14
ARS =~ c(1, 1)*V16 + c(lambda.20_3.g1, lambda.20_3.g2)*V16
ARS =~ c(1, 1)*V18 + c(lambda.21_3.g1, lambda.21_3.g2)*V18
ARS =~ c(1, 1)*V20 + c(lambda.22_3.g1, lambda.22_3.g2)*V20
ARS =~ c(1, 1)*V22 + c(lambda.23_3.g1, lambda.23_3.g2)*V22
ARS =~ c(1, 1)*V24 + c(lambda.24_3.g1, lambda.24_3.g2)*V24

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1, V13.thr1)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2, V13.thr2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3, V13.thr3)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4, V13.thr4)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1, V15.thr1)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2, V15.thr2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3, V15.thr3)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4, V15.thr4)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1, V17.thr1)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2, V17.thr2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3, V17.thr3)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4, V17.thr4)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1, V19.thr1)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2, V19.thr2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3, V19.thr3)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4, V19.thr4)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1, V21.thr1)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2, V21.thr2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3, V21.thr3)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4, V21.thr4)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1, V23.thr1)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2, V23.thr2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3, V23.thr3)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4, V23.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1, V14.thr1)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2, V14.thr2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3, V14.thr3)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4, V14.thr4)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1, V16.thr1)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2, V16.thr2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3, V16.thr3)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4, V16.thr4)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1, V18.thr1)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2, V18.thr2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3, V18.thr3)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4, V18.thr4)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1, V20.thr1)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2, V20.thr2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3, V20.thr3)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4, V20.thr4)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1, V22.thr1)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2, V22.thr2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3, V22.thr3)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4, V22.thr4)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1, V24.thr1)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2, V24.thr2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3, V24.thr3)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4, V24.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V13 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V15 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V17 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V19 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V21 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V23 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1
V2 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, NA)*1
V4 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, NA)*1
V6 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, NA)*1
V8 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, NA)*1
V10 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, NA)*1
V12 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, NA)*1
V14 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, NA)*1
V16 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, NA)*1
V18 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, NA)*1
V20 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, NA)*1
V22 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, NA)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V3 ~*~ c(1, NA)*V3
V5 ~*~ c(1, NA)*V5
V7 ~*~ c(1, NA)*V7
V9 ~*~ c(1, NA)*V9
V11 ~*~ c(1, NA)*V11
V13 ~*~ c(1, NA)*V13
V15 ~*~ c(1, NA)*V15
V17 ~*~ c(1, NA)*V17
V19 ~*~ c(1, NA)*V19
V21 ~*~ c(1, NA)*V21
V23 ~*~ c(1, NA)*V23
V2 ~*~ c(1, NA)*V2
V4 ~*~ c(1, NA)*V4
V6 ~*~ c(1, NA)*V6
V8 ~*~ c(1, NA)*V8
V10 ~*~ c(1, NA)*V10
V12 ~*~ c(1, NA)*V12
V14 ~*~ c(1, NA)*V14
V16 ~*~ c(1, NA)*V16
V18 ~*~ c(1, NA)*V18
V20 ~*~ c(1, NA)*V20
V22 ~*~ c(1, NA)*V22
V24 ~*~ c(1, NA)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.3.g1, alpha.3.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
F1 ~~ c(0, 0)*ARS + c(psi.3_1.g1, psi.3_1.g2)*ARS
F2 ~~ c(0, 0)*ARS + c(psi.3_2.g1, psi.3_2.g2)*ARS
'
        
        LoadModARS <- 
          '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1, lambda.2_1)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1, lambda.3_1)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1, lambda.4_1)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1, lambda.5_1)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1, lambda.6_1)*V11
F1 =~ c(NA, NA)*V13 + c(lambda.7_1, lambda.7_1)*V13
F1 =~ c(NA, NA)*V15 + c(lambda.8_1, lambda.8_1)*V15
F1 =~ c(NA, NA)*V17 + c(lambda.9_1, lambda.9_1)*V17
F1 =~ c(NA, NA)*V19 + c(lambda.10_1, lambda.10_1)*V19
F1 =~ c(NA, NA)*V21 + c(lambda.11_1, lambda.11_1)*V21
F1 =~ c(NA, NA)*V23 + c(lambda.12_1, lambda.12_1)*V23
F2 =~ c(1, 1)*V2 + c(lambda.13_2, lambda.13_2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.14_2, lambda.14_2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.15_2, lambda.15_2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.16_2, lambda.16_2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.17_2, lambda.17_2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.18_2, lambda.18_2)*V12
F2 =~ c(NA, NA)*V14 + c(lambda.19_2, lambda.19_2)*V14
F2 =~ c(NA, NA)*V16 + c(lambda.20_2, lambda.20_2)*V16
F2 =~ c(NA, NA)*V18 + c(lambda.21_2, lambda.21_2)*V18
F2 =~ c(NA, NA)*V20 + c(lambda.22_2, lambda.22_2)*V20
F2 =~ c(NA, NA)*V22 + c(lambda.23_2, lambda.23_2)*V22
F2 =~ c(NA, NA)*V24 + c(lambda.24_2, lambda.24_2)*V24
ARS =~ c(1, 1)*V1 + c(lambda.1_3, lambda.1_3)*V1
ARS =~ c(1, 1)*V3 + c(lambda.2_3, lambda.2_3)*V3
ARS =~ c(1, 1)*V5 + c(lambda.3_3, lambda.3_3)*V5
ARS =~ c(1, 1)*V7 + c(lambda.4_3, lambda.4_3)*V7
ARS =~ c(1, 1)*V9 + c(lambda.5_3, lambda.5_3)*V9
ARS =~ c(1, 1)*V11 + c(lambda.6_3, lambda.6_3)*V11
ARS =~ c(1, 1)*V13 + c(lambda.7_3, lambda.7_3)*V13
ARS =~ c(1, 1)*V15 + c(lambda.8_3, lambda.8_3)*V15
ARS =~ c(1, 1)*V17 + c(lambda.9_3, lambda.9_3)*V17
ARS =~ c(1, 1)*V19 + c(lambda.10_3, lambda.10_3)*V19
ARS =~ c(1, 1)*V21 + c(lambda.11_3, lambda.11_3)*V21
ARS =~ c(1, 1)*V23 + c(lambda.12_3, lambda.12_3)*V23
ARS =~ c(1, 1)*V2 + c(lambda.13_3, lambda.13_3)*V2
ARS =~ c(1, 1)*V4 + c(lambda.14_3, lambda.14_3)*V4
ARS =~ c(1, 1)*V6 + c(lambda.15_3, lambda.15_3)*V6
ARS =~ c(1, 1)*V8 + c(lambda.16_3, lambda.16_3)*V8
ARS =~ c(1, 1)*V10 + c(lambda.17_3, lambda.17_3)*V10
ARS =~ c(1, 1)*V12 + c(lambda.18_3, lambda.18_3)*V12
ARS =~ c(1, 1)*V14 + c(lambda.19_3, lambda.19_3)*V14
ARS =~ c(1, 1)*V16 + c(lambda.20_3, lambda.20_3)*V16
ARS =~ c(1, 1)*V18 + c(lambda.21_3, lambda.21_3)*V18
ARS =~ c(1, 1)*V20 + c(lambda.22_3, lambda.22_3)*V20
ARS =~ c(1, 1)*V22 + c(lambda.23_3, lambda.23_3)*V22
ARS =~ c(1, 1)*V24 + c(lambda.24_3, lambda.24_3)*V24

## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1, V13.thr1)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2, V13.thr2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3, V13.thr3)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4, V13.thr4)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1, V15.thr1)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2, V15.thr2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3, V15.thr3)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4, V15.thr4)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1, V17.thr1)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2, V17.thr2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3, V17.thr3)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4, V17.thr4)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1, V19.thr1)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2, V19.thr2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3, V19.thr3)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4, V19.thr4)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1, V21.thr1)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2, V21.thr2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3, V21.thr3)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4, V21.thr4)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1, V23.thr1)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2, V23.thr2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3, V23.thr3)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4, V23.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1, V14.thr1)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2, V14.thr2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3, V14.thr3)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4, V14.thr4)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1, V16.thr1)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2, V16.thr2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3, V16.thr3)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4, V16.thr4)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1, V18.thr1)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2, V18.thr2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3, V18.thr3)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4, V18.thr4)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1, V20.thr1)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2, V20.thr2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3, V20.thr3)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4, V20.thr4)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1, V22.thr1)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2, V22.thr2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3, V22.thr3)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4, V22.thr4)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1, V24.thr1)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2, V24.thr2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3, V24.thr3)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4, V24.thr4)*t4

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V13 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V15 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V17 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V19 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V21 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V23 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1
V2 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, NA)*1
V4 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, NA)*1
V6 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, NA)*1
V8 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, NA)*1
V10 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, NA)*1
V12 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, NA)*1
V14 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, NA)*1
V16 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, NA)*1
V18 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, NA)*1
V20 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, NA)*1
V22 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, NA)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V3 ~*~ c(1, NA)*V3
V5 ~*~ c(1, NA)*V5
V7 ~*~ c(1, NA)*V7
V9 ~*~ c(1, NA)*V9
V11 ~*~ c(1, NA)*V11
V13 ~*~ c(1, NA)*V13
V15 ~*~ c(1, NA)*V15
V17 ~*~ c(1, NA)*V17
V19 ~*~ c(1, NA)*V19
V21 ~*~ c(1, NA)*V21
V23 ~*~ c(1, NA)*V23
V2 ~*~ c(1, NA)*V2
V4 ~*~ c(1, NA)*V4
V6 ~*~ c(1, NA)*V6
V8 ~*~ c(1, NA)*V8
V10 ~*~ c(1, NA)*V10
V12 ~*~ c(1, NA)*V12
V14 ~*~ c(1, NA)*V14
V16 ~*~ c(1, NA)*V16
V18 ~*~ c(1, NA)*V18
V20 ~*~ c(1, NA)*V20
V22 ~*~ c(1, NA)*V22
V24 ~*~ c(1, NA)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.3.g1, alpha.3.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
F1 ~~ c(0, 0)*ARS + c(psi.3_1.g1, psi.3_1.g2)*ARS
F2 ~~ c(0, 0)*ARS + c(psi.3_2.g1, psi.3_2.g2)*ARS
'
        
      }
      
    }
  } else {
    
    
    
  }
  
  results <- list()      
  results[[1]] <- ConfModnoARS
  results[[2]] <- ThrModnoARS
  results[[3]] <- LoadModnoARS
  results[[4]] <- ConfModARS
  results[[5]] <- ThrModARS
  results[[6]] <- LoadModARS
  
  names(results) <- c("ConfModnoARS", "ThrModnoARS", "LoadModnoARS", "ConfModARS", "ThrModARS", "LoadModARS")      
  return(results)
}

Estimate.MI.Models <- function(Data, Models){
  
  
  
  
  #Configural
  fit.conf <-  myTryCatch(cfa(as.character(Models$ConfModnoARS), 
                  data = Data, 
                  group = "group", 
                  ordered = colnames(Data[,-c(dim(Data)[2])]),
                  estimator = "WLSMV"))  
  
  fit.confARS <-  myTryCatch(cfa(as.character(Models$ConfModARS), 
                     data = Data, 
                     group = "group", 
                     ordered = colnames(Data[,-c(dim(Data)[2])]),
                     estimator = "WLSMV"))
  
  #Thresholds invariant (prop. 4 Wu and Estabrook, 2016)    
  
  fit.thr <-  myTryCatch(cfa(as.character(Models$ThrModnoARS), 
                 data = Data, 
                 group = "group", 
                 ordered = colnames(Data[,-c(dim(Data)[2])]),
                 estimator = "WLSMV") )
  fit.thrARS <-  myTryCatch(cfa(as.character(Models$ThrModARS), 
                    data = Data, 
                    group = "group", 
                    ordered = colnames(Data[,-c(dim(Data)[2])]),
                    estimator = "WLSMV")  )
  
  
  #Thresholds and loadings invariant (prop. 7 Wu and Estabrook, 2016)    
  
  fit.load <-  myTryCatch(cfa(as.character(Models$LoadModnoARS), 
                  data = Data, 
                  group = "group", 
                  ordered = colnames(Data[,-c(dim(Data)[2])]),
                  estimator = "WLSMV")  )
  
  fit.loadARS <-  myTryCatch(cfa(as.character(Models$LoadModARS), 
                     data = Data, 
                     group = "group", 
                     ordered = colnames(Data[,-c(dim(Data)[2])]),
                     estimator = "WLSMV")  )
  
  
  
  Models <- list()
  Models[[1]] <- fit.conf
  Models[[2]] <- fit.load
  Models[[3]] <- fit.thr
  Models[[4]] <- fit.confARS
  Models[[5]] <- fit.loadARS
  Models[[6]] <- fit.thrARS
  
  names(Models) <- c('Mod.Conf', 'Mod.Load', 'Mod.Thr', 
                     'Mod.ConfARS', 'Mod.LoadARS', 'Mod.ThrARS')
  
  Results.fit <- list()
  
  
  
  for(i in 1:length(Models)){
    if(is.na(Models[[i]]$value@Fit@se[2]) ){
      Results.fit[[i]] <- Models[[i]]$warning
    } else {
      Results.fit[[i]] <- c(fitmeasures(Models[[i]]$value))
    }
  }
  
  
#  Results.fit[[1]] <- fitmeasures(fit.conf$value)
#  Results.fit[[2]] <- fitmeasures(fit.thr$value)
#  Results.fit[[3]] <- fitmeasures(fit.load$value)
#  Results.fit[[4]] <- fitmeasures(fit.confARS$value)
#  Results.fit[[5]] <- fitmeasures(fit.thrARS$value)
#  Results.fit[[6]] <- fitmeasures(fit.loadARS$value)
  
  names(Results.fit) <- c('Conf.fit', 'Thr.fit', 'Load.fit', 
                          'Conf.fitARS', 'Thr.fitARS', 'Load.fitARS')

#LavtestLRT and differential fit measures results  
  Comparative.fit <- list()

#Adjust this code to make sure that non-converged models are taken into account and those results are  
#  if(any( c(any(is.na(Models$Mod.Conf$value@Fit@se)), any(is.na(Models$Mod.Thr$value@Fit@se[2])),
#            any(is.na(Models$Mod.Load$value@Fit@se[2])) ))){
#    Comparative.fit[[1]] <- Models$Mod.Conf$warning
#  } else {
    Comparative.fit[[1]] <- c( myTryCatch(lavTestLRT(Models$Mod.Conf$value, Models$Mod.Thr$value, Models$Mod.Load$value)), 
                              myTryCatch(Results.fit$Thr.fit[c("rmsea.scaled", "cfi.scaled")] - Results.fit$Conf.fit[c("rmsea.scaled", "cfi.scaled")]),
                              myTryCatch(Results.fit$Load.fit[c("rmsea.scaled", "cfi.scaled")] - Results.fit$Thr.fit[c("rmsea.scaled", "cfi.scaled")]))
#  }
  
#  if(any( c(any(is.na(Models$Mod.ConfARS$value@Fit@se)), any(is.na(Models$Mod.ThrARS$value@Fit@se[2])),
#            any(is.na(Models$Mod.LoadARS$value@Fit@se[2])) ))){
#    Comparative.fit[[2]] <- Models$Mod.ConfARS$warning
 # } else {
    Comparative.fit[[2]] <- c(myTryCatch(lavTestLRT(Models$Mod.ConfARS$value, Models$Mod.ThrARS$value, Models$Mod.LoadARS$value)), 
                              myTryCatch(Results.fit$Thr.fitARS[c("rmsea.scaled", "cfi.scaled")] - Results.fit$Conf.fitARS[c("rmsea.scaled", "cfi.scaled")]),
                              myTryCatch(Results.fit$Load.fitARS[c("rmsea.scaled", "cfi.scaled")] - Results.fit$Thr.fitARS[c("rmsea.scaled", "cfi.scaled")]))
#  }
  
  
#  if(any( c(any(is.na(Models$Mod.Conf$value@Fit@se)), any(is.na(Models$Mod.ConfARS$value@Fit@se[2])) ))){
#    Comparative.fit[[3]] <- Models$Mod.ConfARS$warning
#  } else {
    Comparative.fit[[3]] <- c(myTryCatch(lavTestLRT(Models$Mod.Conf$value, Models$Mod.ConfARS$value)))
#  }
  
#  Comparative.fit[[1]] <- c(lavTestLRT(Models$Mod.Conf$value, Models$Mod.Thr$value, Models$Mod.Load$value), 
#                           Results.fit$Thr.fit[c("rmsea.scaled", "cfi.scaled")] - Results.fit$Conf.fit[c("rmsea.scaled", "cfi.scaled")],
#                            Results.fit$Load.fit[c("rmsea.scaled", "cfi.scaled")] - Results.fit$Thr.fit[c("rmsea.scaled", "cfi.scaled")])

  
  names(Comparative.fit) <- c("NoARSModels", "ARSModels", "ARSvsNoArs")
  
  results <- list()
#  results[[1]] <- Models
  results[[2]] <- Results.fit
  results[[3]] <- Comparative.fit
  
  names(results) <- c("Models.fit", "fit.measures", "Comparative.fit")
  return(results)
}

Model_Creation.item <- function(Nfac, nitems, cat){
  
  
  if(Nfac == 1){
    if(nitems == 12){
      
      FullyConstrained <- c('

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V2 ~*~ c(1, NA)*V2
V3 ~*~ c(1, NA)*V3
V4 ~*~ c(1, NA)*V4
V5 ~*~ c(1, NA)*V5
V6 ~*~ c(1, NA)*V6
V7 ~*~ c(1, NA)*V7
V8 ~*~ c(1, NA)*V8
V9 ~*~ c(1, NA)*V9
V10 ~*~ c(1, NA)*V10
V11 ~*~ c(1, NA)*V11
V12 ~*~ c(1, NA)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:
'  , 
                            
                            '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1.g1, lambda.1_1.g2)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1, lambda.2_1)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1, lambda.3_1)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1, lambda.4_1)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1, lambda.5_1)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1, lambda.6_1)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1, lambda.7_1)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1, lambda.8_1)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1, lambda.9_1)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1, lambda.10_1)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1, lambda.11_1)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1, lambda.12_1)*V12'
                            
                            ,
                            '## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
'   )
      
      
      
      #Create list of Constrained thresholds        
      lav_thr <- matrix(NA, nitems, cat-1)
      for(i in 1:12){
        for(j in 1:cat-1){
          lav_thr[i,j] <- paste0('V', i, '|c(NA, NA)*t',j,"+c(",paste0(rep(paste0("V",i, 'thr',j),2), sep = "", collapse = ","), ")*t",j)  
        }
      }
      #        as.vector(t(lav_thr))
      
      #Create list of Unconstrained thresholds        
      lav_thr.rel <- matrix(NA, nitems, cat-1)
      for(i in 1:12){
        for(j in 1:cat-1){
          lav_thr.rel[i,j] <- paste0('V', i, '|c(NA, NA)*t',j,"+c(","V",i, 'thr',j,".group1"," ,V",i, 'thr',j, ".group2",  ")*t",j)
        }
      }
      #        as.vector(t(lav_thr.rel))
      
      
      #Create list of Unconstrained loadings F1
      lav_load <- list()
      for(i in seq(1, nitems,1)){
        lav_load[i] <-   paste0('F1 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_1',".group1"," ,lambda.",i, '_1', ".group2",  ")*V",i)
      }
      
      lav_load[1] <- "F1 =~ c(1,1)*V1"
      #for(i in seq(2, 12,2)){
      #  lav_load[i] <-   paste0('F2 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_2',".group1"," ,lambda.",i, '_2', ".group2",  ")*V",i)
      #}
      #lav_load[2] <- "F2 =~ c(1,1)*V2"
      
      #Create list of Constrained loadings F1
      lav_load_Constr <- list()
      for(i in seq(1, nitems,1)){
        lav_load_Constr[i] <-   paste0('F1 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_1',".group1"," ,lambda.",i, '_1', ".group1",  ")*V",i)
      }
      #        for(i in seq(2, 12,2)){
      #          lav_load_Constr[i] <-   paste0('F2 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_2',".group1"," ,lambda.",i, '_2', ".group1",  ")*V",i)
      #        }
      
      lav_load_Constr[1] <- "F1 =~ c(1,1)*V1"
      #        lav_load_Constr[2] <- "F2 =~ c(1,1)*V2"
      
      #Create list of items to replace with unconstrained
      thrselectlist <- list()
      thrselectlist[[1]] <- c(1:4)
      thrselectlist[[2]] <- c(5:8)
      thrselectlist[[3]] <- c(9:12)
      thrselectlist[[4]] <- c(13:16)
      thrselectlist[[5]] <- c(17:20)
      thrselectlist[[6]] <- c(21:24)
      thrselectlist[[7]] <- c(25:28)
      thrselectlist[[8]] <- c(29:32)
      thrselectlist[[9]] <- c(33:36)
      thrselectlist[[10]] <- c(37:40)
      thrselectlist[[11]] <- c(41:44)
      thrselectlist[[12]] <- c(45:48)
      
      Thr.rel <- list()
      
      #Make lists were items constraints are replaced one by one
      for(i in 1:12){
        Thr.rel[[i]] <- as.vector(t(lav_thr))
        Thr.rel[[i]][thrselectlist[[i]]] <- as.vector(t(lav_thr.rel))[thrselectlist[[i]]]
      }
      
      Load.rel <- list()
      for(i in 2:nitems){
        Load.rel[[i]] <- lav_load_Constr
        Load.rel[[i]][[i]] <- lav_load[[i]]
      }
      Load.rel[[1]] <- lav_load_Constr
      
      #Now Create lists for loadings and thresholds item models
      Mod_list_test.thr <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.thr[[i]] <- c(FullyConstrained[[1]],FullyConstrained[[2]], Thr.rel[[i-1]])
      }
      
      Mod_list_test.load <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.load [[i]] <- c(FullyConstrained[[1]],FullyConstrained[[3]], Load.rel[[i-1]])
      }
      
      Mod_list_test.load[[1]] <- Mod_list_test.thr[[1]] <- FullyConstrained
      
      
      FullyConstrainedARS <- c('
ARS =~ c(1, 1)*V1 + c(lambda.1_2, lambda.1_2)*V1
ARS =~ c(1, 1)*V2 + c(lambda.2_2, lambda.2_2)*V2
ARS =~ c(1, 1)*V3 + c(lambda.3_2, lambda.3_2)*V3
ARS =~ c(1, 1)*V4 + c(lambda.4_2, lambda.4_2)*V4
ARS =~ c(1, 1)*V5 + c(lambda.5_2, lambda.5_2)*V5
ARS =~ c(1, 1)*V6 + c(lambda.6_2, lambda.6_2)*V6
ARS =~ c(1, 1)*V7 + c(lambda.7_2, lambda.7_2)*V7
ARS =~ c(1, 1)*V8 + c(lambda.8_2, lambda.8_2)*V8
ARS =~ c(1, 1)*V9 + c(lambda.9_2, lambda.9_2)*V9
ARS =~ c(1, 1)*V10 + c(lambda.10_2, lambda.10_2)*V10
ARS =~ c(1, 1)*V11 + c(lambda.11_2, lambda.11_2)*V11
ARS =~ c(1, 1)*V12 + c(lambda.12_2, lambda.12_2)*V12
## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V2 ~*~ c(1, NA)*V2
V3 ~*~ c(1, NA)*V3
V4 ~*~ c(1, NA)*V4
V5 ~*~ c(1, NA)*V5
V6 ~*~ c(1, NA)*V6
V7 ~*~ c(1, NA)*V7
V8 ~*~ c(1, NA)*V8
V9 ~*~ c(1, NA)*V9
V10 ~*~ c(1, NA)*V10
V11 ~*~ c(1, NA)*V11
V12 ~*~ c(1, NA)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(0, 0)*ARS + c(psi.2_1.g1, psi.2_1.g2)*ARS
', 
                               '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1, lambda.2_1)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1, lambda.3_1)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1, lambda.4_1)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1, lambda.5_1)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1, lambda.6_1)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1, lambda.7_1)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1, lambda.8_1)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1, lambda.9_1)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1, lambda.10_1)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1, lambda.11_1)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1, lambda.12_1)*V12

','
## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
')
      #Now Create lists for loadings and thresholds item models
      Mod_list_test.thr.ARS <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.thr.ARS[[i]] <- c(FullyConstrainedARS[[1]], FullyConstrainedARS[[2]], Thr.rel[[i-1]])
      }
      
      Mod_list_test.load.ARS <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.load.ARS [[i]] <- c(FullyConstrainedARS[[1]],FullyConstrainedARS[[3]], Load.rel[[i-1]])
      }
      
      Mod_list_test.load.ARS[[1]] <- Mod_list_test.thr.ARS[[1]] <- FullyConstrainedARS
      
    } 
    else {
      
      FullyConstrained <- c( 
        '
## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1
V13 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, NA)*1
V14 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, NA)*1
V15 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, NA)*1
V16 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, NA)*1
V17 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, NA)*1
V18 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, NA)*1
V19 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, NA)*1
V20 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, NA)*1
V21 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, NA)*1
V22 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, NA)*1
V23 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, NA)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V2 ~*~ c(1, NA)*V2
V3 ~*~ c(1, NA)*V3
V4 ~*~ c(1, NA)*V4
V5 ~*~ c(1, NA)*V5
V6 ~*~ c(1, NA)*V6
V7 ~*~ c(1, NA)*V7
V8 ~*~ c(1, NA)*V8
V9 ~*~ c(1, NA)*V9
V10 ~*~ c(1, NA)*V10
V11 ~*~ c(1, NA)*V11
V12 ~*~ c(1, NA)*V12
V13 ~*~ c(1, NA)*V13
V14 ~*~ c(1, NA)*V14
V15 ~*~ c(1, NA)*V15
V16 ~*~ c(1, NA)*V16
V17 ~*~ c(1, NA)*V17
V18 ~*~ c(1, NA)*V18
V19 ~*~ c(1, NA)*V19
V20 ~*~ c(1, NA)*V20
V21 ~*~ c(1, NA)*V21
V22 ~*~ c(1, NA)*V22
V23 ~*~ c(1, NA)*V23
V24 ~*~ c(1, NA)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:
          ',
        '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1, lambda.2_1)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1, lambda.3_1)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1, lambda.4_1)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1, lambda.5_1)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1, lambda.6_1)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1, lambda.7_1)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1, lambda.8_1)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1, lambda.9_1)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1, lambda.10_1)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1, lambda.11_1)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1, lambda.12_1)*V12
F1 =~ c(NA, NA)*V13 + c(lambda.13_1, lambda.13_1)*V13
F1 =~ c(NA, NA)*V14 + c(lambda.14_1, lambda.14_1)*V14
F1 =~ c(NA, NA)*V15 + c(lambda.15_1, lambda.15_1)*V15
F1 =~ c(NA, NA)*V16 + c(lambda.16_1, lambda.16_1)*V16
F1 =~ c(NA, NA)*V17 + c(lambda.17_1, lambda.17_1)*V17
F1 =~ c(NA, NA)*V18 + c(lambda.18_1, lambda.18_1)*V18
F1 =~ c(NA, NA)*V19 + c(lambda.19_1, lambda.19_1)*V19
F1 =~ c(NA, NA)*V20 + c(lambda.20_1, lambda.20_1)*V20
F1 =~ c(NA, NA)*V21 + c(lambda.21_1, lambda.21_1)*V21
F1 =~ c(NA, NA)*V22 + c(lambda.22_1, lambda.22_1)*V22
F1 =~ c(NA, NA)*V23 + c(lambda.23_1, lambda.23_1)*V23
F1 =~ c(NA, NA)*V24 + c(lambda.24_1, lambda.24_1)*V24
','
## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1, V13.thr1)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2, V13.thr2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3, V13.thr3)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4, V13.thr4)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1, V14.thr1)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2, V14.thr2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3, V14.thr3)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4, V14.thr4)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1, V15.thr1)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2, V15.thr2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3, V15.thr3)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4, V15.thr4)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1, V16.thr1)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2, V16.thr2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3, V16.thr3)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4, V16.thr4)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1, V17.thr1)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2, V17.thr2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3, V17.thr3)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4, V17.thr4)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1, V18.thr1)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2, V18.thr2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3, V18.thr3)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4, V18.thr4)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1, V19.thr1)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2, V19.thr2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3, V19.thr3)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4, V19.thr4)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1, V20.thr1)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2, V20.thr2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3, V20.thr3)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4, V20.thr4)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1, V21.thr1)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2, V21.thr2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3, V21.thr3)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4, V21.thr4)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1, V22.thr1)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2, V22.thr2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3, V22.thr3)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4, V22.thr4)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1, V23.thr1)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2, V23.thr2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3, V23.thr3)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4, V23.thr4)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1, V24.thr1)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2, V24.thr2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3, V24.thr3)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4, V24.thr4)*t4
')
      
      #Create list of Constrained thresholds        
      lav_thr <- matrix(NA, nitems, cat-1)
      for(i in 1:nitems){
        for(j in 1:cat-1){
          lav_thr[i,j] <- paste0('V', i, '|c(NA, NA)*t',j,"+c(",paste0(rep(paste0("V",i, 'thr',j),2), sep = "", collapse = ","), ")*t",j)  
        }
      }
      #        as.vector(t(lav_thr))
      
      #Create list of Unconstrained thresholds        
      lav_thr.rel <- matrix(NA, nitems, cat-1)
      for(i in 1:nitems){
        for(j in 1:cat-1){
          lav_thr.rel[i,j] <- paste0('V', i, '|c(NA, NA)*t',j,"+c(","V",i, 'thr',j,".group1"," ,V",i, 'thr',j, ".group2",  ")*t",j)
        }
      }
      #        as.vector(t(lav_thr.rel))
      
      
      #Create list of Unconstrained loadings F1
      lav_load <- list()
      for(i in seq(1, nitems,1)){
        lav_load[i] <-   paste0('F1 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_1',".group1"," ,lambda.",i, '_1', ".group2",  ")*V",i)
      }
      
      lav_load[1] <- "F1 =~ c(1,1)*V1"
      #for(i in seq(2, 12,2)){
      #  lav_load[i] <-   paste0('F2 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_2',".group1"," ,lambda.",i, '_2', ".group2",  ")*V",i)
      #}
      #lav_load[2] <- "F2 =~ c(1,1)*V2"
      
      #Create list of Constrained loadings F1
      lav_load_Constr <- list()
      for(i in seq(1, nitems,1)){
        lav_load_Constr[i] <-   paste0('F1 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_1',".group1"," ,lambda.",i, '_1', ".group1",  ")*V",i)
      }
      #        for(i in seq(2, 12,2)){
      #          lav_load_Constr[i] <-   paste0('F2 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_2',".group1"," ,lambda.",i, '_2', ".group1",  ")*V",i)
      #        }
      
      lav_load_Constr[1] <- "F1 =~ c(1,1)*V1"
      #        lav_load_Constr[2] <- "F2 =~ c(1,1)*V2"
      
      #Create list of items to replace with unconstrained
      thrselectlist <- list()
      thrselectlist[[1]] <- c(1:4)
      thrselectlist[[2]] <- c(5:8)
      thrselectlist[[3]] <- c(9:12)
      thrselectlist[[4]] <- c(13:16)
      thrselectlist[[5]] <- c(17:20)
      thrselectlist[[6]] <- c(21:24)
      thrselectlist[[7]] <- c(25:28)
      thrselectlist[[8]] <- c(29:32)
      thrselectlist[[9]] <- c(33:36)
      thrselectlist[[10]] <- c(37:40)
      thrselectlist[[11]] <- c(41:44)
      thrselectlist[[12]] <- c(45:48)
      thrselectlist[[13]] <- c(49:52)
      thrselectlist[[14]] <- c(53:56)
      thrselectlist[[15]] <- c(57:60)
      thrselectlist[[16]] <- c(61:64)
      thrselectlist[[17]] <- c(65:68)
      thrselectlist[[18]] <- c(69:72)
      thrselectlist[[19]] <- c(73:76)
      thrselectlist[[20]] <- c(77:80)
      thrselectlist[[21]] <- c(81:84)
      thrselectlist[[22]] <- c(85:88)
      thrselectlist[[23]] <- c(89:92)
      thrselectlist[[24]] <- c(93:96)
      
      Thr.rel <- list()
      
      #Make lists were items constraints are replaced one by one
      for(i in 1:nitems){
        Thr.rel[[i]] <- as.vector(t(lav_thr))
        Thr.rel[[i]][thrselectlist[[i]]] <- as.vector(t(lav_thr.rel))[thrselectlist[[i]]]
      }
      
      Load.rel <- list()
      for(i in 2:nitems){
        Load.rel[[i]] <- lav_load_Constr
        Load.rel[[i]][[i]] <- lav_load[[i]]
      }
      Load.rel[[1]] <- lav_load_Constr
      
      #Now Create lists for loadings and thresholds item models
      Mod_list_test.thr <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.thr[[i]] <- c(FullyConstrained[[1]],FullyConstrained[[2]], Thr.rel[[i-1]])
      }
      
      Mod_list_test.load <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.load [[i]] <- c(FullyConstrained[[1]],FullyConstrained[[3]], Load.rel[[i-1]])
      }
      
      Mod_list_test.load[[1]] <- Mod_list_test.thr[[1]] <- FullyConstrained
      
      
      
      
      
      FullyConstrainedARS <- c(
        '
ARS =~ c(1, 1)*V1 + c(lambda.1_2, lambda.1_2)*V1
ARS =~ c(1, 1)*V2 + c(lambda.2_2, lambda.2_2)*V2
ARS =~ c(1, 1)*V3 + c(lambda.3_2, lambda.3_2)*V3
ARS =~ c(1, 1)*V4 + c(lambda.4_2, lambda.4_2)*V4
ARS =~ c(1, 1)*V5 + c(lambda.5_2, lambda.5_2)*V5
ARS =~ c(1, 1)*V6 + c(lambda.6_2, lambda.6_2)*V6
ARS =~ c(1, 1)*V7 + c(lambda.7_2, lambda.7_2)*V7
ARS =~ c(1, 1)*V8 + c(lambda.8_2, lambda.8_2)*V8
ARS =~ c(1, 1)*V9 + c(lambda.9_2, lambda.9_2)*V9
ARS =~ c(1, 1)*V10 + c(lambda.10_2, lambda.10_2)*V10
ARS =~ c(1, 1)*V11 + c(lambda.11_2, lambda.11_2)*V11
ARS =~ c(1, 1)*V12 + c(lambda.12_2, lambda.12_2)*V12
ARS =~ c(1, 1)*V13 + c(lambda.13_2, lambda.13_2)*V13
ARS =~ c(1, 1)*V14 + c(lambda.14_2, lambda.14_2)*V14
ARS =~ c(1, 1)*V15 + c(lambda.15_2, lambda.15_2)*V15
ARS =~ c(1, 1)*V16 + c(lambda.16_2, lambda.16_2)*V16
ARS =~ c(1, 1)*V17 + c(lambda.17_2, lambda.17_2)*V17
ARS =~ c(1, 1)*V18 + c(lambda.18_2, lambda.18_2)*V18
ARS =~ c(1, 1)*V19 + c(lambda.19_2, lambda.19_2)*V19
ARS =~ c(1, 1)*V20 + c(lambda.20_2, lambda.20_2)*V20
ARS =~ c(1, 1)*V21 + c(lambda.21_2, lambda.21_2)*V21
ARS =~ c(1, 1)*V22 + c(lambda.22_2, lambda.22_2)*V22
ARS =~ c(1, 1)*V23 + c(lambda.23_2, lambda.23_2)*V23
ARS =~ c(1, 1)*V24 + c(lambda.24_2, lambda.24_2)*V24

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V2 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V3 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V4 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V5 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V6 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V7 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V8 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V9 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V10 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V11 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1
V13 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, NA)*1
V14 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, NA)*1
V15 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, NA)*1
V16 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, NA)*1
V17 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, NA)*1
V18 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, NA)*1
V19 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, NA)*1
V20 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, NA)*1
V21 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, NA)*1
V22 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, NA)*1
V23 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, NA)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V2 ~*~ c(1, NA)*V2
V3 ~*~ c(1, NA)*V3
V4 ~*~ c(1, NA)*V4
V5 ~*~ c(1, NA)*V5
V6 ~*~ c(1, NA)*V6
V7 ~*~ c(1, NA)*V7
V8 ~*~ c(1, NA)*V8
V9 ~*~ c(1, NA)*V9
V10 ~*~ c(1, NA)*V10
V11 ~*~ c(1, NA)*V11
V12 ~*~ c(1, NA)*V12
V13 ~*~ c(1, NA)*V13
V14 ~*~ c(1, NA)*V14
V15 ~*~ c(1, NA)*V15
V16 ~*~ c(1, NA)*V16
V17 ~*~ c(1, NA)*V17
V18 ~*~ c(1, NA)*V18
V19 ~*~ c(1, NA)*V19
V20 ~*~ c(1, NA)*V20
V21 ~*~ c(1, NA)*V21
V22 ~*~ c(1, NA)*V22
V23 ~*~ c(1, NA)*V23
V24 ~*~ c(1, NA)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(0, 0)*ARS + c(psi.2_1.g1, psi.2_1.g2)*ARS
',
        '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V2 + c(lambda.2_1, lambda.2_1)*V2
F1 =~ c(NA, NA)*V3 + c(lambda.3_1, lambda.3_1)*V3
F1 =~ c(NA, NA)*V4 + c(lambda.4_1, lambda.4_1)*V4
F1 =~ c(NA, NA)*V5 + c(lambda.5_1, lambda.5_1)*V5
F1 =~ c(NA, NA)*V6 + c(lambda.6_1, lambda.6_1)*V6
F1 =~ c(NA, NA)*V7 + c(lambda.7_1, lambda.7_1)*V7
F1 =~ c(NA, NA)*V8 + c(lambda.8_1, lambda.8_1)*V8
F1 =~ c(NA, NA)*V9 + c(lambda.9_1, lambda.9_1)*V9
F1 =~ c(NA, NA)*V10 + c(lambda.10_1, lambda.10_1)*V10
F1 =~ c(NA, NA)*V11 + c(lambda.11_1, lambda.11_1)*V11
F1 =~ c(NA, NA)*V12 + c(lambda.12_1, lambda.12_1)*V12
F1 =~ c(NA, NA)*V13 + c(lambda.13_1, lambda.13_1)*V13
F1 =~ c(NA, NA)*V14 + c(lambda.14_1, lambda.14_1)*V14
F1 =~ c(NA, NA)*V15 + c(lambda.15_1, lambda.15_1)*V15
F1 =~ c(NA, NA)*V16 + c(lambda.16_1, lambda.16_1)*V16
F1 =~ c(NA, NA)*V17 + c(lambda.17_1, lambda.17_1)*V17
F1 =~ c(NA, NA)*V18 + c(lambda.18_1, lambda.18_1)*V18
F1 =~ c(NA, NA)*V19 + c(lambda.19_1, lambda.19_1)*V19
F1 =~ c(NA, NA)*V20 + c(lambda.20_1, lambda.20_1)*V20
F1 =~ c(NA, NA)*V21 + c(lambda.21_1, lambda.21_1)*V21
F1 =~ c(NA, NA)*V22 + c(lambda.22_1, lambda.22_1)*V22
F1 =~ c(NA, NA)*V23 + c(lambda.23_1, lambda.23_1)*V23
F1 =~ c(NA, NA)*V24 + c(lambda.24_1, lambda.24_1)*V24
','
## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1, V13.thr1)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2, V13.thr2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3, V13.thr3)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4, V13.thr4)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1, V14.thr1)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2, V14.thr2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3, V14.thr3)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4, V14.thr4)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1, V15.thr1)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2, V15.thr2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3, V15.thr3)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4, V15.thr4)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1, V16.thr1)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2, V16.thr2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3, V16.thr3)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4, V16.thr4)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1, V17.thr1)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2, V17.thr2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3, V17.thr3)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4, V17.thr4)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1, V18.thr1)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2, V18.thr2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3, V18.thr3)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4, V18.thr4)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1, V19.thr1)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2, V19.thr2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3, V19.thr3)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4, V19.thr4)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1, V20.thr1)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2, V20.thr2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3, V20.thr3)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4, V20.thr4)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1, V21.thr1)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2, V21.thr2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3, V21.thr3)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4, V21.thr4)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1, V22.thr1)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2, V22.thr2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3, V22.thr3)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4, V22.thr4)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1, V23.thr1)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2, V23.thr2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3, V23.thr3)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4, V23.thr4)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1, V24.thr1)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2, V24.thr2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3, V24.thr3)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4, V24.thr4)*t4')
      
      #Now Create lists for loadings and thresholds item models
      Mod_list_test.thr.ARS <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.thr.ARS[[i]] <- c(FullyConstrainedARS[[1]], FullyConstrainedARS[[2]], Thr.rel[[i-1]])
      }
      
      Mod_list_test.load.ARS <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.load.ARS [[i]] <- c(FullyConstrainedARS[[1]],FullyConstrainedARS[[3]], Load.rel[[i-1]])
      }
      
      Mod_list_test.load.ARS[[1]] <- Mod_list_test.thr.ARS[[1]] <- FullyConstrainedARS
      
      
      
      
    }
    
  } else {
    if(nitems== 12){
      FullyConstrained <- c(
        '
## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V2 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V4 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V6 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V8 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V10 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V3 ~*~ c(1, NA)*V3
V5 ~*~ c(1, NA)*V5
V7 ~*~ c(1, NA)*V7
V9 ~*~ c(1, NA)*V9
V11 ~*~ c(1, NA)*V11
V2 ~*~ c(1, NA)*V2
V4 ~*~ c(1, NA)*V4
V6 ~*~ c(1, NA)*V6
V8 ~*~ c(1, NA)*V8
V10 ~*~ c(1, NA)*V10
V12 ~*~ c(1, NA)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:


## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
',' 
          ## LOADINGS:
          
          F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
          F1 =~ c(NA, NA)*V3 + c(lambda.2_1, lambda.2_1)*V3
          F1 =~ c(NA, NA)*V5 + c(lambda.3_1, lambda.3_1)*V5
          F1 =~ c(NA, NA)*V7 + c(lambda.4_1, lambda.4_1)*V7
          F1 =~ c(NA, NA)*V9 + c(lambda.5_1, lambda.5_1)*V9
          F1 =~ c(NA, NA)*V11 + c(lambda.6_1, lambda.6_1)*V11
          F2 =~ c(1, 1)*V2 + c(lambda.7_2, lambda.7_2)*V2
          F2 =~ c(NA, NA)*V4 + c(lambda.8_2, lambda.8_2)*V4
          F2 =~ c(NA, NA)*V6 + c(lambda.9_2, lambda.9_2)*V6
          F2 =~ c(NA, NA)*V8 + c(lambda.10_2, lambda.10_2)*V8
          F2 =~ c(NA, NA)*V10 + c(lambda.11_2, lambda.11_2)*V10
          F2 =~ c(NA, NA)*V12 + c(lambda.12_2, lambda.12_2)*V12
', '          
          ## THRESHOLDS:
          
          V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
          V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
          V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
          V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
          V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
          V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
          V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
          V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
          V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
          V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
          V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
          V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
          V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
          V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
          V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
          V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
          V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
          V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
          V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
          V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
          V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
          V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
          V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
          V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
          V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
          V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
          V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
          V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
          V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
          V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
          V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
          V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
          V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
          V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
          V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
          V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
          V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
          V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
          V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
          V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
          V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
          V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
          V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
          V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
          V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
          V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
          V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
          V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
' )
      
      #Create list of Constrained thresholds        
      lav_thr <- matrix(NA, nitems, cat-1)
      for(i in 1:nitems){
        for(j in 1:cat-1){
          lav_thr[i,j] <- paste0('V', i, '|c(NA, NA)*t',j,"+c(",paste0(rep(paste0("V",i, 'thr',j),2), sep = "", collapse = ","), ")*t",j)  
        }
      }
      as.vector(t(lav_thr))
      
      #Create list of Unconstrained thresholds        
      lav_thr.rel <- matrix(NA, nitems, cat-1)
      for(i in 1:nitems){
        for(j in 1:cat-1){
          lav_thr.rel[i,j] <- paste0('V', i, '|c(NA, NA)*t',j,"+c(","V",i, 'thr',j,".group1"," ,V",i, 'thr',j, ".group2",  ")*t",j)
        }
      }
      as.vector(t(lav_thr.rel))
      
      #F1 =~ c(NA, NA)*V3 + c(lambda.2_1, lambda.2_1)*V3
      
      #Create list of Unconstrained loadings F1
      lav_load <- list()
      for(i in seq(1, nitems,2)){
        lav_load[i] <-   paste0('F1 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_1',".group1"," ,lambda.",i, '_1', ".group2",  ")*V",i)
      }
      
      lav_load[1] <- "F1 =~ c(1,1)*V1"
      for(i in seq(2, nitems,2)){
        lav_load[i] <-   paste0('F2 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_2',".group1"," ,lambda.",i, '_2', ".group2",  ")*V",i)
      }
      lav_load[2] <- "F2 =~ c(1,1)*V2"
      
      #Create list of Constrained loadings F1
      lav_load_Constr <- list()
      for(i in seq(1, nitems,2)){
        lav_load_Constr[i] <-   paste0('F1 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_1',".group1"," ,lambda.",i, '_1', ".group1",  ")*V",i)
      }
      for(i in seq(2, nitems,2)){
        lav_load_Constr[i] <-   paste0('F2 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_2',".group1"," ,lambda.",i, '_2', ".group1",  ")*V",i)
      }
      
      lav_load_Constr[1] <- "F1 =~ c(1,1)*V1"
      lav_load_Constr[2] <- "F2 =~ c(1,1)*V2"
      
      #Create list of items to replace with unconstrained
      thrselectlist <- list()
      thrselectlist[[1]] <- c(1:4)
      thrselectlist[[2]] <- c(5:8)
      thrselectlist[[3]] <- c(9:12)
      thrselectlist[[4]] <- c(13:16)
      thrselectlist[[5]] <- c(17:20)
      thrselectlist[[6]] <- c(21:24)
      thrselectlist[[7]] <- c(25:28)
      thrselectlist[[8]] <- c(29:32)
      thrselectlist[[9]] <- c(33:36)
      thrselectlist[[10]] <- c(37:40)
      thrselectlist[[11]] <- c(41:44)
      thrselectlist[[12]] <- c(45:48)
      
      Thr.rel <- list()
      
      #Make lists were items constraints are replaced one by one
      for(i in 1:nitems){
        Thr.rel[[i]] <- as.vector(t(lav_thr))
        Thr.rel[[i]][thrselectlist[[i]]] <- as.vector(t(lav_thr.rel))[thrselectlist[[i]]]
      }
      
      Load.rel <- list()
      for(i in 3:12){
        Load.rel[[i]] <- lav_load_Constr
        Load.rel[[i]][[i]] <- lav_load[[i]]
      }
      Load.rel[[1]] <- Load.rel[[2]] <- lav_load_Constr
      
      #Now Create lists for loadings and thresholds item models
      Mod_list_test.thr <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.thr[[i]] <- c(FullyConstrained[[1]],FullyConstrained[[2]], Thr.rel[[i-1]])
      }
      
      Mod_list_test.load <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.load [[i]] <- c(FullyConstrained[[1]],FullyConstrained[[3]], Load.rel[[i-1]])
      }
      
      Mod_list_test.load[[1]] <- Mod_list_test.thr[[1]] <- FullyConstrained
      
      
      
      
      FullyConstrainedARS <- c(
        
        '
ARS =~ c(1, 1)*V1 + c(lambda.1_3, lambda.1_3)*V1
ARS =~ c(1, 1)*V3 + c(lambda.2_3, lambda.2_3)*V3
ARS =~ c(1, 1)*V5 + c(lambda.3_3, lambda.3_3)*V5
ARS =~ c(1, 1)*V7 + c(lambda.4_3, lambda.4_3)*V7
ARS =~ c(1, 1)*V9 + c(lambda.5_3, lambda.5_3)*V9
ARS =~ c(1, 1)*V11 + c(lambda.6_3, lambda.6_3)*V11
ARS =~ c(1, 1)*V2 + c(lambda.7_3, lambda.7_3)*V2
ARS =~ c(1, 1)*V4 + c(lambda.8_3, lambda.8_3)*V4
ARS =~ c(1, 1)*V6 + c(lambda.9_3, lambda.9_3)*V6
ARS =~ c(1, 1)*V8 + c(lambda.10_3, lambda.10_3)*V8
ARS =~ c(1, 1)*V10 + c(lambda.11_3, lambda.11_3)*V10
ARS =~ c(1, 1)*V12 + c(lambda.12_3, lambda.12_3)*V12

## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V2 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V4 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V6 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V8 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V10 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V12 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V3 ~*~ c(1, NA)*V3
V5 ~*~ c(1, NA)*V5
V7 ~*~ c(1, NA)*V7
V9 ~*~ c(1, NA)*V9
V11 ~*~ c(1, NA)*V11
V2 ~*~ c(1, NA)*V2
V4 ~*~ c(1, NA)*V4
V6 ~*~ c(1, NA)*V6
V8 ~*~ c(1, NA)*V8
V10 ~*~ c(1, NA)*V10
V12 ~*~ c(1, NA)*V12


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.3.g1, alpha.3.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
F1 ~~ c(0, 0)*ARS + c(psi.3_1.g1, psi.3_1.g2)*ARS
F2 ~~ c(0, 0)*ARS + c(psi.3_2.g1, psi.3_2.g2)*ARS',
        
        '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1, lambda.2_1)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1, lambda.3_1)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1, lambda.4_1)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1, lambda.5_1)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1, lambda.6_1)*V11
F2 =~ c(1, 1)*V2 + c(lambda.7_2, lambda.7_2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.8_2, lambda.8_2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.9_2, lambda.9_2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.10_2, lambda.10_2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.11_2, lambda.11_2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.12_2, lambda.12_2)*V12
','
## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
')
      #Now Create lists for loadings and thresholds item models
      Mod_list_test.thr.ARS <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.thr.ARS[[i]] <- c(FullyConstrainedARS[[1]], FullyConstrainedARS[[2]], Thr.rel[[i-1]])
      }
      
      Mod_list_test.load.ARS <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.load.ARS [[i]] <- c(FullyConstrainedARS[[1]],FullyConstrainedARS[[3]], Load.rel[[i-1]])
      }
      
      Mod_list_test.load.ARS[[1]] <- Mod_list_test.thr.ARS[[1]] <- FullyConstrainedARS
      
      
      
      
    } 
    else {
      
      FullyConstrained <- c(
        '
## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V13 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V15 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V17 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V19 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V21 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V23 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1
V2 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, NA)*1
V4 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, NA)*1
V6 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, NA)*1
V8 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, NA)*1
V10 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, NA)*1
V12 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, NA)*1
V14 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, NA)*1
V16 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, NA)*1
V18 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, NA)*1
V20 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, NA)*1
V22 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, NA)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V3 ~*~ c(1, NA)*V3
V5 ~*~ c(1, NA)*V5
V7 ~*~ c(1, NA)*V7
V9 ~*~ c(1, NA)*V9
V11 ~*~ c(1, NA)*V11
V13 ~*~ c(1, NA)*V13
V15 ~*~ c(1, NA)*V15
V17 ~*~ c(1, NA)*V17
V19 ~*~ c(1, NA)*V19
V21 ~*~ c(1, NA)*V21
V23 ~*~ c(1, NA)*V23
V2 ~*~ c(1, NA)*V2
V4 ~*~ c(1, NA)*V4
V6 ~*~ c(1, NA)*V6
V8 ~*~ c(1, NA)*V8
V10 ~*~ c(1, NA)*V10
V12 ~*~ c(1, NA)*V12
V14 ~*~ c(1, NA)*V14
V16 ~*~ c(1, NA)*V16
V18 ~*~ c(1, NA)*V18
V20 ~*~ c(1, NA)*V20
V22 ~*~ c(1, NA)*V22
V24 ~*~ c(1, NA)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:


## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
',
        '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1, lambda.2_1)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1, lambda.3_1)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1, lambda.4_1)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1, lambda.5_1)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1, lambda.6_1)*V11
F1 =~ c(NA, NA)*V13 + c(lambda.7_1, lambda.7_1)*V13
F1 =~ c(NA, NA)*V15 + c(lambda.8_1, lambda.8_1)*V15
F1 =~ c(NA, NA)*V17 + c(lambda.9_1, lambda.9_1)*V17
F1 =~ c(NA, NA)*V19 + c(lambda.10_1, lambda.10_1)*V19
F1 =~ c(NA, NA)*V21 + c(lambda.11_1, lambda.11_1)*V21
F1 =~ c(NA, NA)*V23 + c(lambda.12_1, lambda.12_1)*V23
F2 =~ c(1, 1)*V2 + c(lambda.13_2, lambda.13_2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.14_2, lambda.14_2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.15_2, lambda.15_2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.16_2, lambda.16_2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.17_2, lambda.17_2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.18_2, lambda.18_2)*V12
F2 =~ c(NA, NA)*V14 + c(lambda.19_2, lambda.19_2)*V14
F2 =~ c(NA, NA)*V16 + c(lambda.20_2, lambda.20_2)*V16
F2 =~ c(NA, NA)*V18 + c(lambda.21_2, lambda.21_2)*V18
F2 =~ c(NA, NA)*V20 + c(lambda.22_2, lambda.22_2)*V20
F2 =~ c(NA, NA)*V22 + c(lambda.23_2, lambda.23_2)*V22
F2 =~ c(NA, NA)*V24 + c(lambda.24_2, lambda.24_2)*V24

','
## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1, V13.thr1)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2, V13.thr2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3, V13.thr3)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4, V13.thr4)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1, V15.thr1)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2, V15.thr2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3, V15.thr3)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4, V15.thr4)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1, V17.thr1)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2, V17.thr2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3, V17.thr3)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4, V17.thr4)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1, V19.thr1)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2, V19.thr2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3, V19.thr3)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4, V19.thr4)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1, V21.thr1)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2, V21.thr2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3, V21.thr3)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4, V21.thr4)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1, V23.thr1)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2, V23.thr2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3, V23.thr3)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4, V23.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1, V14.thr1)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2, V14.thr2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3, V14.thr3)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4, V14.thr4)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1, V16.thr1)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2, V16.thr2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3, V16.thr3)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4, V16.thr4)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1, V18.thr1)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2, V18.thr2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3, V18.thr3)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4, V18.thr4)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1, V20.thr1)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2, V20.thr2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3, V20.thr3)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4, V20.thr4)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1, V22.thr1)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2, V22.thr2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3, V22.thr3)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4, V22.thr4)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1, V24.thr1)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2, V24.thr2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3, V24.thr3)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4, V24.thr4)*t4
')
      
      #Create list of Constrained thresholds        
      lav_thr <- matrix(NA, nitems, cat-1)
      for(i in 1:nitems){
        for(j in 1:cat-1){
          lav_thr[i,j] <- paste0('V', i, '|c(NA, NA)*t',j,"+c(",paste0(rep(paste0("V",i, 'thr',j),2), sep = "", collapse = ","), ")*t",j)  
        }
      }
      #        as.vector(t(lav_thr))
      
      #Create list of Unconstrained thresholds        
      lav_thr.rel <- matrix(NA, nitems, cat-1)
      for(i in 1:nitems){
        for(j in 1:cat-1){
          lav_thr.rel[i,j] <- paste0('V', i, '|c(NA, NA)*t',j,"+c(","V",i, 'thr',j,".group1"," ,V",i, 'thr',j, ".group2",  ")*t",j)
        }
      }
      #        as.vector(t(lav_thr.rel))
      
      
      #Create list of Unconstrained loadings F1
      lav_load <- list()
      for(i in seq(1, nitems,2)){
        lav_load[i] <-   paste0('F1 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_1',".group1"," ,lambda.",i, '_1', ".group2",  ")*V",i)
      }
      
      lav_load[1] <- "F1 =~ c(1,1)*V1"
      for(i in seq(2, 24,2)){
        lav_load[i] <-   paste0('F2 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_2',".group1"," ,lambda.",i, '_2', ".group2",  ")*V",i)
      }
      lav_load[2] <- "F2 =~ c(1,1)*V2"
      
      #Create list of Constrained loadings F1
      lav_load_Constr <- list()
      for(i in seq(1, nitems,2)){
        lav_load_Constr[i] <-   paste0('F1 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_1',".group1"," ,lambda.",i, '_1', ".group1",  ")*V",i)
      }
      for(i in seq(2, nitems,2)){
        lav_load_Constr[i] <-   paste0('F2 =~', 'c(NA, NA)*V',i,"+c(","lambda.",i, '_2',".group1"," ,lambda.",i, '_2', ".group1",  ")*V",i)
      }
      
      lav_load_Constr[1] <- "F1 =~ c(1,1)*V1"
      lav_load_Constr[2] <- "F2 =~ c(1,1)*V2"
      
      #Create list of items to replace with unconstrained
      thrselectlist <- list()
      thrselectlist[[1]] <- c(1:4)
      thrselectlist[[2]] <- c(5:8)
      thrselectlist[[3]] <- c(9:12)
      thrselectlist[[4]] <- c(13:16)
      thrselectlist[[5]] <- c(17:20)
      thrselectlist[[6]] <- c(21:24)
      thrselectlist[[7]] <- c(25:28)
      thrselectlist[[8]] <- c(29:32)
      thrselectlist[[9]] <- c(33:36)
      thrselectlist[[10]] <- c(37:40)
      thrselectlist[[11]] <- c(41:44)
      thrselectlist[[12]] <- c(45:48)
      thrselectlist[[13]] <- c(49:52)
      thrselectlist[[14]] <- c(53:56)
      thrselectlist[[15]] <- c(57:60)
      thrselectlist[[16]] <- c(61:64)
      thrselectlist[[17]] <- c(65:68)
      thrselectlist[[18]] <- c(69:72)
      thrselectlist[[19]] <- c(73:76)
      thrselectlist[[20]] <- c(77:80)
      thrselectlist[[21]] <- c(81:84)
      thrselectlist[[22]] <- c(85:88)
      thrselectlist[[23]] <- c(89:92)
      thrselectlist[[24]] <- c(93:96)
      
      Thr.rel <- list()
      
      #Make lists were items constraints are replaced one by one
      for(i in 1:nitems){
        Thr.rel[[i]] <- as.vector(t(lav_thr))
        Thr.rel[[i]][thrselectlist[[i]]] <- as.vector(t(lav_thr.rel))[thrselectlist[[i]]]
      }
      
      Load.rel <- list()
      for(i in 2:nitems){
        Load.rel[[i]] <- lav_load_Constr
        Load.rel[[i]][[i]] <- lav_load[[i]]
      }
      Load.rel[[1]] <- lav_load_Constr
      
      #Now Create lists for loadings and thresholds item models
      Mod_list_test.thr <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.thr[[i]] <- c(FullyConstrained[[1]],FullyConstrained[[2]], Thr.rel[[i-1]])
      }
      
      Mod_list_test.load <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.load [[i]] <- c(FullyConstrained[[1]],FullyConstrained[[3]], Load.rel[[i-1]])
      }
      
      Mod_list_test.load[[1]] <- Mod_list_test.thr[[1]] <- FullyConstrained
      
      
      
      
      
      
      
      FullyConstrainedARS <- c(
        '
ARS =~ c(1, 1)*V1 + c(lambda.1_3, lambda.1_3)*V1
ARS =~ c(1, 1)*V3 + c(lambda.2_3, lambda.2_3)*V3
ARS =~ c(1, 1)*V5 + c(lambda.3_3, lambda.3_3)*V5
ARS =~ c(1, 1)*V7 + c(lambda.4_3, lambda.4_3)*V7
ARS =~ c(1, 1)*V9 + c(lambda.5_3, lambda.5_3)*V9
ARS =~ c(1, 1)*V11 + c(lambda.6_3, lambda.6_3)*V11
ARS =~ c(1, 1)*V13 + c(lambda.7_3, lambda.7_3)*V13
ARS =~ c(1, 1)*V15 + c(lambda.8_3, lambda.8_3)*V15
ARS =~ c(1, 1)*V17 + c(lambda.9_3, lambda.9_3)*V17
ARS =~ c(1, 1)*V19 + c(lambda.10_3, lambda.10_3)*V19
ARS =~ c(1, 1)*V21 + c(lambda.11_3, lambda.11_3)*V21
ARS =~ c(1, 1)*V23 + c(lambda.12_3, lambda.12_3)*V23
ARS =~ c(1, 1)*V2 + c(lambda.13_3, lambda.13_3)*V2
ARS =~ c(1, 1)*V4 + c(lambda.14_3, lambda.14_3)*V4
ARS =~ c(1, 1)*V6 + c(lambda.15_3, lambda.15_3)*V6
ARS =~ c(1, 1)*V8 + c(lambda.16_3, lambda.16_3)*V8
ARS =~ c(1, 1)*V10 + c(lambda.17_3, lambda.17_3)*V10
ARS =~ c(1, 1)*V12 + c(lambda.18_3, lambda.18_3)*V12
ARS =~ c(1, 1)*V14 + c(lambda.19_3, lambda.19_3)*V14
ARS =~ c(1, 1)*V16 + c(lambda.20_3, lambda.20_3)*V16
ARS =~ c(1, 1)*V18 + c(lambda.21_3, lambda.21_3)*V18
ARS =~ c(1, 1)*V20 + c(lambda.22_3, lambda.22_3)*V20
ARS =~ c(1, 1)*V22 + c(lambda.23_3, lambda.23_3)*V22
ARS =~ c(1, 1)*V24 + c(lambda.24_3, lambda.24_3)*V24
          
        
## INTERCEPTS:

V1 ~ c(nu.1.g1, nu.1.g2)*1 + c(0, NA)*1
V3 ~ c(nu.2.g1, nu.2.g2)*1 + c(0, NA)*1
V5 ~ c(nu.3.g1, nu.3.g2)*1 + c(0, NA)*1
V7 ~ c(nu.4.g1, nu.4.g2)*1 + c(0, NA)*1
V9 ~ c(nu.5.g1, nu.5.g2)*1 + c(0, NA)*1
V11 ~ c(nu.6.g1, nu.6.g2)*1 + c(0, NA)*1
V13 ~ c(nu.7.g1, nu.7.g2)*1 + c(0, NA)*1
V15 ~ c(nu.8.g1, nu.8.g2)*1 + c(0, NA)*1
V17 ~ c(nu.9.g1, nu.9.g2)*1 + c(0, NA)*1
V19 ~ c(nu.10.g1, nu.10.g2)*1 + c(0, NA)*1
V21 ~ c(nu.11.g1, nu.11.g2)*1 + c(0, NA)*1
V23 ~ c(nu.12.g1, nu.12.g2)*1 + c(0, NA)*1
V2 ~ c(nu.13.g1, nu.13.g2)*1 + c(0, NA)*1
V4 ~ c(nu.14.g1, nu.14.g2)*1 + c(0, NA)*1
V6 ~ c(nu.15.g1, nu.15.g2)*1 + c(0, NA)*1
V8 ~ c(nu.16.g1, nu.16.g2)*1 + c(0, NA)*1
V10 ~ c(nu.17.g1, nu.17.g2)*1 + c(0, NA)*1
V12 ~ c(nu.18.g1, nu.18.g2)*1 + c(0, NA)*1
V14 ~ c(nu.19.g1, nu.19.g2)*1 + c(0, NA)*1
V16 ~ c(nu.20.g1, nu.20.g2)*1 + c(0, NA)*1
V18 ~ c(nu.21.g1, nu.21.g2)*1 + c(0, NA)*1
V20 ~ c(nu.22.g1, nu.22.g2)*1 + c(0, NA)*1
V22 ~ c(nu.23.g1, nu.23.g2)*1 + c(0, NA)*1
V24 ~ c(nu.24.g1, nu.24.g2)*1 + c(0, NA)*1

## SCALING FACTORS:

V1 ~*~ c(1, NA)*V1
V3 ~*~ c(1, NA)*V3
V5 ~*~ c(1, NA)*V5
V7 ~*~ c(1, NA)*V7
V9 ~*~ c(1, NA)*V9
V11 ~*~ c(1, NA)*V11
V13 ~*~ c(1, NA)*V13
V15 ~*~ c(1, NA)*V15
V17 ~*~ c(1, NA)*V17
V19 ~*~ c(1, NA)*V19
V21 ~*~ c(1, NA)*V21
V23 ~*~ c(1, NA)*V23
V2 ~*~ c(1, NA)*V2
V4 ~*~ c(1, NA)*V4
V6 ~*~ c(1, NA)*V6
V8 ~*~ c(1, NA)*V8
V10 ~*~ c(1, NA)*V10
V12 ~*~ c(1, NA)*V12
V14 ~*~ c(1, NA)*V14
V16 ~*~ c(1, NA)*V16
V18 ~*~ c(1, NA)*V18
V20 ~*~ c(1, NA)*V20
V22 ~*~ c(1, NA)*V22
V24 ~*~ c(1, NA)*V24


## LATENT MEANS/INTERCEPTS:

F1 ~ c(alpha.1.g1, alpha.1.g2)*1 + c(0, 0)*1
F2 ~ c(alpha.2.g1, alpha.2.g2)*1 + c(0, 0)*1
ARS ~ c(alpha.3.g1, alpha.3.g2)*1 + c(0, 0)*1

## COMMON-FACTOR VARIANCES:

## COMMON-FACTOR COVARIANCES:

F1 ~~ c(NA, NA)*F2 + c(psi.2_1.g1, psi.2_1.g2)*F2
F1 ~~ c(0, 0)*ARS + c(psi.3_1.g1, psi.3_1.g2)*ARS
F2 ~~ c(0, 0)*ARS + c(psi.3_2.g1, psi.3_2.g2)*ARS
',
        '## LOADINGS:

F1 =~ c(1, 1)*V1 + c(lambda.1_1, lambda.1_1)*V1
F1 =~ c(NA, NA)*V3 + c(lambda.2_1, lambda.2_1)*V3
F1 =~ c(NA, NA)*V5 + c(lambda.3_1, lambda.3_1)*V5
F1 =~ c(NA, NA)*V7 + c(lambda.4_1, lambda.4_1)*V7
F1 =~ c(NA, NA)*V9 + c(lambda.5_1, lambda.5_1)*V9
F1 =~ c(NA, NA)*V11 + c(lambda.6_1, lambda.6_1)*V11
F1 =~ c(NA, NA)*V13 + c(lambda.7_1, lambda.7_1)*V13
F1 =~ c(NA, NA)*V15 + c(lambda.8_1, lambda.8_1)*V15
F1 =~ c(NA, NA)*V17 + c(lambda.9_1, lambda.9_1)*V17
F1 =~ c(NA, NA)*V19 + c(lambda.10_1, lambda.10_1)*V19
F1 =~ c(NA, NA)*V21 + c(lambda.11_1, lambda.11_1)*V21
F1 =~ c(NA, NA)*V23 + c(lambda.12_1, lambda.12_1)*V23
F2 =~ c(1, 1)*V2 + c(lambda.13_2, lambda.13_2)*V2
F2 =~ c(NA, NA)*V4 + c(lambda.14_2, lambda.14_2)*V4
F2 =~ c(NA, NA)*V6 + c(lambda.15_2, lambda.15_2)*V6
F2 =~ c(NA, NA)*V8 + c(lambda.16_2, lambda.16_2)*V8
F2 =~ c(NA, NA)*V10 + c(lambda.17_2, lambda.17_2)*V10
F2 =~ c(NA, NA)*V12 + c(lambda.18_2, lambda.18_2)*V12
F2 =~ c(NA, NA)*V14 + c(lambda.19_2, lambda.19_2)*V14
F2 =~ c(NA, NA)*V16 + c(lambda.20_2, lambda.20_2)*V16
F2 =~ c(NA, NA)*V18 + c(lambda.21_2, lambda.21_2)*V18
F2 =~ c(NA, NA)*V20 + c(lambda.22_2, lambda.22_2)*V20
F2 =~ c(NA, NA)*V22 + c(lambda.23_2, lambda.23_2)*V22
F2 =~ c(NA, NA)*V24 + c(lambda.24_2, lambda.24_2)*V24
','
## THRESHOLDS:

V1 | c(NA, NA)*t1 + c(V1.thr1, V1.thr1)*t1
V1 | c(NA, NA)*t2 + c(V1.thr2, V1.thr2)*t2
V1 | c(NA, NA)*t3 + c(V1.thr3, V1.thr3)*t3
V1 | c(NA, NA)*t4 + c(V1.thr4, V1.thr4)*t4
V3 | c(NA, NA)*t1 + c(V3.thr1, V3.thr1)*t1
V3 | c(NA, NA)*t2 + c(V3.thr2, V3.thr2)*t2
V3 | c(NA, NA)*t3 + c(V3.thr3, V3.thr3)*t3
V3 | c(NA, NA)*t4 + c(V3.thr4, V3.thr4)*t4
V5 | c(NA, NA)*t1 + c(V5.thr1, V5.thr1)*t1
V5 | c(NA, NA)*t2 + c(V5.thr2, V5.thr2)*t2
V5 | c(NA, NA)*t3 + c(V5.thr3, V5.thr3)*t3
V5 | c(NA, NA)*t4 + c(V5.thr4, V5.thr4)*t4
V7 | c(NA, NA)*t1 + c(V7.thr1, V7.thr1)*t1
V7 | c(NA, NA)*t2 + c(V7.thr2, V7.thr2)*t2
V7 | c(NA, NA)*t3 + c(V7.thr3, V7.thr3)*t3
V7 | c(NA, NA)*t4 + c(V7.thr4, V7.thr4)*t4
V9 | c(NA, NA)*t1 + c(V9.thr1, V9.thr1)*t1
V9 | c(NA, NA)*t2 + c(V9.thr2, V9.thr2)*t2
V9 | c(NA, NA)*t3 + c(V9.thr3, V9.thr3)*t3
V9 | c(NA, NA)*t4 + c(V9.thr4, V9.thr4)*t4
V11 | c(NA, NA)*t1 + c(V11.thr1, V11.thr1)*t1
V11 | c(NA, NA)*t2 + c(V11.thr2, V11.thr2)*t2
V11 | c(NA, NA)*t3 + c(V11.thr3, V11.thr3)*t3
V11 | c(NA, NA)*t4 + c(V11.thr4, V11.thr4)*t4
V13 | c(NA, NA)*t1 + c(V13.thr1, V13.thr1)*t1
V13 | c(NA, NA)*t2 + c(V13.thr2, V13.thr2)*t2
V13 | c(NA, NA)*t3 + c(V13.thr3, V13.thr3)*t3
V13 | c(NA, NA)*t4 + c(V13.thr4, V13.thr4)*t4
V15 | c(NA, NA)*t1 + c(V15.thr1, V15.thr1)*t1
V15 | c(NA, NA)*t2 + c(V15.thr2, V15.thr2)*t2
V15 | c(NA, NA)*t3 + c(V15.thr3, V15.thr3)*t3
V15 | c(NA, NA)*t4 + c(V15.thr4, V15.thr4)*t4
V17 | c(NA, NA)*t1 + c(V17.thr1, V17.thr1)*t1
V17 | c(NA, NA)*t2 + c(V17.thr2, V17.thr2)*t2
V17 | c(NA, NA)*t3 + c(V17.thr3, V17.thr3)*t3
V17 | c(NA, NA)*t4 + c(V17.thr4, V17.thr4)*t4
V19 | c(NA, NA)*t1 + c(V19.thr1, V19.thr1)*t1
V19 | c(NA, NA)*t2 + c(V19.thr2, V19.thr2)*t2
V19 | c(NA, NA)*t3 + c(V19.thr3, V19.thr3)*t3
V19 | c(NA, NA)*t4 + c(V19.thr4, V19.thr4)*t4
V21 | c(NA, NA)*t1 + c(V21.thr1, V21.thr1)*t1
V21 | c(NA, NA)*t2 + c(V21.thr2, V21.thr2)*t2
V21 | c(NA, NA)*t3 + c(V21.thr3, V21.thr3)*t3
V21 | c(NA, NA)*t4 + c(V21.thr4, V21.thr4)*t4
V23 | c(NA, NA)*t1 + c(V23.thr1, V23.thr1)*t1
V23 | c(NA, NA)*t2 + c(V23.thr2, V23.thr2)*t2
V23 | c(NA, NA)*t3 + c(V23.thr3, V23.thr3)*t3
V23 | c(NA, NA)*t4 + c(V23.thr4, V23.thr4)*t4
V2 | c(NA, NA)*t1 + c(V2.thr1, V2.thr1)*t1
V2 | c(NA, NA)*t2 + c(V2.thr2, V2.thr2)*t2
V2 | c(NA, NA)*t3 + c(V2.thr3, V2.thr3)*t3
V2 | c(NA, NA)*t4 + c(V2.thr4, V2.thr4)*t4
V4 | c(NA, NA)*t1 + c(V4.thr1, V4.thr1)*t1
V4 | c(NA, NA)*t2 + c(V4.thr2, V4.thr2)*t2
V4 | c(NA, NA)*t3 + c(V4.thr3, V4.thr3)*t3
V4 | c(NA, NA)*t4 + c(V4.thr4, V4.thr4)*t4
V6 | c(NA, NA)*t1 + c(V6.thr1, V6.thr1)*t1
V6 | c(NA, NA)*t2 + c(V6.thr2, V6.thr2)*t2
V6 | c(NA, NA)*t3 + c(V6.thr3, V6.thr3)*t3
V6 | c(NA, NA)*t4 + c(V6.thr4, V6.thr4)*t4
V8 | c(NA, NA)*t1 + c(V8.thr1, V8.thr1)*t1
V8 | c(NA, NA)*t2 + c(V8.thr2, V8.thr2)*t2
V8 | c(NA, NA)*t3 + c(V8.thr3, V8.thr3)*t3
V8 | c(NA, NA)*t4 + c(V8.thr4, V8.thr4)*t4
V10 | c(NA, NA)*t1 + c(V10.thr1, V10.thr1)*t1
V10 | c(NA, NA)*t2 + c(V10.thr2, V10.thr2)*t2
V10 | c(NA, NA)*t3 + c(V10.thr3, V10.thr3)*t3
V10 | c(NA, NA)*t4 + c(V10.thr4, V10.thr4)*t4
V12 | c(NA, NA)*t1 + c(V12.thr1, V12.thr1)*t1
V12 | c(NA, NA)*t2 + c(V12.thr2, V12.thr2)*t2
V12 | c(NA, NA)*t3 + c(V12.thr3, V12.thr3)*t3
V12 | c(NA, NA)*t4 + c(V12.thr4, V12.thr4)*t4
V14 | c(NA, NA)*t1 + c(V14.thr1, V14.thr1)*t1
V14 | c(NA, NA)*t2 + c(V14.thr2, V14.thr2)*t2
V14 | c(NA, NA)*t3 + c(V14.thr3, V14.thr3)*t3
V14 | c(NA, NA)*t4 + c(V14.thr4, V14.thr4)*t4
V16 | c(NA, NA)*t1 + c(V16.thr1, V16.thr1)*t1
V16 | c(NA, NA)*t2 + c(V16.thr2, V16.thr2)*t2
V16 | c(NA, NA)*t3 + c(V16.thr3, V16.thr3)*t3
V16 | c(NA, NA)*t4 + c(V16.thr4, V16.thr4)*t4
V18 | c(NA, NA)*t1 + c(V18.thr1, V18.thr1)*t1
V18 | c(NA, NA)*t2 + c(V18.thr2, V18.thr2)*t2
V18 | c(NA, NA)*t3 + c(V18.thr3, V18.thr3)*t3
V18 | c(NA, NA)*t4 + c(V18.thr4, V18.thr4)*t4
V20 | c(NA, NA)*t1 + c(V20.thr1, V20.thr1)*t1
V20 | c(NA, NA)*t2 + c(V20.thr2, V20.thr2)*t2
V20 | c(NA, NA)*t3 + c(V20.thr3, V20.thr3)*t3
V20 | c(NA, NA)*t4 + c(V20.thr4, V20.thr4)*t4
V22 | c(NA, NA)*t1 + c(V22.thr1, V22.thr1)*t1
V22 | c(NA, NA)*t2 + c(V22.thr2, V22.thr2)*t2
V22 | c(NA, NA)*t3 + c(V22.thr3, V22.thr3)*t3
V22 | c(NA, NA)*t4 + c(V22.thr4, V22.thr4)*t4
V24 | c(NA, NA)*t1 + c(V24.thr1, V24.thr1)*t1
V24 | c(NA, NA)*t2 + c(V24.thr2, V24.thr2)*t2
V24 | c(NA, NA)*t3 + c(V24.thr3, V24.thr3)*t3
V24 | c(NA, NA)*t4 + c(V24.thr4, V24.thr4)*t4
')
      Mod_list_test.thr.ARS <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.thr.ARS[[i]] <- c(FullyConstrainedARS[[1]], FullyConstrainedARS[[2]], Thr.rel[[i-1]])
      }
      
      Mod_list_test.load.ARS <- list()
      for(i in 2:(nitems+1)){
        Mod_list_test.load.ARS [[i]] <- c(FullyConstrainedARS[[1]],FullyConstrainedARS[[3]], Load.rel[[i-1]])
      }
      
      Mod_list_test.load.ARS[[1]] <- Mod_list_test.thr.ARS[[1]] <- FullyConstrainedARS
      
      
    }
    
  }
  
  
  results <- list()      
  results[[1]] <- Mod_list_test.thr
  results[[2]] <- Mod_list_test.load
  results[[3]] <- Mod_list_test.thr.ARS
  results[[4]] <- Mod_list_test.load.ARS
  
  
  names(results) <- c("ThrModnoARS", "LoadModnoARS", "ThrModARS", "LoadModARS")      
  return(results)
}

Estimate.MI.Models.item <- function(Data, Models.item, Nfac, nitems){
  
  #fit models following the backward approach (MG-IRT-based), where the constraints are released for a single item once at a time  
  
  # Because in the case of models with 2 factors, the first two items are used to identify factor 1 and 2, respectively, it 
  # is necessary to distinguish these models and make sure that a test is ran only for those items that are not used to
  # identify the variance of the latent factor(s)
  
  #Thresholds invariance  
  Mod.fit.thr <- list()
  Fit.Meas.thr <- list()
  
  #Loadings invariance
  Mod.fit.load <- list()
  Fit.Meas.load <- list()
  
  #Thresholds invariance  
  Mod.fit.thr.ARS <- list()
  Fit.Meas.thr.ARS <- list()
  
  #Loadings invariance
  Mod.fit.load.ARS <- list()
  Fit.Meas.load.ARS <- list()
  
  if(Nfac == 1){ start <- 3 }  else { start <- 4}
  
  tic()
  for(i in start:(nitems+1)){
    Mod.fit.thr[[i]] <- myTryCatch(cfa(as.character(Models.item$ThrModnoARS[[i]]), 
                                       data = Data, 
                                       group = "group",
                                       ordered = colnames(Data[,-c(dim(Data)[2])]),
                                       estimator = "WLSMV"))
    
    
    
    Mod.fit.thr.ARS[[i]] <- myTryCatch(cfa(as.character(Models.item$ThrModARS[[i]]), 
                                           data = Data, 
                                           group = "group",
                                           ordered = colnames(Data[,-c(dim(Data)[2])]),
                                           estimator = "WLSMV"))
    
    Mod.fit.load[[i]] <- myTryCatch(cfa(as.character(Models.item$LoadModnoARS[[i]]), 
                                        data = Data, 
                                        group = "group",
                                        ordered = colnames(Data[,-c(dim(Data)[2])]),
                                        estimator = "WLSMV"))
    Mod.fit.load.ARS[[i]] <- myTryCatch(cfa(as.character(Models.item$LoadModARS[[i]]), 
                                            data = Data, 
                                            group = "group",
                                            ordered = colnames(Data[,-c(dim(Data)[2])]),
                                            estimator = "WLSMV"))
  }
  toc()    
  
  Mod.fit.thr[[1]] <- myTryCatch(cfa(as.character(Models.item$ThrModnoARS[[1]]), 
                                     data = Data, 
                                     group = "group",
                                     ordered = colnames(Data[,-c(dim(Data)[2])]),
                                     estimator = "WLSMV"))
  Mod.fit.thr.ARS[[1]] <- myTryCatch(cfa(as.character(Models.item$ThrModARS[[1]]), 
                                         data = Data, 
                                         group = "group",
                                         ordered = colnames(Data[,-c(dim(Data)[2])]),
                                         estimator = "WLSMV"))
  Mod.fit.load[[1]] <- myTryCatch(cfa(as.character(Models.item$LoadModnoARS[[1]]), 
                                      data = Data, 
                                      group = "group",
                                      ordered = colnames(Data[,-c(dim(Data)[2])]),
                                      estimator = "WLSMV"))
  Mod.fit.load.ARS[[1]] <- myTryCatch(cfa(as.character(Models.item$LoadModARS[[1]]), 
                                          data = Data, 
                                          group = "group",
                                          ordered = colnames(Data[,-c(dim(Data)[2])]),
                                          estimator = "WLSMV"))
  
  
  
  Est.Models <- list()
  Est.Models[[1]] <- Mod.fit.thr
  Est.Models[[2]] <- Mod.fit.thr.ARS
  Est.Models[[3]] <- Mod.fit.load
  Est.Models[[4]] <- Mod.fit.load.ARS
  
  names(Est.Models) <- c('Mod.Thr', 'Mod.Thr.ARS', 'Mod.Load', 'Mod.Load.ARS')
  
  
  Results.fit.thr <- list()
  Results.fit.thr.ARS <- list()
  Results.fit.load <- list()
  Results.fit.load.ARS <- list()
  
  
  
  for(i in start:(nitems+1)){
    
#    if(any(is.na(Mod.fit.thr[[i]]$value@Fit@se)) | any(is.na(Mod.fit.thr[[1]]$value@Fit@se) )){
#      Results.fit.thr[[i]] <- Mod.fit.thr[[i]]$warning
#    } else {
      Results.fit.thr[[i]] <- c(myTryCatch(lavTestLRT(Mod.fit.thr[[1]]$value, Mod.fit.thr[[i]]$value)),
                                myTryCatch(fitmeasures(Mod.fit.thr[[i]]$value)))
#    }
    
#    if(any(is.na(Mod.fit.thr.ARS[[i]]$value@Fit@se)) | any(is.na(Mod.fit.thr.ARS[[1]]$value@Fit@se) )){
#      Results.fit.thr.ARS[[i]] <- Mod.fit.thr.ARS[[i]]$warning
#    } else {
      Results.fit.thr.ARS[[i]] <- c(myTryCatch(lavTestLRT(Mod.fit.thr.ARS[[1]]$value, Mod.fit.thr.ARS[[i]]$value)),
                                    myTryCatch(fitmeasures(Mod.fit.thr.ARS[[i]]$value)))
#    }
    
#    if(any(is.na(Mod.fit.load[[i]]$value@Fit@se)) | any(is.na(Mod.fit.load[[1]]$value@Fit@se) )){
#      Results.fit.load[[i]] <- Mod.fit.load[[i]]$warning
#    } else {
      Results.fit.load[[i]] <- c(myTryCatch(lavTestLRT(Mod.fit.load[[1]]$value, Mod.fit.load[[i]]$value)),
                                 fitmeasures(Mod.fit.load[[i]]$value))
#    }
    
#    if(any(is.na(Mod.fit.load.ARS[[i]]$value@Fit@se)) | any(is.na(Mod.fit.load.ARS[[1]]$value@Fit@se) )){
#      Results.fit.load.ARS[[i]] <- Mod.fit.load.ARS[[i]]$warning
#    } else {
      Results.fit.load.ARS[[i]] <- c(myTryCatch(lavTestLRT(Mod.fit.load.ARS[[1]]$value, Mod.fit.load.ARS[[i]]$value)),
                                     myTryCatch(fitmeasures(Mod.fit.load.ARS[[i]]$value)))
#    }
    
  }
  
  
  #  Results.fit[[1]] <- fitmeasures(fit.conf$value)
  #  Results.fit[[2]] <- fitmeasures(fit.thr$value)
  #  Results.fit[[3]] <- fitmeasures(fit.load$value)
  #  Results.fit[[4]] <- fitmeasures(fit.confARS$value)
  #  Results.fit[[5]] <- fitmeasures(fit.thrARS$value)
  #  Results.fit[[6]] <- fitmeasures(fit.loadARS$value)
  
  Results.fit <- list()
  Results.fit[[1]] <- Results.fit.thr
  Results.fit[[2]] <- Results.fit.thr.ARS
  Results.fit[[3]] <- Results.fit.load
  Results.fit[[4]] <- Results.fit.load.ARS
  

  names(Results.fit) <- c('Thr.fit', 'Thr.fit.ARS', 
                          'Load.fit', 'Load.fit.ARS')
  
  
  results <- c(#Est.Models, 
               Results.fit)
  
  return(results)
}






