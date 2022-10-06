Model_Creation.item <- function(Nfac, nitems){
  

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
  