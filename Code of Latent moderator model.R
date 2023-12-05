setwd("~/Proactive project")
data <- read.csv('Proactive Field Study after cleaning.csv') 
# Read the original data file as data frame table. 

data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                           as.factor)

library(lavaan)
library(dplyr)
data <- mutate(data, SLPar1 = rowMeans(data[, c('SL1','SL5','SL6')], na.rm = TRUE))
data <- mutate(data, SLPar2 = rowMeans(data[, c('SL3','SL4')], na.rm = TRUE))
data <- mutate(data, SLPar3 = rowMeans(data[, c('SL2','SL7')], na.rm = TRUE))
# Add parceling indicators SLPar1, SLPar2, SLPar3 to the data frame. 

data <- mutate(data, TLPar1 = rowMeans(data[, c('TL1','TL2','TL7')], na.rm = TRUE))
data <- mutate(data, TLPar2 = rowMeans(data[, c('TL3','TL4')], na.rm = TRUE))
data <- mutate(data, TLPar3 = rowMeans(data[, c('TL5','TL6')], na.rm = TRUE))
# Add parceling indicators TLPar1, TLPar2, TLPar3 to the data frame. 

data <- mutate(data, AbusivePar1 = rowMeans(data[, c('Abusive1','Abusive3')], na.rm = TRUE))
data <- mutate(data, AbusivePar2 = rowMeans(data[, c('Abusive2','Abusive5')], na.rm = TRUE))
data <- mutate(data, AbusivePar3 = rowMeans(data[, c('Abusive4','Abusive4')], na.rm = TRUE))
# Add parceling indicators AbusivePar1, AbusivePar2, AbusivePar3 to the data frame. 

data <- mutate(data, ProactivePar1 = rowMeans(data[, c('Proactive1','Proactive3','Proactive4','Proactive7')], na.rm = TRUE))
data <- mutate(data, ProactivePar2 = rowMeans(data[, c('Proactive2','Proactive5','Proactive6','Proactive10')], na.rm = TRUE))
data <- mutate(data, ProactivePar3 = rowMeans(data[, c('Proactive8','Proactive9')], na.rm = TRUE))
# Add parceling indicators ProactivePar1, ProactivePar2, ProactivePar3 to the data frame. 

data <- mutate(data, ConsciPar1 = rowMeans(data[, c('Consci1','Consci3','Consci5','Consci8')], na.rm = TRUE))
data <- mutate(data, ConsciPar2 = rowMeans(data[, c('Consci2','Consci4','Consci7')], na.rm = TRUE))
data <- mutate(data, ConsciPar3 = rowMeans(data[, c('Consci6','Consci9')], na.rm = TRUE))
# Add parceling indicators ConsciPar1, ConsciPar2, ConsciPar3 to the data frame. 

data <- mutate(data, ExtravPar1 = rowMeans(data[, c('Extrav1','Extrav2','Extrav5','Extrav6')], na.rm = TRUE))
data <- mutate(data, ExtravPar2 = rowMeans(data[, c('Extrav4', 'Extrav7')], na.rm = TRUE))
data <- mutate(data, ExtravPar3 = rowMeans(data[, c('Extrav3','Extrav8')], na.rm = TRUE))
# Add parceling indicators ExtravPar1, ExtravPar2, ExtravPar3 to the data frame. 

#############################################################################################################
#r manually adding first-order indicators
#Step 1: Multiply all first-order variables
# SLPar1 SLPar2 SLPar3
# ProactivePar1 ProactivePar2 ProactivePar3
data$pt_SLP11 <- data$SLPar1 * data$ProactivePar1
data$pt_SLP12 <- data$SLPar1 * data$ProactivePar2
data$pt_SLP13 <- data$SLPar1 * data$ProactivePar3
data$pt_SLP21 <- data$SLPar2 * data$ProactivePar1
data$pt_SLP22 <- data$SLPar2 * data$ProactivePar2
data$pt_SLP23 <- data$SLPar2 * data$ProactivePar3
data$pt_SLP31 <- data$SLPar3 * data$ProactivePar1
data$pt_SLP32 <- data$SLPar3 * data$ProactivePar2
data$pt_SLP33 <- data$SLPar3 * data$ProactivePar3

data$res_SLP11 <- resid(lm(pt_SLP11 ~ SLPar1 + SLPar2 + SLPar3 + ProactivePar1 + ProactivePar2 + ProactivePar3, 
                           data = data, na.action = na.exclude))
data$res_SLP12 <- resid(lm(pt_SLP12 ~ SLPar1 + SLPar2 + SLPar3 + ProactivePar1 + ProactivePar2 + ProactivePar3, 
                           data = data,na.action = na.exclude))
data$res_SLP13 <- resid(lm(pt_SLP13 ~ SLPar1 + SLPar2 + SLPar3 + ProactivePar1 + ProactivePar2 + ProactivePar3,  
                           data = data,na.action = na.exclude))
data$res_SLP21 <- resid(lm(pt_SLP21 ~ SLPar1 + SLPar2 + SLPar3 + ProactivePar1 + ProactivePar2 + ProactivePar3, 
                           data = data,na.action = na.exclude))
data$res_SLP22 <- resid(lm(pt_SLP22 ~ SLPar1 + SLPar2 + SLPar3 + ProactivePar1 + ProactivePar2 + ProactivePar3, 
                           data = data,na.action = na.exclude))
data$res_SLP23 <- resid(lm(pt_SLP23 ~ SLPar1 + SLPar2 + SLPar3 + ProactivePar1 + ProactivePar2 + ProactivePar3, 
                           data = data,na.action = na.exclude))
data$res_SLP31 <- resid(lm(pt_SLP31 ~ SLPar1 + SLPar2 + SLPar3 + ProactivePar1 + ProactivePar2 + ProactivePar3, 
                           data = data,na.action = na.exclude))
data$res_SLP32 <- resid(lm(pt_SLP32 ~ SLPar1 + SLPar2 + SLPar3 + ProactivePar1 + ProactivePar2 + ProactivePar3, 
                           data = data,na.action = na.exclude))
data$res_SLP33 <- resid(lm(pt_SLP33 ~ SLPar1 + SLPar2 + SLPar3 + ProactivePar1 + ProactivePar2 + ProactivePar3, 
                           data = data,na.action = na.exclude))

mean(data$res_SLP11,na.rm=TRUE)

#For the latter, the correlation matrix of all variables is calculated:
modvar_SLP <- data[c("SLPar1", "SLPar2", "SLPar3", "ProactivePar1", "ProactivePar2", "ProactivePar3",
                     "res_SLP11","res_SLP12","res_SLP13","res_SLP21", "res_SLP22","res_SLP23","res_SLP31","res_SLP32", "res_SLP33")]

round(cor(modvar_SLP,use="pair"),2)


inflat_SLP <- '
  SL =~ SLPar1 + SLPar2 + SLPar3
  Proactive =~ ProactivePar1 + ProactivePar2 + ProactivePar3
  PO_fit =~ PO1 + PO2 + PO3
  PRTRM_SLP =~ res_SLP11+res_SLP12+res_SLP13+res_SLP21+res_SLP22+res_SLP23+res_SLP31+res_SLP32+res_SLP33
  PRTRM_SLP ~~0*Proactive #The product term does not correlate with 
  PRTRM_SLP ~~0*SL #the first-order-effect-variables

#Error covariances of relevant product term indciators
  res_SLP11~~res_SLP12
  res_SLP12~~res_SLP13
  res_SLP11~~res_SLP13 
  
  #res_SLP21, res_SLP22, res_SLP23
  res_SLP21~~res_SLP22
  res_SLP22~~res_SLP23
  res_SLP21~~res_SLP23

  #res_SLP31, res_SLP32, res_SLP33
  res_SLP31~~res_SLP32
  res_SLP32~~res_SLP33
  res_SLP31~~res_SLP33
  
  res_SLP11~~res_SLP21
  res_SLP11~~res_SLP31
  res_SLP21~~res_SLP31

  #res_SLP12, res_SLP22, und res_SLP32
  res_SLP12~~res_SLP22
  res_SLP12~~res_SLP32
  res_SLP22~~res_SLP32

  #res_SLP13, res_SLP23, und res_SLP33
  res_SLP13~~res_SLP23
  res_SLP13~~res_SLP33
  res_SLP23~~res_SLP33
  
  #Structural model
  PO_fit ~ Proactive + SL + PRTRM_SLP
    '

fit_SLP <- sem(inflat_SLP, data=data, missing="fiml")
summary(fit_SLP)