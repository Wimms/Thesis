library(MASS)
library(tidyverse)
library(caret)
library(leaps)
library(dplyr)
library(ggplot2)
library(glmnet)
library(tree)
library(randomForest)
library(reshape2)
library(plotROC)
library(Rcpp)
library(reliabilitydiag)
library(murphydiagram)
library(MASS)
library(lmtest)
library(sandwich)
library(grf) 
library(quantreg)
library(rqPen)
library(quantmod)
library(qrnn)
library(rollRegres)
library(quantregForest)
library(randomForest)
library(gbm)
library(h2o)
library(lqr)
library(lubridate)
library(gridExtra)


#setwd("C:/Users/Marc/Desktop/Master/FINAL PROJECT")

rm(list = ls())
# Clear the environment
set.seed(1)

NFCI <- read.csv( # national financial conditions index
  file = "nfci.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  stringsAsFactor = TRUE
)

GDP <- read.csv( # GDP growth rates
  file = "gdp.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  stringsAsFactor = TRUE
)

TS <- read.csv( # term spread
  file = "ts.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  stringsAsFactor = TRUE
)

SV <- read.csv( # stock variance
  file = "sv.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  stringsAsFactor = TRUE
)

HP <- read.csv( # housing prices
  file = "hp.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  stringsAsFactor = TRUE
)

CG <- read.csv( # credit-to-GDP gap
  file = "cg.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  stringsAsFactor = TRUE
)

CR <- read.csv( # credit-to-GDP growth
  file = "cr.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  stringsAsFactor = TRUE
)

IGREA <- read.csv( # index of global real economic activity (taken from federal reserve bank of dallas)  change, index, not seasonally adjusted, with quarterly average
  file = "IGREA.csv",
  header = TRUE,
  sep = ",",
  dec = ".",
  stringsAsFactor = TRUE
)

# Function to create a data set with all relevant data for a specific country using the position of the country in the data sets.
# BE CAREFUL THIS REQUIRES THE SAME POSITION OF COUNTRIES IN ALL DATA SETS (excluding data sets with global data)
country <- function(pos) {
  name <- data.frame(GDP[104:279, 2], GDP[104:279,pos], GDP[105:280, pos], GDP[106:281, pos], GDP[107:282, pos], GDP[108:283, pos],
                     NFCI[pos],  TS[35:210, pos], SV[93:268,3], HP[11:186, pos], CG[43:218, pos],
                     CR[83:258, pos], IGREA[20:195, 2])
  rownames(name) <- NULL
  colnames(name) <- c("Quarter", "GDP", "GDPt1", "GDPt2", "GDPt3", "GDPt4", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
  return(name)
}

# combining the different data sets given our time frame for several countries.
USA <- country(14)
Japan <- country(9)
Germany <- country(17)

### IN SAMPLE ANALYSIS ###

# IN-SAMPLE bivariate regression analysis
bivreg <- function(country) {
  NFCIt1 <- quantreg::rq(GDPt1~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[3], country[7]))
  NFCIt2 <- quantreg::rq(GDPt2~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[4], country[7]))
  NFCI <- list("NFCIt1" = NFCIt1, "NFCIt2" = NFCIt2)
  
  TSt1 <- quantreg::rq(GDPt1~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[3], country[8]))
  TSt2 <- quantreg::rq(GDPt2~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[4], country[8]))
  TS <- list("TSt1" = TSt1, "TSt2" = TSt2)
  
  SVt1 <- quantreg::rq(GDPt1~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[3], country[9]))
  SVt2 <- quantreg::rq(GDPt2~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[4], country[9]))
  SV <- list("SVt1" = SVt1, "SVt2" = SVt2)
  
  HPt1 <- quantreg::rq(GDPt1~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[3], country[10]))
  HPt2 <- quantreg::rq(GDPt2~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[4], country[10]))
  HP <- list("HPt1" = HPt1, "HPt2" = HPt2)
  
  CGt1 <- quantreg::rq(GDPt1~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[3], country[11]))
  CGt2 <- quantreg::rq(GDPt2~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[4], country[11]))
  CG <- list("CGt1" = CGt1, "CGt2" = CGt2)
  
  CRt1 <- quantreg::rq(GDPt1~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[3], country[12]))
  CRt2 <- quantreg::rq(GDPt2~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[4], country[12]))
  CR <- list("CRt1" = CRt1, "CRt2" = CRt2)
  
  IGREAt1 <- quantreg::rq(GDPt1~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[3], country[13]))
  IGREAt2 <- quantreg::rq(GDPt2~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[4], country[13]))
  IGREA <- list("IGREAt1" = IGREAt1, "IGREAt2" = IGREAt2)
  
  final <-list("NFCI" = NFCI, "TS" = TS, "SV" = SV, "HP" = HP, "CG" = CG, "CR" = CR, "IGREA" = IGREA)
  return(final)
}

# IN-SAMPLE multivariate regression analysis
multireg <- function(country) {
  t1 <- quantreg::rq(GDPt1~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[3], country[7], country[8], country[9], country[10], country[11], country[12], country[13]))
  t2 <- quantreg::rq(GDPt2~., tau=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data=c(country[2], country[4], country[7], country[8], country[9], country[10], country[11], country[12], country[13]))
  final <- list("t1" = t1, "t2" = t2)
  return(final)
}

# IN-SAMPLE lasso analysis
lasso <- function(country) {
  t1 <- quantreg::rq(GDPt1~., tau = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data = c(country[2], country[3], country[7], country[8], country[9], country[10], country[11], country[12], country[13]), method = "lasso")
  t2 <- quantreg::rq(GDPt2~., tau = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), data = c(country[2], country[4], country[7], country[8], country[9], country[10], country[11], country[12], country[13]), method = "lasso")
  final <- list("t1" = t1, "t2" = t2)
  return(final)
}

# Running the In-Sample function all at once
insample <- function(country) {
  Bivariate <- bivreg(country)
  Multivariate <- multireg(country)
  Lasso <- lasso(country)
  results <- list("Bivariate" = Bivariate, "Multivariate" = Multivariate, "Lasso" = Lasso)
  return(results)
}

InSampleUSA <- insample(USA)
InSampleJapan <- insample(Japan)
InSampleGermany <- insample(Germany)


#################
# OUT OF SAMPLE #
#################

# Creating Samples for out-of-sample predictions
sample <- function(country, start, finish) {
  data_train <- country[start:finish,]
  data_test <- country[-(start:finish),]
  final <- list("data_train" = data_train, "data_test" = data_test)
  return(final)
}

### PREDICTIONS ###

# # Rolling Window Quantile Regression (multiple fits, bivariate)
rwrbi <- function(country, window) {
  pred_t1 <- data.frame()
  pred_t2 <- data.frame()
  for (i in 1:(nrow(country)-window)) {
    
    reg_datat1 <- data.frame(country[i:window, 3], country[i:window, 2], country[i:window, 7],
                             country[i:window, 8], country[i:window, 9], country[i:window, 10],
                             country[i:window, 11], country[i:window, 12], country[i:window, 13])
    rownames(reg_datat1) <- NULL
    colnames(reg_datat1) <- c("GDPt1", "GDP", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
    
    reg_datat2 <- data.frame(country[i:window, 4], country[i:window, 2], country[i:window, 7],
                             country[i:window, 8], country[i:window, 9], country[i:window, 10],
                             country[i:window, 11], country[i:window, 12], country[i:window, 13])
    rownames(reg_datat2) <- NULL
    colnames(reg_datat2) <- c("GDPt2", "GDP", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
    
    window <- window+1
    
    pred_datat1 <- data.frame(country[window, 3], country[window, 2], country[window, 7],
                              country[window, 8], country[window, 9], country[window, 10],
                              country[window, 11], country[window, 12], country[window, 13])
    rownames(pred_datat1) <- NULL
    colnames(pred_datat1) <- c("GDPt1", "GDP", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
    
    pred_datat2 <- data.frame(country[window, 4], country[window, 2], country[window, 7],
                              country[window, 8], country[window, 9], country[window, 10],
                              country[window, 11], country[window, 12], country[window, 13])
    rownames(pred_datat2) <- NULL
    colnames(pred_datat2) <- c("GDPt2", "GDP", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
    
    for (z in 3:9) {
      t1 <- quantreg::rq(GDPt1~., tau = 0.05, data = c(reg_datat1[1],reg_datat1[2], reg_datat1[z]))
      predt1 <- as.data.frame(predict(t1, newdata = c(pred_datat1[2] ,pred_datat1[z])))   
      pred_t1[i, z-2] <- predt1
      
      t2 <- quantreg::rq(GDPt2~., tau = 0.05, data = c(reg_datat2[1],reg_datat2[2], reg_datat1[z]))
      predt2 <- as.data.frame(predict(t2, newdata = c(pred_datat2[2] ,pred_datat2[z])))   
      pred_t2[i, z-2] <- predt2
    }
    t1 <- quantreg::rq(GDPt1~., tau = 0.05, data = c(reg_datat1[1],reg_datat1[2]))
    predt1 <- as.data.frame(predict(t1, newdata = c(pred_datat1[2])))   
    pred_t1[i, 8] <- predt1
    
    t2 <- quantreg::rq(GDPt2~., tau = 0.05, data = c(reg_datat2[1],reg_datat2[2]))
    predt2 <- as.data.frame(predict(t2, newdata = c(pred_datat2[2])))   
    pred_t2[i, 8] <- predt2
    predictions <- list("t1" = pred_t1, "t2" = pred_t2)
    colnames(predictions$t1) <- c("based on NFCI", "based on TS",
                                  "based on SV", "based on HP", "based on CG",
                                  "based on CR", "based on IGREA", "based on GDP")
    colnames(predictions$t2) <- c("based on NFCI", "based on TS",
                                  "based on SV", "based on HP", "based on CG",
                                  "based on CR", "based on IGREA", "based on GDP")
    
  }
  return(predictions)  
}

# # Rolling Window Quantile Regression (multiple fits, multivariate,)
rwrmulti <- function(country, window) {
  pred_t1 <- data.frame()
  pred_t2 <- data.frame()
  for (i in 1:(nrow(country)-window)) {
    
    reg_data <- data.frame(country[i:window, 2], country[i:window, 3], country[i:window, 7],
                           country[i:window, 8], country[i:window, 9], country[i:window, 10],
                           country[i:window, 11], country[i:window, 12], country[i:window, 13])
    rownames(reg_data) <- NULL
    colnames(reg_data) <- c("GDP", "GDPt1", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
    
    t1 <- quantreg::rq(GDPt1~., tau = 0.05, data = c(reg_data[1], reg_data[2], reg_data[3],
                                                     reg_data[4], reg_data[5], reg_data[6],
                                                     reg_data[7], reg_data[8], reg_data[9]))
    # Doing the same for t+2 prediction
    reg_data2 <- data.frame(country[i:window, 2], country[i:window, 4], country[i:window, 7],
                            country[i:window, 8], country[i:window, 9], country[i:window, 10],
                            country[i:window, 11], country[i:window, 12], country[i:window, 13])
    rownames(reg_data2) <- NULL
    colnames(reg_data2) <- c("GDP", "GDPt2", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
    
    t2 <- quantreg::rq(GDPt2~., tau = 0.05, data = c(reg_data2[1], reg_data2[2], reg_data2[3],
                                                     reg_data2[4], reg_data2[5], reg_data2[6],
                                                     reg_data2[7], reg_data2[8], reg_data2[9]))
    
    window <- window+1
    
    pred_data <- data.frame(country[window, 2], country[window, 3], country[window, 7],
                            country[window, 8], country[window, 9], country[window, 10],
                            country[window, 11], country[window, 12], country[window, 13])
    rownames(pred_data) <- NULL
    colnames(pred_data) <- c("GDP", "GDPt1", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
    
    predt1 <- as.data.frame(predict(t1, newdata = c(pred_data[1], pred_data[3],
                                                    pred_data[4], pred_data[5], pred_data[6],
                                                    pred_data[7], pred_data[8], pred_data[9])))
    
    pred_t1[i, 1] <- predt1
    # SAME STEP FOR THE PREDICTIONS OF t+2
    pred_data2 <- data.frame(country[window, 2], country[window, 4], country[window, 7],
                             country[window, 8], country[window, 9], country[window, 10],
                             country[window, 11], country[window, 12], country[window, 13])
    rownames(pred_data2) <- NULL
    colnames(pred_data2) <- c("GDP", "GDPt2", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
    
    predt2 <- as.data.frame(predict(t2, newdata = c(pred_data2[1], pred_data2[3],
                                                    pred_data2[4], pred_data2[5], pred_data2[6],
                                                    pred_data2[7], pred_data2[8], pred_data2[9])))
    
    pred_t2[i, 1] <- predt2
  }
  colnames(pred_t1) <- c("t1")
  colnames(pred_t2) <- c("t2")
  predictions <- list("t1" = pred_t1, "t2" = pred_t2)
  return(predictions)
}

# # Rolling Window Quantile Regression (multiple fits, multivariate, LASSO )
rwrmultilasso <- function(country, window) { #LASSO
  pred_t1 <- data.frame()
  pred_t2 <- data.frame()
  for (i in 1:(nrow(country)-window)) {
    
    reg_data <- data.frame(country[i:window, 2], country[i:window, 3], country[i:window, 7],
                           country[i:window, 8], country[i:window, 9], country[i:window, 10],
                           country[i:window, 11], country[i:window, 12], country[i:window, 13])
    rownames(reg_data) <- NULL
    colnames(reg_data) <- c("GDP", "GDPt1", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
    
    t1 <- quantreg::rq(GDPt1~., tau = 0.05, data = c(reg_data[1], reg_data[2], reg_data[3],
                                                     reg_data[4], reg_data[5], reg_data[6],
                                                     reg_data[7], reg_data[8], reg_data[9]), method = "lasso")
    # Doing the same for t+2 prediction
    reg_data2 <- data.frame(country[i:window, 2], country[i:window, 4], country[i:window, 7],
                            country[i:window, 8], country[i:window, 9], country[i:window, 10],
                            country[i:window, 11], country[i:window, 12], country[i:window, 13])
    rownames(reg_data2) <- NULL
    colnames(reg_data2) <- c("GDP", "GDPt2", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
    
    t2 <- quantreg::rq(GDPt2~., tau = 0.05, data = c(reg_data2[1], reg_data2[2], reg_data2[3],
                                                     reg_data2[4], reg_data2[5], reg_data2[6],
                                                     reg_data2[7], reg_data2[8], reg_data2[9]), method = "lasso")
    
    window <- window+1
    
    pred_data <- data.frame(country[window, 2], country[window, 3], country[window, 7],
                            country[window, 8], country[window, 9], country[window, 10],
                            country[window, 11], country[window, 12], country[window, 13])
    rownames(pred_data) <- NULL
    colnames(pred_data) <- c("GDP", "GDPt1", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
    
    predt1 <- as.data.frame(predict(t1, newdata = c(pred_data[1], pred_data[3],
                                                    pred_data[4], pred_data[5], pred_data[6],
                                                    pred_data[7], pred_data[8], pred_data[9])))
    
    pred_t1[i, 1] <- predt1
    # SAME STEP FOR THE PREDICTIONS OF t+2
    pred_data2 <- data.frame(country[window, 2], country[window, 4], country[window, 7],
                             country[window, 8], country[window, 9], country[window, 10],
                             country[window, 11], country[window, 12], country[window, 13])
    rownames(pred_data2) <- NULL
    colnames(pred_data2) <- c("GDP", "GDPt2", "NFCI", "TS", "SV", "HP", "CG", "CR", "IGREA")
    
    predt2 <- as.data.frame(predict(t2, newdata = c(pred_data2[1], pred_data2[3],
                                                    pred_data2[4], pred_data2[5], pred_data2[6],
                                                    pred_data2[7], pred_data2[8], pred_data2[9])))
    
    pred_t2[i, 1] <- predt2
  }
  colnames(pred_t1) <- c("t1")
  colnames(pred_t2) <- c("t2")
  predictions <- list("t1" = pred_t1, "t2" = pred_t2)
  return(predictions)
}

# # ROLLING WINDOW QUANTILE RANDOM FOREST

rwrquantregforest <- function(country, window, trees, share) {
  pred_t1 <- data.frame()
  pred_t2 <- data.frame()
  for (i in 1:(nrow(country)-window)) {
    Xtraint1 <- (country[i:window, -(3:6)])[,-1]
    Ytraint1 <- country[i:window, 3]
    
    Xtraint2 <- (country[i:window, -(3:6)])[,-1]
    Ytraint2 <- country[i:window, 4]
    
    window <- window+1
    
    Xtestt1 <- (country[window, -(3:6)])[,-1]
    Xtestt2 <- (country[window, -(3:6)])[,-1]
    
    qrft1 <- quantregForest(Xtraint1, Ytraint1, ntree = trees, mtry = ceiling(ncol(Xtraint1)*share), importance = TRUE)
    condquantt1 <- predict(qrft1, Xtestt1, what = 0.05)
    pred_t1[i, 1] <- condquantt1
    
    qrft2 <- quantregForest(Xtraint2, Ytraint2, ntree = trees, mtry = ceiling(ncol(Xtraint2)*share), importane = TRUE)
    condquantt2 <- predict(qrft2, Xtestt2, what = 0.05)
    pred_t2[i, 1] <- condquantt2
    
    predictions <- list("t1" = pred_t1, "t2" = pred_t2)
  }
  return(predictions)  
}
# HIGH COMPUTING DEMAND

# # Quantile Gradient Boosting Multiple Fits

QGBmf <- function(country, window) {
  pred_t1 <- data.frame()
  pred_t2 <- data.frame()
  for (i in 1:(nrow(country)-window)) {
    Xtraint1 <- (country[i:window, -(4:6)])[,-1]
    
    
    Xtraint2 <- (country[i:window, -(5:6)])[,-1]
    
    
    window <- window+1
    
    Xtestt1 <- (country[window, -(3:6)])[,-1]
    Xtestt2 <- (country[window, -(3:6)])[,-1]
    
    QGB1 <- gbm(GDPt1~., data = Xtraint1, distribution = list(name = "quantile",alpha=0.05), n.trees = 10000, shrinkage = 0.001,
                n.cores = NULL, cv.fold = 1)
    best.iter <- gbm.perf(QGB1, method = "OOB")
    QGB1pred <- as.data.frame(predict(QGB1, newdata = Xtestt1, n.trees = best.iter, type = "link"))
    colnames(QGB1pred) <- "Pred"
    pred_t1[i, 1] <- QGB1pred 
    
    QGB2 <- gbm(GDPt2~., data = Xtraint2[,-2], distribution = list(name = "quantile",alpha=0.05), n.trees = 10000, shrinkage = 0.001,
                n.cores = NULL, cv.fold = 1)
    best.iter2 <- gbm.perf(QGB2, method = "OOB")
    QGB2pred <- as.data.frame(predict(QGB2, newdata = Xtestt2, n.trees = best.iter2, type = "link"))
    colnames(QGB2pred) <- "Pred"
    pred_t2[i, 1] <- QGB2pred
    
    predictions <- list("t1" = pred_t1, "t2" = pred_t2)
    
  }
  return(predictions) 
}

# # Java Quantile Neural Network

NNJava <- function(country, window, inoutratio) {
  pred_t1 <- data.frame()
  model_t1 <- list()
  perf_t1 <- list()
  pred_t2 <- data.frame()
  model_t2 <- list()
  perf_t2 <- list()
  h2o.init()
  data_hf <- as.h2o(country[-c(1,5:6)])
  stopifnot(is.h2o(data_hf), dim(data_hf) == dim(country[-c(1,5:6)]))
  for (i in 1:(nrow(country)-window)) {
    model <- h2o.deeplearning(x = 1:10, y = "GDPt1", distribution = "quantile", quantile_alpha = 0.05, seed = 1, 
                              input_dropout_ratio = inoutratio, train_samples_per_iteration = -1, classification_stop = -1, 
                              reproducible = TRUE, training_frame = data_hf[i:window, -3])
    model_t1[i] <- model
    perf <- h2o.performance(model)
    perf_t1[i] <- perf
    
    model2 <- h2o.deeplearning(x = 1:10, y = "GDPt2", distribution = "quantile", quantile_alpha = 0.05, seed = 1, 
                               input_dropout_ratio = inoutratio, train_samples_per_iteration = -1, classification_stop = -1, 
                               reproducible = TRUE, training_frame = data_hf[i:window, -2])
    model_t2[i] <- model2
    perf2 <- h2o.performance(model2)
    perf_t2[i] <- perf2 
    
    window = window + 1
    
    test_hf <- data_hf[window,-(2:3)]
    pred <- h2o.predict(model, newdata = test_hf)
    clean <- as.data.frame(pred)
    pred_t1[i, 1] <- clean
    
    test_hf2 <- data_hf[window,-(2:3)]
    pred2 <- h2o.predict(model2, newdata = test_hf2)
    clean2 <- as.data.frame(pred2)
    pred_t2[i, 1] <- clean2
    
    predictions <- list("t1" = pred_t1, "modelt1" = model_t1, "perft1" = perf_t1, "t2" = pred_t2, "modelt2" = model_t2, "perft2" = perf_t2)
  }
  return(predictions)
}

# # Quantile Neural Network

# Normalization and Denormalization functions needed for the Neural Network

# min_max_norm <- function(x) {
#   (x - min(x)) / (max(x) - min(x))
# }
# 
# denormalize <- function(x,minval,maxval) {
#   x*(maxval-minval) + minval
# }
# 
# QNNmf <- function(country, window) {
#   model_t1 <- list()
#   model_t2 <- list()
#   pred_t1 <- data.frame()
#   pred_t2 <- data.frame()
#   maxs <- apply(country[,-1], 2, max)
#   mins <- apply(country[,-1], 2, min)
#   scaled.data <- as.data.frame(scale(country[,-1], center = mins, scale = maxs - mins))
#   for (i in 1:(nrow(country)-window)) {
#     train <- scaled.data[i:window, -(3:5)]
#     ytrain <- as.matrix(scaled.data[i:window, 2])
#     xtrain <- model.matrix(GDPt1 ~ GDP + NFCI + TS + HP + CG + SV + CR + IGREA, train)[,-1]
#     
#     train2 <-  scaled.data[i:window,]
#     ytrain2 <- as.matrix(scaled.data[i:window, 3])
#     xtrain2 <- model.matrix(GDPt2 ~ GDP + NFCI + TS + HP + CG + SV + CR + IGREA, train2)[,-1]
#     
#     window <- window+1
#     
#     test <- as.matrix(scaled.data[window, -(2:5)])
#     #xtest <- model.matrix(GDPt1 ~ GDP + NFCI + TS + HP + CG + SV + CR + IGREA, test)[,-1]
#     
#     qrnntest <- qrnn.fit(xtrain, ytrain, n.hidden = 1, w=NULL, tau = 0.05, iter.max = 5000, n.trial = 5)
#     model_t1[i] <- qrnntest
#     testest <- qrnn.predict(test, qrnntest)
#     pred_t1[i, 1] <- testest
#     
#     qrnntest2 <- qrnn.fit(xtrain2, ytrain2, n.hidden = 1, w=NULL, tau = 0.05, iter.max = 5000, n.trial = 5)
#     model_t2[i] <- qrnntest2
#     testest2 <- qrnn.predict(test, qrnntest2)
#     pred_t2[i, 1] <- testest2
#     
#     
#     predictions <- list("t1" = pred_t1, "t2" = pred_t2, "modelt1" = model_t1, "modelt2" = model_t2)
#   }
#   denormalize(predictions$t1, mins, maxs)
#   denormalize(predictions$t1, mins, maxs)
#   return(predictions)
# }

### CREATION OF RESULTS ###

# Running all functions #

results <- function(country, start, finish, window, seed, treesforest, shareforest, inoutratio) {
  set.seed(seed)
  Sample <- sample(country, start, finish)
  JavaNN <- NNJava(country, window, inoutratio)
  Bivariate <-  rwrbi(country, window)
  Multivariate <- rwrmulti(country, window)
  Lasso <- rwrmultilasso(country, window)
  QRF <- rwrquantregforest(country, window, treesforest, shareforest)
  Gradient <- QGBmf(country, window)
  # QNN <- QNNmf(country, window)
  Results <- list("Sample" = Sample, "Bivariate" = Bivariate, "Multivariate" = Multivariate, "Lasso" = Lasso,
                  "QRF" = QRF, "GB" = Gradient, "JavaNN" = JavaNN)
  return(Results)
}

USAresults <- results(USA, 1, 44, 44, 1, 5000, 1/3, 0.2)
Japanresults <- results(Japan, 1, 44, 44, 1, 5000, 1/3, 0.2)
Germanyresults <- results(Germany, 1, 44, 44, 1, 5000, 1/3, 0.2)

# Implement a function for the MZ Test
MZ.quant.test <- function(pred, real, tau=0.05){
  MZ.QR.fit <- quantreg::rq(real~pred, tau=tau) 
  # se="boot" uses a bootstrap for the covariance (for simplicity)
  Summary.MZ.QR.fit <- summary(MZ.QR.fit, covariance=TRUE, se="boot")
  
  W <- t(coef(MZ.QR.fit) - c(0,1)) %*% solve(Summary.MZ.QR.fit$cov) %*% (coef(MZ.QR.fit) - c(0,1))
  pval <- 1- pchisq(W,2) %>% round(3)
  
  # Add a respective plot
  p <- ggplot(data=data.frame(real=real, pred=pred), aes(x=pred, y=real)) +
    geom_abline(slope=1, intercept=0, col="black") +
    geom_point(color="grey50") +
    geom_quantile(quantiles=tau, color="red") +
    xlab("Forecast") +
    ylab("Realization") 
  return(list(estimate=Summary.MZ.QR.fit, WaldStat=as.numeric(W), Wald.pval=as.numeric(pval), plot=p))
}

# Implement a function for the Tick Score and Score differences #
apl_score <- function(x,y,tau){ 
  (1-tau)*(x-y)*(x>y) + tau*(y-x)*(y>=x) 
}

scorediff <- function(score1, score2) { 
  score_diff <- score1 - score2
  mean <- mean(score_diff) 
  DM.quant <- lm(score_diff~1) %>% coefci(vcov. = NeweyWest) 
  results <- list("mean score diff" = mean, "DM-Test" = DM.quant)
}

# Implementing a country specific list that contains all Score and Differences-in-Scores results and MZ #
scoring <- function(data, seed) {
  set.seed(seed)
  Historical <- data.frame((matrix(0, 132, 1))) 
  colnames(Historical) <- c("HistoricalLoss")
  
  LassoScore = apl_score(data$Lasso$t1$t1, data$Sample$data_test$GDPt1, tau=0.05)
  LassoScoreMean = mean(apl_score(data$Lasso$t1$t1, data$Sample$data_test$GDPt1, tau=0.05))
  LassoMZ = MZ.quant.test(data$Lasso$t1$t1, data$Sample$data_test$GDPt1)
  
  LassoScore2 = apl_score(data$Lasso$t2$t2, data$Sample$data_test$GDPt2, tau=0.05)
  LassoScoreMean2 = mean(apl_score(data$Lasso$t2$t2, data$Sample$data_test$GDPt2, tau=0.05))
  LassoMZ2 = MZ.quant.test(data$Lasso$t2$t2, data$Sample$data_test$GDPt2)
  
  MultiScore = apl_score(data$Multivariate$t1$t1, data$Sample$data_test$GDPt1, tau=0.05)
  MultiScoreMean = mean(apl_score(data$Multivariate$t1$t1, data$Sample$data_test$GDPt1, tau=0.05))
  MultiMZ = MZ.quant.test(data$Multivariate$t1$t1, data$Sample$data_test$GDPt1)
  
  MultiScore2 = apl_score(data$Multivariate$t2$t2, data$Sample$data_test$GDPt2, tau=0.05)
  MultiScoreMean2 = mean(apl_score(data$Multivariate$t2$t2, data$Sample$data_test$GDPt2, tau=0.05))
  MultiMZ2 = MZ.quant.test(data$Multivariate$t2$t2, data$Sample$data_test$GDPt2)
  
  QRFScore = apl_score(data$QRF$t1$V1, data$Sample$data_test$GDPt1, tau=0.05)
  QRFScoreMean = mean(apl_score(data$QRF$t1$V1, data$Sample$data_test$GDPt1, tau=0.05))
  QRFMZ = MZ.quant.test(data$QRF$t1$V1, data$Sample$data_test$GDPt1)
  
  QRFScore2 = apl_score(data$QRF$t2$V1, data$Sample$data_test$GDPt2, tau=0.05)
  QRFScoreMean2 = mean(apl_score(data$QRF$t2$V1, data$Sample$data_test$GDPt2, tau=0.05))
  QRFMZ2 = MZ.quant.test(data$QRF$t2$V1, data$Sample$data_test$GDPt2)
  
  GBScore = apl_score(data$GB$t1$Pred, data$Sample$data_test$GDPt1, tau=0.05)
  GBScoreMean = mean(apl_score(data$GB$t1$Pred, data$Sample$data_test$GDPt1, tau=0.05))
  GBMZ = MZ.quant.test(data$GB$t1$Pred, data$Sample$data_test$GDPt1)
  
  GBScore2 = apl_score(data$GB$t2$Pred, data$Sample$data_test$GDPt2, tau=0.05)
  GBScoreMean2 = mean(apl_score(data$GB$t2$Pred, data$Sample$data_test$GDPt2, tau=0.05))
  GBMZ2 = MZ.quant.test(data$GB$t2$Pred, data$Sample$data_test$GDPt2)
  
  # QNNScore = apl_score(data$QNN$t1$V1, data$Sample$data_test$GDPt1, tau=0.05)
  # QNNScoreMean = mean(apl_score(data$QNN$t1$V1, data$Sample$data_test$GDPt1, tau=0.05))
  # QNNMZ = MZ.quant.test(data$QNN$t1$V1, data$Sample$data_test$GDPt1)
  # 
  # QNNScore2 = apl_score(data$QNN$t2$V1, data$Sample$data_test$GDPt2, tau=0.05)
  # QNNScoreMean2 = mean(apl_score(data$QNN$t2$V1, data$Sample$data_test$GDPt2, tau=0.05))
  # QNNMZ2 = MZ.quant.test(data$QNN$t2$V1, data$Sample$data_test$GDPt2)
  
  JavaNNScore = apl_score(data$JavaNN$t1$predict, USAresults$Sample$data_test$GDPt1, tau=0.05)
  JavaNNScoreMean = mean(apl_score(data$JavaNN$t1$predict, USAresults$Sample$data_test$GDPt1, tau=0.05))
  JavaNNMZ = MZ.quant.test(data$JavaNN$t1$predict, USAresults$Sample$data_test$GDPt1)
  
  JavaNNScore2 = apl_score(data$JavaNN$t2$predict, USAresults$Sample$data_test$GDPt2, tau=0.05)
  JavaNNScoreMean2 = mean(apl_score(data$JavaNN$t2$predict, USAresults$Sample$data_test$GDPt2, tau=0.05))
  JavaNNMZ2 = MZ.quant.test(data$JavaNN$t2$predict, USAresults$Sample$data_test$GDPt2)
  
  GDPScore = apl_score(data$Bivariate$t1$`based on GDP`, data$Sample$data_test$GDPt1, tau=0.05)
  GDPScoreMean = mean(apl_score(data$Bivariate$t1$`based on GDP`, data$Sample$data_test$GDPt1, tau=0.05))
  GDPMZ = MZ.quant.test(data$Bivariate$t1$`based on GDP`, data$Sample$data_test$GDPt1)
  
  GDPScore2 = apl_score(data$Bivariate$t2$`based on GDP`, data$Sample$data_test$GDPt2, tau=0.05)
  GDPScoreMean2 = mean(apl_score(data$Bivariate$t2$`based on GDP`, data$Sample$data_test$GDPt2, tau=0.05))
  GDPMZ2 = MZ.quant.test(data$Bivariate$t2$`based on GDP`, data$Sample$data_test$GDPt2)
  
  NFCIScore = apl_score(data$Bivariate$t1$`based on NFCI`, data$Sample$data_test$GDPt1, tau=0.05)
  NFCIScoreMean = mean(apl_score(data$Bivariate$t1$`based on NFCI`, data$Sample$data_test$GDPt1, tau=0.05))
  NFCIMZ = MZ.quant.test(data$Bivariate$t1$`based on NFCI`, data$Sample$data_test$GDPt1)
  
  NFCIScore2 = apl_score(data$Bivariate$t2$`based on NFCI`, data$Sample$data_test$GDPt2, tau=0.05)
  NFCIScoreMean2 = mean(apl_score(data$Bivariate$t2$`based on NFCI`, data$Sample$data_test$GDPt2, tau=0.05))
  NFCIMZ2 = MZ.quant.test(data$Bivariate$t2$`based on NFCI`, data$Sample$data_test$GDPt2)
  
  HPScore = apl_score(data$Bivariate$t1$`based on HP`, data$Sample$data_test$GDPt1, tau=0.05)
  HPScoreMean = mean(apl_score(data$Bivariate$t1$`based on HP`, data$Sample$data_test$GDPt1, tau=0.05))
  HPMZ = MZ.quant.test(data$Bivariate$t1$`based on HP`, data$Sample$data_test$GDPt1)
  
  HPScore2 = apl_score(data$Bivariate$t2$`based on HP`, data$Sample$data_test$GDPt2, tau=0.05)
  HPScoreMean2 = mean(apl_score(data$Bivariate$t2$`based on HP`, data$Sample$data_test$GDPt2, tau=0.05))
  HPMZ2 = MZ.quant.test(data$Bivariate$t2$`based on HP`, data$Sample$data_test$GDPt2)
  
  SVScore = apl_score(data$Bivariate$t1$`based on SV`, data$Sample$data_test$GDPt1, tau=0.05)
  SVScoreMean = mean(apl_score(data$Bivariate$t1$`based on SV`, data$Sample$data_test$GDPt1, tau=0.05))
  SVMZ = MZ.quant.test(data$Bivariate$t1$`based on SV`, data$Sample$data_test$GDPt1)
  
  SVScore2 = apl_score(data$Bivariate$t2$`based on SV`, data$Sample$data_test$GDPt2, tau=0.05)
  SVScoreMean2 = mean(apl_score(data$Bivariate$t2$`based on SV`, data$Sample$data_test$GDPt2, tau=0.05))
  SVMZ2 = MZ.quant.test(data$Bivariate$t2$`based on SV`, data$Sample$data_test$GDPt2)
  
  TSScore = apl_score(data$Bivariate$t1$`based on TS`, data$Sample$data_test$GDPt1, tau=0.05)
  TSScoreMean = mean(apl_score(data$Bivariate$t1$`based on TS`, data$Sample$data_test$GDPt1, tau=0.05))
  TSMZ = MZ.quant.test(data$Bivariate$t1$`based on TS`, data$Sample$data_test$GDPt1)
  
  TSScore2 = apl_score(data$Bivariate$t2$`based on TS`, data$Sample$data_test$GDPt2, tau=0.05)
  TSScoreMean2 = mean(apl_score(data$Bivariate$t2$`based on TS`, data$Sample$data_test$GDPt2, tau=0.05))
  TSMZ2 = MZ.quant.test(data$Bivariate$t2$`based on TS`, data$Sample$data_test$GDPt2)
  
  CRScore = apl_score(data$Bivariate$t1$`based on CR`, data$Sample$data_test$GDPt1, tau=0.05)
  CRScoreMean = mean(apl_score(data$Bivariate$t1$`based on CR`, data$Sample$data_test$GDPt1, tau=0.05))
  CRMZ = MZ.quant.test(data$Bivariate$t1$`based on CR`, data$Sample$data_test$GDPt1)
  
  CRScore2 = apl_score(data$Bivariate$t2$`based on CR`, data$Sample$data_test$GDPt2, tau=0.05)
  CRScoreMean2 = mean(apl_score(data$Bivariate$t2$`based on CR`, data$Sample$data_test$GDPt2, tau=0.05))
  CRMZ2 = MZ.quant.test(data$Bivariate$t2$`based on CR`, data$Sample$data_test$GDPt2)
  
  CGScore = apl_score(data$Bivariate$t1$`based on CG`, data$Sample$data_test$GDPt1, tau=0.05)
  CGScoreMean = mean(apl_score(data$Bivariate$t1$`based on CG`, data$Sample$data_test$GDPt1, tau=0.05))
  CGMZ = MZ.quant.test(data$Bivariate$t1$`based on CG`, data$Sample$data_test$GDPt1)
  
  CGScore2 = apl_score(data$Bivariate$t2$`based on CG`, data$Sample$data_test$GDPt2, tau=0.05)
  CGScoreMean2 = mean(apl_score(data$Bivariate$t2$`based on CG`, data$Sample$data_test$GDPt2, tau=0.05))
  CGMZ2 = MZ.quant.test(data$Bivariate$t2$`based on CG`, data$Sample$data_test$GDPt2)
  
  IGREAScore = apl_score(data$Bivariate$t1$`based on IGREA`, data$Sample$data_test$GDPt1, tau=0.05)
  IGREAScoreMean = mean(apl_score(data$Bivariate$t1$`based on IGREA`, data$Sample$data_test$GDPt1, tau=0.05))
  IGREAMZ = MZ.quant.test(data$Bivariate$t1$`based on IGREA`, data$Sample$data_test$GDPt1)
  
  IGREAScore2 = apl_score(data$Bivariate$t2$`based on IGREA`, data$Sample$data_test$GDPt2, tau=0.05)
  IGREAScoreMean2 = mean(apl_score(data$Bivariate$t2$`based on IGREA`, data$Sample$data_test$GDPt2, tau=0.05))
  IGREAMZ2 = MZ.quant.test(data$Bivariate$t2$`based on IGREA`, data$Sample$data_test$GDPt2)
  
  GDPDiff = scorediff(GDPScore, Historical$HistoricalLoss)
  LassoDiff = scorediff(LassoScore, GDPScore)
  MultiDiff = scorediff(MultiScore, GDPScore)
  QRFDiff = scorediff(QRFScore, GDPScore)
  GBDiff = scorediff(GBScore, GDPScore)
  # QNNDiff = scorediff(QNNScore, GDPScore)
  JavaNNDiff = scorediff(JavaNNScore, GDPScore)
  NFCIDiff = scorediff(NFCIScore, GDPScore)
  HPDiff = scorediff(HPScore, GDPScore)
  SVDiff = scorediff(SVScore, GDPScore)
  TSDiff = scorediff(TSScore, GDPScore)
  CRDiff = scorediff(CRScore, GDPScore)
  CGDiff = scorediff(CGScore, GDPScore)
  IGREADiff = scorediff(IGREAScore, GDPScore)
  
  Lasso = list("Score" = LassoScore, "MeanScore" = LassoScoreMean, "ScoreDifference" = LassoDiff, "MZ" = LassoMZ)
  Multi = list("Score" = MultiScore, "MeanScore" = MultiScoreMean, "ScoreDifference" = MultiDiff, "MZ" = MultiMZ)
  QRF = list("Score" = QRFScore, "MeanScore" = QRFScoreMean, "ScoreDifference" = QRFDiff, "MZ" = QRFMZ)
  GB = list("Score" = GBScore, "MeanScore" = GBScoreMean, "ScoreDifference" = GBDiff, "MZ" = GBMZ)
  # QNN = list("score" = QNNScore, "MeanScore" = QNNScoreMean, "ScoreDifference" = QNNDiff, "MZ" = QNNMZ)
  JavaNN = list("score" = JavaNNScore, "MeanScore" = JavaNNScoreMean, "ScoreDifference" = JavaNNDiff, "MZ" = JavaNNMZ)
  GDP = list("Score" = GDPScore, "MeanScore" = GDPScoreMean, "Historical Difference" = GDPDiff, "MZ" = GDPMZ)
  NFCI = list("Score" = NFCIScore, "MeanScore" = NFCIScoreMean, "ScoreDifference" = NFCIDiff, "MZ" = NFCIMZ)
  HP = list("Score" = HPScore, "MeanScore" = HPScoreMean, "ScoreDifference" = HPDiff, "MZ" = HPMZ)
  SV = list("Score" = SVScore, "MeanScore" = SVScoreMean, "ScoreDifference" = SVDiff, "MZ" = SVMZ)
  TS = list("Score" = TSScore, "MeanScore" = TSScoreMean, "ScoreDifference" = TSDiff, "MZ" = TSMZ)
  CR = list("Score" = CRScore, "MeanScore" = CRScoreMean, "ScoreDifference" = CRDiff, "MZ" = CRMZ)
  CG = list("Score" = CGScore, "MeanScore" = CGScoreMean, "ScoreDifference" = CGDiff, "MZ" = CGMZ)
  IGREA = list("Score" = IGREAScore, "MeanScore" = IGREAScoreMean, "ScoreDifference" = IGREADiff, "MZ" = IGREAMZ)
  
  results = list("Lasso" = Lasso, "Multi" = Multi, "QRF" = QRF, "GB" = GB, "JavaNN" = JavaNN, "GDP" = GDP, "NFCI" = NFCI,
                 "HP" = HP, "SV" = SV, "TS" = TS, "CR" = CR, "CG" = CG, "IGREA" = IGREA)
  
  GDPDiff2 = scorediff(GDPScore2, Historical$HistoricalLoss)
  LassoDiff2 = scorediff(LassoScore2, GDPScore2)
  MultiDiff2 = scorediff(MultiScore2, GDPScore2)
  QRFDiff2 = scorediff(QRFScore2, GDPScore2)
  GBDiff2 = scorediff(GBScore2, GDPScore2)
  # QNNDiff2 = scorediff(QNNScore2, GDPScore2)
  JavaNNDiff2 = scorediff(JavaNNScore2, GDPScore2)
  NFCIDiff2 = scorediff(NFCIScore2, GDPScore2)
  HPDiff2 = scorediff(HPScore2, GDPScore2)
  SVDiff2 = scorediff(SVScore2, GDPScore2)
  TSDiff2 = scorediff(TSScore2, GDPScore2)
  CRDiff2 = scorediff(CRScore2, GDPScore2)
  CGDiff2 = scorediff(CGScore2, GDPScore2)
  IGREADiff2 = scorediff(IGREAScore2, GDPScore2)
  
  
  Lasso2 = list("Score" = LassoScore2, "MeanScore" = LassoScoreMean2, "ScoreDifference" = LassoDiff2, "MZ" = LassoMZ2)
  Multi2 = list("Score" = MultiScore2, "MeanScore" = MultiScoreMean2, "ScoreDifference" = MultiDiff2, "MZ" = MultiMZ2)
  QRF2 = list("Score" = QRFScore2, "MeanScore" = QRFScoreMean2, "ScoreDifference" = QRFDiff2, "MZ" = QRFMZ2)
  GB2 = list("Score" = GBScore2, "MeanScore" = GBScoreMean2, "ScoreDifference" = GBDiff2, "MZ" = GBMZ2)
  # QNN2 = list("score" = QNNScore2, "MeanScore" = QNNScoreMean2, "ScoreDifference" = QNNDiff2, "MZ" = QNNMZ2)
  JavaNN2 = list("score" = JavaNNScore2, "MeanScore" = JavaNNScoreMean2, "ScoreDifference" = JavaNNDiff2, "MZ" = JavaNNMZ2)
  GDP2 = list("Score" = GDPScore2, "MeanScore" = GDPScoreMean2, "Historical Difference" = GDPDiff2, "MZ" = GDPMZ2)
  NFCI2 = list("Score" = NFCIScore2, "MeanScore" = NFCIScoreMean2, "ScoreDifference" = NFCIDiff2, "MZ" = NFCIMZ2)
  HP2 = list("Score" = HPScore2, "MeanScore" = HPScoreMean2, "ScoreDifference" = HPDiff2, "MZ" = HPMZ2)
  SV2 = list("Score" = SVScore2, "MeanScore" = SVScoreMean2, "ScoreDifference" = SVDiff2, "MZ" = SVMZ2)
  TS2 = list("Score" = TSScore2, "MeanScore" = TSScoreMean2, "ScoreDifference" = TSDiff2, "MZ" = TSMZ2)
  CR2 = list("Score" = CRScore2, "MeanScore" = CRScoreMean2, "ScoreDifference" = CRDiff2, "MZ" = CRMZ2)
  CG2 = list("Score" = CGScore2, "MeanScore" = CGScoreMean2, "ScoreDifference" = CGDiff2, "MZ" = CGMZ2)
  IGREA2 = list("Score" = IGREAScore2, "MeanScore" = IGREAScoreMean2, "ScoreDifference" = IGREADiff2, "MZ" = IGREAMZ2)
  
  results2 = list("Lasso" = Lasso2, "Multi" = Multi2, "QRF" = QRF2, "GB" = GB2, "JavaNN" = JavaNN2, "GDP" = GDP2, "NFCI" = NFCI2,
                 "HP" = HP2, "SV" = SV2, "TS" = TS2, "CR" = CR2, "CG" = CG2, "IGREA" = IGREA2)
  
  final = list("t1" = results, "t2" = results2)
  return(final)
}

USAscores <- scoring(USAresults, 1)
Japanscores <- scoring(Japanresults, 1)
Germanyscores <- scoring(Germanyresults, 1)
