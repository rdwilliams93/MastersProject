# Support vector regression 
library(caret)
library(combinat)
library(hash)
source("script_readData_23052017.R")

data.train$WHITE <- as.numeric(data.train$V1) + as.numeric(data.train$V5)
data.train$BLACK <- as.numeric(data.train$V2) + as.numeric(data.train$V6)
data.test$WHITE <- as.numeric(data.test$V1) + as.numeric(data.test$V5)
data.test$BLACK <- as.numeric(data.test$V2) + as.numeric(data.test$V6)
predVars <- c("sbp.baseline.in.data","sex.num", "AGE", "BMI", "Race.num", "WHITE", "BLACK")
training <- data.train[predVars]
testing <- data.test[predVars]


#Linear Kernel svr function
Linear_model_func <- function(...){ train(x=training[c(...)],
                                          y= data.train$sbp_change,
                                          method = "svmLinear",   # Linear kernel
                                          tuneLength = 9,					# 9 values of the cost function
                                          preProc = c("center","scale"),  # Center and scale data
                                          metric="RMSE",
                                          trControl=trainControl(method='repeatedCV',number = 5, repeats = 100, classProbs = FALSE)
)}
#polynomial kernel svr function
#Poly_model_func <- function(...){ train(x=training[c(...)],
#                                        y= data.train$sbp_change,
#                                        method = "svmPoly",   # Poly kernel
#                                        tuneLength = 9,					# 9 values of the cost function
#                                        preProc = c("center","scale"),  # Center and scale data
#                                        metric="RMSE",
#                                        trControl=trainControl(method='repeatedCV',number = 5, repeats = 100, classProbs = FALSE)
#)}

#Radial kernel svr function
Radial_model_func <- function(...){ train(x=training[c(...)],
                                          y= data.train$sbp_change,
                                          method = "svmRadial",   # Radial kernel
                                          tuneLength = 9,					# 9 values of the cost function
                                          preProc = c("center","scale"),  # Center and scale data
                                          metric="RMSE",
                                          trControl=trainControl(method='repeatedCV',number = 5, repeats = 100, classProbs = FALSE)
)}


#run analysis 
models <- c("lin","rad")


#create all the combinations for all lengths of variables
combos <- hash()
for (i in 1:length(predVars)) {
  temp <- paste("combo", i, sep = '_')
  combos[[temp]] <- combn(predVars, i, simplify = FALSE)
}

#run the analysis
for (cur_key in keys(combos)){
  for (i in combos[[cur_key]]){
    y <- 1
    for (x in 1:length(models)){
      
      if (models[x] == models[1]){temp <- Linear_model_func(i)}
      else if (models[x] == models[2]){temp <- Radial_model_func(i)}
      #else if (models[x] == models[3]){temp <- Poly_model_func(i)}
      #print (i)
      set.seed(345)
      assign(paste(models[x], i,collapse = '_', sep = '_'), temp)
      y <- y + 1
      
    }
  }
}
