#Importing libraries and functions
library(tidyverse)
library(kernlab)
library(foreach)
library(doParallel)
library(mlbench)
library(randomForest)
# source("D:/my_computer/Est_ML_2019/random_svm/random_svr/svr_functions.R")
# source("D:/my_computer/Est_ML_2019/random_svm/random_svr/adaptative_bagging.R")
# source("D:/my_computer/Est_ML_2019/random_svm/random_machines_baseline_code.R")
set.seed(42)

setwd("D:/my_computer/Est_ML_2019/random_svm/bolsa_familia/")
mtry<-trunc(c(0.25,0.5,1,2,4)*trunc(sqrt((ncol(list_data[[1]]$training)-1))),digits = 0)
mtry<-mtry[mtry>=1 & mtry<=ncol(list_data[[1]]$training)]
node_size<-c(5,10,25)
ntree<-c(100,500,1000)




parameters_multivariate_tree<-expand.grid(mtry=mtry,node_size=node_size,ntree=ntree)

parameters_multivariate_tree<-data.frame(mtry=round(sqrt(17)),node_size=5,ntree=1000)


resultado_random_forest_one<-list()
resultado_random_forest_two<-list()
resultado_random_forest_three<-list()
resultado_random_forest_four<-list()

#=====Setting Parameters===============
library(doSNOW)
ntasks <- 30
# pb <- tkProgressBar(max=ntasks)
progress <- function(n) cat(sprintf("task %d is complete\n", n))
opts <- list(progress=progress)
cl <- makeSOCKcluster(7)
registerDoSNOW(cl)

# nrep<-30
train_ratio<-0.7


for (i in 1:trunc((nrow(parameters_multivariate_tree)/4))){
      resultado_random_forest_one[[i]]<-foreach(x=list_data,.packages = c('purrr','dplyr','magrittr','randomForest'),.options.snow=opts) %dopar%{
            set.seed(42)
            randomForest(formula=class~.,#Formula that will be used
                         data=x$training,ntree=parameters_multivariate_tree[i,'ntree'],
                         mtry=parameters_multivariate_tree[i,'mtry'],
                         nodesize=parameters_multivariate_tree[i,'node_size'],importance=TRUE)
      }
      cat("Tuning Round...",i,"\n\n")
}

# extract_grid_min<-function(resultado,list_data){
#       unpacking<-transpose(resultado) %>% modify_depth(1,unlist)
#       predict<-split(unpacking$predicted,ceiling(seq_along(unpacking$predicted)/nrow(list_data[[1]]$test)))
#       
#       RMSE<-map2_dbl(predict,list_data,~RMSE(predicted = .x,observed = .y$test$y))
#       return(mean(RMSE))
# }

predict_random_forest_reg<-function(model,list_data){
      predictions<-purrr::map2(model,list_data,~predict(.x,newdata=.y$test,type='class'))
      acc_rf<-map2_dbl(predictions,list_data,~acc(predicted = .x,observed=.y$test$class))
      mcc_rf<-map2_dbl(predictions,list_data,~mcc(predicted = .x,observed=.y$test$class))
      return(list(acc=acc_rf,mcc=mcc_rf))
}

just_predict<-function(model,list_data){
      predictions<-purrr::map2(model,list_data,~predict(.x,newdata=.y$test,type='class'))
      return(predictions)
}
      
extract_grid_max<-function(resultado,list_data){
      metric_rf<-map(resultado,~predict_random_forest_reg(model =.x,list_data = list_data))
      acc<- purrr::map_dbl(metric_rf,~mean(.x$acc))
      mcc<- purrr::map_dbl(metric_rf,~mean(.x$mcc))
      return(list(acc=acc,mcc=mcc))
}

prediction_rf_one<-map(resultado_random_forest_one,~just_predict(model =.x,list_data = list_data))
save(prediction_rf_one,file="classification/rf/pred_result_rf_pred_100_2_class_one_for_article.Rdata")
save(resultado_random_forest_one,file="classification/rf/result_pred_100_2_class_one_for_article.Rdata")

# save(prediction_rf_one,file="classification/rf/result_rf_pred_100_3_class_one.Rdata")
# save(prediction_rf_one,file="classification/rf/result_rf_pred_100_4_class_one.Rdata")


# save(resultado_random_forest_one,file="NEW_result_class_one_selected")

# rm(resultado_random_forest_one)
# rm(prediction_rf_one)

# Part two
for (i in (trunc(nrow(parameters_multivariate_tree)/4)+1):(2*trunc(nrow(parameters_multivariate_tree)/4))){
      resultado_random_forest_two[[i]]<-foreach(x=list_data,.packages = c('purrr','dplyr','magrittr','randomForest'),.options.snow=opts) %dopar%{
            set.seed(42)
            randomForest(formula=class~.,#Formula that will be used
                         data=x$training,ntree=parameters_multivariate_tree[i,'ntree'],
                         mtry=parameters_multivariate_tree[i,'mtry'],
                         nodesize=parameters_multivariate_tree[i,'node_size'])
      }
      cat("Tuning Round...",i,"\n\n")
}
prediction_rf_two<-map(resultado_random_forest_two,~just_predict(model =.x,list_data = list_data))
# save(prediction_rf_two,file="classification/rf/result_rf_pred_100_2_class_two.Rdata")
# save(prediction_rf_two,file="classification/rf/result_rf_pred_100_3_class_two.Rdata")
# save(prediction_rf_two,file="classification/rf/result_rf_pred_100_4_class_two.Rdata")

rm(resultado_random_forest_two)
rm(prediction_rf_two)

# Part three
for (i in (trunc(nrow(parameters_multivariate_tree)*2/4)+1):(trunc(nrow(parameters_multivariate_tree)*3/4))){
      resultado_random_forest_three[[i]]<-foreach(x=list_data,.packages = c('purrr','dplyr','magrittr','randomForest'),.options.snow=opts) %dopar%{
            set.seed(42)
            randomForest(formula=class~.,#Formula that will be used
                         data=x$training,ntree=parameters_multivariate_tree[i,'ntree'],
                         mtry=parameters_multivariate_tree[i,'mtry'],
                         nodesize=parameters_multivariate_tree[i,'node_size'])
      }
      cat("Tuning Round...",i,"\n\n")
}
prediction_rf_three<-map(resultado_random_forest_three,~just_predict(model =.x,list_data = list_data))
# save(prediction_rf_three,file="classification/rf/result_rf_pred_100_2_class_three.Rdata")
# save(prediction_rf_three,file="classification/rf/result_rf_pred_100_3_class_three.Rdata")
# save(prediction_rf_three,file="classification/rf/result_rf_pred_100_4_class_three.Rdata")

rm(resultado_random_forest_three)
rm(prediction_rf_three)


# Part four
for (i in (trunc(nrow(parameters_multivariate_tree)*3/4)+1):nrow(parameters_multivariate_tree)){
      resultado_random_forest_four[[i]]<-foreach(x=list_data,.packages = c('purrr','dplyr','magrittr','randomForest'),.options.snow=opts) %dopar%{
            set.seed(42)
            randomForest(formula=class~.,#Formula that will be used
                         data=x$training,ntree=parameters_multivariate_tree[i,'ntree'],
                         mtry=parameters_multivariate_tree[i,'mtry'],
                         nodesize=parameters_multivariate_tree[i,'node_size'])
      }
      cat("Tuning Round...",i,"\n\n")
}
prediction_rf_four<-map(resultado_random_forest_four,~just_predict(model =.x,list_data = list_data))
# save(prediction_rf_four,file="classification/rf/result_rf_pred_100_2_class_four.Rdata")
# save(prediction_rf_four,file="classification/rf/result_rf_pred_100_3_class_four.Rdata")
# save(prediction_rf_four,file="classification/rf/result_rf_pred_100_4_class_four.Rdata")

rm(resultado_random_forest_four)
rm(prediction_rf_four)

#Calculating do 
# metric_rf<-map(resultado_random_forest,~predict_random_forest_reg(model =.x,list_data = list_data))
# metric_grid_rf<-extract_grid_max(resultado = resultado_random_forest,list_data = list_data)

      
#Acc
# rf_max_acc_model<-resultado_random_forest[[which.max(metric_grid_rf$acc)]]
# acc_max_rf<-metric_rf[[which.max(metric_grid_rf$acc)]]
# acc_max_index<-which.max(metric_grid_rf$acc)

#

#Formatting the lists
prediction_rf_three[1:11]<-prediction_rf_one
prediction_rf_three[12:22]<-prediction_rf_two[12:22]
prediction_rf_four[1:33]<-prediction_rf_three
      
prediction_rf<-prediction_rf_four

#acc grid
acc_grid<-list()
mcc_grid<-list()
f1_grid<-list()

acc_macro<-function(observed,predicted){
      acc<-numeric()
      class_levels<-levels(observed)
      
      for(i in 1:length(class_levels)){
            aux_observed<-ifelse(observed==class_levels[i],1,-1) %>% as.factor()
            aux_predicted<-ifelse(predicted==class_levels[i],1,-1) %>% as.factor()
            
            cf<-table(aux_observed,aux_predicted)
            acc[i]<-sum(diag(cf))/sum(cf)
      }
      return(mean(acc))
}


mcc_macro<-function(observed,predicted){
      mcc<-numeric()
      class_levels<-levels(observed)
      
      for(i in 1:length(class_levels)){
            aux_observed<-ifelse(observed==class_levels[i],1,-1) %>% as.factor()
            aux_predicted<-ifelse(predicted==class_levels[i],1,-1) %>% as.factor()
            
            cf<-table(aux_observed,aux_predicted)

            TP=cf[1,1]
            TN=cf[2,2]
            FP=cf[2,1]
            FN=cf[1,2]
            mcc[i]<-(TP*TN-FP*FN)/sqrt((TP+FP+1e-5)*(TP+FN+1e-5)*(TN+FP+1e-5)*(TN+FN+1e-5))
      }
      return(mean(mcc))
}

f1_score_macro<-function(observed,predicted){
      f1<-numeric()
      class_levels<-levels(observed)
      
      for(i in 1:length(class_levels)){
            aux_observed<-ifelse(observed==class_levels[i],1,-1) %>% as.factor()
            aux_predicted<-ifelse(predicted==class_levels[i],1,-1) %>% as.factor()
            
            cf<-table(aux_observed,aux_predicted)
            
            precision<-cf[1,1]/(cf[1,1]+cf[2,1])
            recall<-cf[1,1]/(cf[1,1]+cf[1,2])
            f1[i]<-(2*precision*recall)/(precision+recall)
      }
      return(mean(f1))
}

#Calculating the acc
for(i in 1:1){
            acc_grid[[i]]<-map2_dbl(list_data,prediction_rf_one[[i]],~acc_macro(observed = .x$test$class,predicted = .y))
}
# 
mcc_grid<-list()
# #Calculating the acc
for(i in 1:1){
      mcc_grid[[i]]<-map2_dbl(list_data,prediction_rf_one[[i]],~mcc_macro(observed =.x$test$class,predicted = .y ))
}
# 
f1_score<-function(observed,predicted){
      cf<-table(observed,predicted)
      precision<-cf[1,1]/(cf[1,1]+cf[2,1])
      recall<-cf[1,1]/(cf[1,1]+cf[1,2])
      f1<-(2*precision*recall)/(precision+recall)
}
# 
f1_grid<-list()
for(i in 1:1){
      f1_grid[[i]]<-map2_dbl(list_data,prediction_rf_one[[i]],~f1_score_macro(observed =.x$test$class,predicted = .y ))
}





acc_grid %>% map_dbl(mean) %>% which.max %>% acc_grid[[.]] %>% mean
acc_grid %>% map_dbl(mean) %>% which.max %>% acc_grid[[.]] %>% sd

mcc_grid %>% map_dbl(mean) %>% max #%>% parameters_multivariate_tree[.,]
mcc_grid %>% map_dbl(mean) %>% which.max %>% mcc_grid[[.]] %>% sd

f1_grid %>% map_dbl(mean) %>% max
f1_grid %>% map_dbl(mean) %>% which.max %>% f1_grid[[.]] %>% sd

#MCC
# rf_max_mcc_model<-resultado_random_forest[[which.max(metric_grid_rf$mcc)]]
# mcc_max_rf<-metric_rf[[which.max(metric_grid_rf$mcc)]]
# mcc_max_index<-which.max(metric_grid_rf$mcc)
# 
# 
# 

# parameters_multivariate_tree[which.max(acc_max_index),]
# parameters_multivariate_tree[which.max(mcc_max_index),]


# save(prediction_rf,file="classification/rf/result_rf_pred_100_2_class.Rdata")
# save(prediction_rf,file="classification/rf/result_rf_pred_100_3_class.Rdata")
# save(prediction_rf,file="classification/rf/result_rf_pred_100_4_class.Rdata")


# save(parameters_multivariate_tree,file="classification/rf/parameters_multivariate_rf.Rdata")
# 
# #Just see the max of that
# save(metric_grid_rf,file='classification/rf/metric_grid_rf.Rdata')
# #Acc
# save(acc_max_rf,file='classification/rf/acc_max_rf.Rdata')
# save(acc_max_index,file='classification/rf/acc_max_index.Rdata')
# 
# #Mcc
# save(mcc_max_rf,file='classification/rf/mcc_max_rf.Rdata')
# save(mcc_max_index,file='classification/rf/mcc_max_index.Rdata')



metric_final<-list(acc=acc_grid,mcc=mcc_grid,f1=f1_grid)
