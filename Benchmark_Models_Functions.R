#### Functions for Benchmark Models: recosystem, popular, random models ####



library(recosystem)
library(fastmatch)
library(MASS)

######
#Trains model based on opts_tune parameters (found earlier thorugh CV)
#returns: prediction model and evaluation results
CF_latent1 <- function(train  , test , eval_method = mean_rank,
                       opts_tune){
  ##train,test are data.frame triples (user,item,rating).
  ##train,test should contain the full user*item number of rows including
  ##ratings that are missing (0) -- train,test have the same number of rows
  
  #train model and predict
  r <- Reco()
  train_set <- data_memory(user_index = train[,1], 
                           item_index = train[,2],
                           rating = train[,3], index1 = TRUE)
  
  r$train(train_set, opts = c(opts_tune$min,                     
                              niter = 100, nthread = 4)) 
  
  pred <- expand.grid(user = sort(unique(test$user))
                      , item = sort(unique(test$item)) )
  
  test_set <- data_memory(user_index = pred$user, item_index = pred$item, 
                          index1 = TRUE)
  pred$rating <- r$predict(test_set, out_memory())
  
  #evaluate predictions
  ranking_results <- sapply(sort(unique(test$user)), FUN = eval_method, pred = pred, test = test)
  return(list("evaluation_results" = ranking_results,"pred" = pred) )
  
}

####
global_popular <- function(train , test , eval_method = mean_rank,...){
  #Input Form 1
  ##train,test are data.frame triples (user,item,rating).
  ##train,test should contain the full user*item number of rows including
  ##ratings that are missing (0) -- train,test have the same number of rows
  
  #Input Form 2 (more efficient for evaluation)
  ##train still in data.frame triple form
  ##test in matrix form
  
  require(reshape2)
  
  #recommendations made on most globally rated/consumed item 
  train$user <- as.factor(train$user); train$item <- as.factor(train$item)
  item_consumed_r <- sort(tapply(train$rating, INDEX = list(train$item), FUN = function(x){
    mean(x)
  }), decreasing = T) #avg ratings of each item sorted in descending order
  
  item_consumed <- as.numeric(names(item_consumed_r))
  
  pred_matrix <- matrix(nrow = length(unique(train$user)), ncol = length(unique(train$item)))
  for (u in 1:dim(pred_matrix)[1]){
    pred_matrix[u,item_consumed] <- item_consumed_r
    #pred_matrix contains the predicted ratings for each user x item pair
  }
  if(is.data.frame(test) == T){
    pred <- setNames(melt(pred_matrix), c('user','item','rating'))
    #evaluate predictions
    print("Evaluating Predictions")
    ranking_results <- sapply(sort(unique(test$user)), FUN = eval_method, pred = pred, test = test,...)
  }
  else{
    #test is in matrix form. Use more efficient function to compute results
    ranking_results <- eval_method(pred = pred_matrix, test = test)
  }
  return(list("evaluation_results" = ranking_results, "pred_matrix" = pred_matrix) )
}

#####
#### RANDOM PREDICTIONS
random_recom <- function(train = NA , test , eval_method = mean_rank,...){
  #Input Form 1
  ##train,test are data.frame triples (user,item,rating).
  ##train,test should contain the full user*item number of rows including
  ##ratings that are missing (0) -- train,test have the same number of rows
  
  #Input Form 2 (more efficient for evaluation)
  ##train still in data.frame triple form
  ##test in matrix form
  if(is.data.frame(test) == T){nuser <-length(unique(test$item))
                              nitem <-length(unique(test$user)) }
  else{nuser <-dim(test)[1]
      nitem <- dim(test)[2]}
    
  random_ratings <- runif(n = nuser*nitem, 
                          min = 0, max = 4)
  
  pred_matrix <- matrix(random_ratings, nrow =nuser, ncol =nitem)
  pred <- setNames(melt(pred_matrix), c('user','item','rating'))
  
  if(is.data.frame(test) == T){
    #evaluate predictions
    print("evaluating predictions")
    ranking_results <- sapply(sort(unique(test$user)), FUN = eval_method, pred = pred, test = test,...)
    }
  else{
    #test is in matrix form. Use more efficient function to compute results
    ranking_results <- eval_method(pred = pred_matrix, test = test)
  }
  return(list("evaluation_results" = ranking_results,"pred" = pred) )
}



###


##Evaluate model using k-fold CV

kfoldcv <- function(fulldata, k = 5, seed = 123, model , eval_method = mean_rank, type = "random",...){
  set.seed(seed)
  #fulldata is the full user x item ratings matrix
  #model is an R function which takes the trainData as input and does 
  #predictions using testData
  

  if(type == "random"){
    rows <- dim(fulldata)[1]; cols <- dim(fulldata)[2]
    folds <- sample(rep(1:k, length.out = rows*cols))
    foldmat <- matrix(folds, nrow = rows, ncol = cols)
  }
  
  eval_list <- list()
  for(i in 1:k){
    print(paste("Running Block ", i))
    
    testData <- fulldata
    testData[foldmat != i] <- 0 
    trainData <- fulldata
    trainData[foldmat == i] <- 0
    trainData <- setNames(melt(trainData), c('user','item','rating')) #train in triple form
    
    
    fit <- model( train = trainData, test = testData, eval_method = eval_method,...)
    eval_list[[length(eval_list)+1]] <- fit$evaluation_results
  
  }
  
  CV_mat <- matrix(nrow = length(eval_list),ncol = length(eval_list[[1]]))
  for (i in 1:length(eval_list)){
    CV_mat[i,] <- eval_list[[i]]
  }
  
  CV_avg <- apply(CV_mat, MARGIN = 2, FUN = mean, na.rm = T)
  
  return(CV_avg)
  
}

##############################
kfoldcv_old <- function(fulldata, k = 5, seed = 123, model , eval_method = mean_rank, type = "random",...){
  #old function that required fulldata to be data.frame triples form
  set.seed(seed)
  #fulldata is a dataframe with entries of triples (user, item, rating)
  #model is an R function which takes the trainData as input and does 
  #predictions using testData
  
  if(type == "random"){
    size <- nrow(fulldata)
    fulldata <- fulldata[sample(size),] #shuffle the data
    folds <- cut(seq(1,size),breaks=k,labels=FALSE)
  }
  
  eval_list <- list()
  for(i in 1:k){
    print(paste("Running Block ", i))
    testInd <- which(folds == i, arr.ind = T)
    testData <- fulldata[testInd,]
    trainData <- fulldata[-testInd,]
    fit <- model( train = trainData, test = testData, eval_method = eval_method,...)
    eval_list[[length(eval_list)+1]] <- fit$evaluation_results
  }
  
  CV_mat <- matrix(nrow = length(eval_list),ncol = length(eval_list[[1]]))
  for (i in 1:length(eval_list)){
    CV_mat[i,] <- eval_list[[i]]
  }
  
  CV_avg <- apply(CV_mat, MARGIN = 2, FUN = mean, na.rm = T)
  
  return(CV_avg)
  
}
######
kfoldcv.export <- function(fulldata, k = 5, seed = 123, type = "random", ...){
  set.seed(seed)
  #fulldata is a ratings matrix users x items
  #exports data into k training and testing datasets
  #trainData in the form of user x item matrix
  #testData in the form of dataframe triple
  
  if(type == "random"){
    rows <- dim(fulldata)[1]; cols <- dim(fulldata)[2]
    folds <- sample(rep(1:k, length.out = rows*cols))
    foldmat <- matrix(folds, nrow = rows, ncol = cols)
  }
  write.matrix(foldmat, file = "5foldCVindex.txt")
  result <- for(i in 1:k){
    testData <- fulldata
    testData[foldmat != i] <- 0 
    #testData <- setNames(melt(testData), c('user','item','rating')) #test in triple form
    trainData <- fulldata
    trainData[foldmat == i] <- 0
    write.matrix(trainData, file = paste(k,"foldCV","trainData",i,".txt", sep=""))
    write.matrix(testData,file = paste(k,"foldCV","testData",i,".txt", sep="") )
    }

}

##Function to convert dataframe with triples into full ratings matrix

df_to_mat <- function(df_triple){
  #df_triple is a dataframe containing triples: user,item,rating
  df_triple <- df_triple[order(df_triple$item, df_triple$user),]
}



