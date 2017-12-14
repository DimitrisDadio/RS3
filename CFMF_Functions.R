library(reshape2)
library(ggplot2)
library(gridExtra)
library(foreach)
library(doParallel)
library(parallel)

######################
######Evaluation Method through simple cross ranking######
mean_rank <- function(pred, test, user.u = NULL){
  #pred and test are triples both in the form of data_frames OR matrices
  #function checks where the consumed items by user.u (listed in test_triple) 
  ##are ranked in the pred_triple which is the recommendation output
  if(is.data.frame(pred) ==T & is.data.frame(test) == T){
    pred_u <- pred[pred$user == user.u,]
    item_recom <- pred_u[order(pred_u$rating, decreasing = T), "item"]
    test_u <- test[test$user == user.u & test$rating != 0, "item"]
    rawmean <- mean(match(test_u, item_recom), na.rm = T)
    idealmean <- (length(test_u) + 1)/2
    meanrank <- rawmean - idealmean
  }
  
  else{
    #assume pred and test is in matrix form
    print("using matrix form")
    recommended_matrix <- t(apply(pred, MARGIN = 1, FUN = order, decreasing = T)) #ordered recommendation list for each user
    meanrank <- sapply(1:dim(test)[1], FUN = function(u){
      test_u <- as.vector(test[u,])
      test_u <- (1:length(test_u))[test_u!= 0 ]
      item_recom <- as.vector(recommended_matrix[u,])
      rawmean <- mean(match(test_u, item_recom), na.rm = T)
      idealmean <- (length(test_u) + 1)/2
      meanrank <- rawmean - idealmean
    })
  }
  
  return(meanrank)
  
  #Note NaNs are a result of the users in the test set 
  ##having no rated items i.e. all items have rating 0
}


##########

opt_rank <- function(pred, test, user.u){
  #pred and test are triples both in the form of data_frames OR matrices
  #optimistic rank: similar to eval_ranking but we take 
  #the first(highest) ranked success in the recommendation list
  if(is.data.frame(pred) ==T & is.data.frame(test) == T){
    pred_u <- pred[pred$user == user.u,]
    item_recom <- pred_u[order(pred_u$rating, decreasing = T), "item"]
    test_u <- test[test$user == user.u & test$rating != 0, "item"]
    
    ranks <- match(test_u,item_recom)
    
    if (all(is.na(ranks)) == T){ return(NA) }
    else{ min(ranks, na.rm = T) } 
  }
  
  else{
    #assume pred and test is in matrix form
    print("using matrix form")
    recommended_matrix <- t(apply(pred, MARGIN = 1, FUN = order, decreasing = T)) #ordered recommendation list for each user
    
    optrank<- sapply(1:dim(test)[1], FUN = function(u){
      test_u <- as.vector(test[u,])
      test_u <- (1:length(test_u))[test_u!= 0 ]
      item_recom <- as.vector(recommended_matrix[u,])
      ranks <- match(test_u, item_recom)
      if (all(is.na(ranks)) == T){ return(NA) }
      else{ min(ranks, na.rm = T) } 
      
    })
    return(optrank)
    
  }
  #Note we drop Inf values - a result of no matches in the recom lists
  #having no rated items i.e. all items have rating 0
  
}


hit_ratio <- function(pred, test, user.u, topN = 100){
  #pred and test are triples both in the form of data_frames OR matrices
  #HR measures whether the ground truth item is present on the ranked list
  #topN is the top number of items of the recommended list that is kept for evaluation
  
  if(is.data.frame(pred) ==T & is.data.frame(test) == T){
    pred_u <- pred[pred$user == user.u,]
    item_recom <- pred_u[order(pred_u$rating, decreasing = T), "item"]
    test_u <- test[test$user == user.u & test$rating != 0, "item"]
    
    HR <- mean(test_u %in% (item_recom[1:topN]))
  }
  else{
    #assume pred and test is in matrix form
    print("using matrix form")
    recommended_matrix <- t(apply(pred, MARGIN = 1, FUN = order, decreasing = T)) #ordered recommendation list for each user
    HR <- sapply(1:dim(test)[1], FUN = function(u){
      test_u <- as.vector(test[u,])
      test_u <- (1:length(test_u))[test_u!= 0 ] #items in test set
      item_recom <- as.vector(recommended_matrix[u,])
      mean(test_u %in% (item_recom[1:topN]))
  
    })
  }
  
  return(HR)
}

DCG <- function(y) y[1] + sum(y[-1]/log(1+(2:length(y)), base = 2)) #used to compute nDCG below

nDCG <- function(pred, test, user.u, topN = 100){
  #pred and test are triples both in the form of data_frames OR matrices
  #nDCG: Normalised discounted cumulative gain (well known measure)
  #accounts for the position of the ground truth items on the ranked list
  
  if(is.data.frame(pred) ==T & is.data.frame(test) == T){
    pred_u <- pred[pred$user == user.u,]
    item_recom <- pred_u[order(pred_u$rating, decreasing = T), "item"]
    item_recom <- item_recom[1:topN]
    test_u <- test[test$user == user.u & test$rating != 0, "item"]
    
    rel <- as.integer(item_recom %in% test_u)
    #ideal_rel <- rev(sort(rel))
    ideal_rel <- rep(1, length.out = length(test_u))
    ndcg <- DCG(rel)/DCG(ideal_rel)
    
  }
  else{
    #assume pred and test is in matrix form
    print("using matrix form")
    recommended_matrix <- t(apply(pred, MARGIN = 1, FUN = order, decreasing = T)) #ordered recommendation list for each user
    ndcg <- sapply(1:dim(test)[1], FUN = function(u){
      test_u <- as.vector(test[u,])
      test_u <- (1:length(test_u))[test_u!= 0 ]  #items in test set
      item_recom <- as.vector(recommended_matrix[u,])[1:topN]
      
      rel <- as.integer(item_recom %in% test_u)
      ideal_rel <- rep(1, length.out = length(test_u))
      ndcg <- DCG(rel)/DCG(ideal_rel)
    })
  }
  return(ndcg)
  #if hit ratio = 0, nDCG will naturally return NaN because rel and ideal_rev are
  #vectors of 0s
}

lift_index <- function(pred, test, user.u, topN = 100, n_deciles = 10){
  #pred and test are triples both in the form of data_frames OR matrices
  #n_deciles default 10: assumes the ranked list is divided into 10 equal deciles
  #lift_index uses a linear reduction factor
  #accounts for the position of the ground truth items on the ranked list
  
  if(is.data.frame(pred) ==T & is.data.frame(test) == T){
    pred_u <- pred[pred$user == user.u,]
    item_recom <- pred_u[order(pred_u$rating, decreasing = T), "item"]
    item_recom <- item_recom[1:topN]
    test_u <- test[test$user == user.u & test$rating != 0, "item"]
    
    hr <- as.integer(item_recom %in% test_u)
    if(sum(hr) != 0){
      weight <- seq(to = 1/n_deciles,from = 1,length.out = n_deciles)
      nmembers <- ceiling(length(item_recom)/n_deciles)
      hr_split <- split(hr, ceiling(seq_along(hr)/nmembers))
      lift_index <- sum(sapply(1:n_deciles, FUN = function(x){
        sum(weight[x] * hr_split[[x]])
      } ))/sum(hr)
    }
    else{lift_index <- 0}
  }
  else{
    #assume pred and test is in matrix form
    print("using matrix form")
    recommended_matrix <- t(apply(pred, MARGIN = 1, FUN = order, decreasing = T)) #ordered recommendation list for each user
    lift_index <- sapply(1:dim(test)[1], FUN = function(u){
      test_u <- as.vector(test[u,])
      test_u <- (1:length(test_u))[test_u!= 0 ]
      item_recom <- as.vector(recommended_matrix[u,])[1:topN]
      
      hr <- as.integer(item_recom %in% test_u)
      if(sum(hr) != 0){
        weight <- seq(to = 1/n_deciles,from = 1,length.out = n_deciles)
        nmembers <- ceiling(length(item_recom)/n_deciles)
        hr_split <- split(hr, ceiling(seq_along(hr)/nmembers))
        lift_index <- sum(sapply(1:n_deciles, FUN = function(x){
          sum(weight[x] * hr_split[[x]])
        } ))/sum(hr)
      }
      else{lift_index <- 0}
    })
  }
  return(lift_index)
}


###########

AUC_user <- function(pred, test, user.u, topN = 100){
  #AUC: Area Under ROC (receiver operating characteristic) Curve
  #pred and test are triples in the form of data_frames.
  #function computes the components of the famous AUC metric per user
  #Take mean across all users to compute AUT
  
  pred_u <- pred[pred$user == user.u,]
  item_recom <- pred_u[order(pred_u$rating, decreasing = T), "item"]
  item_recom <- item_recom[1:topN]
  test_u <- test[test$user == user.u & test$rating != 0, "item"]
  
  if (length(test_u) > 0 ){
    rank_gooditems <- match(test_u, item_recom)
    n_baditems <- topN - length(test_u) #number of irrelevant items
    auc_user <- c()
    for(ranki in rank_gooditems){
      auc_user <- c(auc_user, (topN - ranki)/n_baditems) #fraction of bad items ranked below good item
    }
    auc_user <- mean(auc_user) #mean across all the good items
  }
  if (length(test_u) ==0){auc_user <- 0.5}
  else( auc_user <- 0 )#no good items are in the topN recommendation

  return(auc_user)
  
  #Note NaNs are a result of the users in the test set 
  ##having no rated items i.e. all items have rating 0
}


#####################
svdCF <- function(train,test,f,eval_method = loss_fn, lambda = 1){
  full_SVD <- svd(train)
  fit <- matreduce_svd(full_SVD = full_SVD, f = f)
  loss <- eval_method(test_mat = test, fit_SVD = fit,lambda = lambda)
  return(list("loss" = loss, "lambda" = lambda, "fit" = fit))
}

##########

matreduce_svd <- function(full_SVD, f){
  #full_SVD is the output of base function svd()
  #f is the desired number of singular values, singular vectors ,
  #i.e. number of factors
  
  total_eig <- length(full_SVD$d)
  #dprime <- diag(c(full_SVD$d[1:f],rep(0,total_eig-f)))
  
  Xu <- full_SVD$u[,1:f] %*% sqrt( diag(full_SVD$d[1:f]))
  Yi <- full_SVD$v[,1:f] %*% sqrt( diag(full_SVD$d[1:f]))
  
  reduced <- Xu %*% t(Yi)
  
  return(list("reduced" = reduced, "Xu" = Xu, "Yi"= Yi))
}


#####

loss_fn <- function(test_mat, fit_SVD, lambda, confidence = F, conf_mat = NULL ){
  #fit_SVD: output object from matreduce_svd() or simply list(reduced,Xu,Yi)
  #reduced: fitted matrix
  #test_mat: test data in matrix form (usually the same full matrix the SVD is computed with)  
  #conf_mat: confidence matrix - output from confidence()
  if(confidence == F){
    loss <- sum((test_mat-fit_SVD$reduced)^2) + lambda*(sum((fit_SVD$Xu)^2) + sum((fit_SVD$Yi)^2))
  }
  if(confidence == T){
    loss <- sum(conf_mat*(test_mat-fit_SVD$reduced)^2) + lambda*(sum((fit_SVD$Xu)^2) + sum((fit_SVD$Yi)^2))
  }
  return(loss)
}


confidence <- function(imp_ratings, conf_params = list(alpha = 40, type = "log", eps = 4)){
  #function calculated confidence measure in observing preferences 1 or 0
  #imp_ratings is the full implicit ratings matrix (user x item)
  if (conf_params$type == "linear"){conf_mat <- 1 + conf_params$alpha*imp_ratings }
  if (conf_params$type == "log"){conf_mat <- 1 + conf_params$alpha*log(1 + imp_ratings/conf_params$eps)}
  return(conf_mat)
}

conf_user <- function(conf_mat){
  #conf_mat input is the full user x item matrix output calculated from confidence()
  nusers <- dim(conf_mat)[1]; nitems <- dim(conf_mat)[2]
  lapply(1:nusers, FUN = function(u){
    diag(conf_mat[u,])
  })
}

conf_item <- function(conf_mat){
  #conf_mat input is the full user x item matrix output calculated from confidence()
  nusers <- dim(conf_mat)[1]; nitems <- dim(conf_mat)[2]
  lapply(1:nitems, FUN = function(i){
    diag(conf_mat[,i])
  })
}

CFMF <- function(train,f, loss_fn = loss_fn, lambda = 1, iter =15, confidence = F, imp_mat = NULL, ncoresB = 1,
                 conf_params = list(alpha = 40, type = "log", eps = 4)){
  #collab filtering matrix factorisation (with lambda as penalty coefficient)
  #train: preference ratings matrix: users x items
  #iter: number of iterations to minimise cost function factoring in lambda
  #imp_mat: implicit ratings matrix required if confidence == T
  
  full_SVD <- svd(train)
  fit <- matreduce_svd(full_SVD = full_SVD, f = f)
  Yi <- fit$Yi; Xu <- fit$Xu
  
  if (iter == 0){
    print("iter = 0: skipping minimisation with respect to lambda")
  }
  
  if(confidence == F){ #recompute to minimise cost function
    for (i in 1:iter){
      print(paste("running iter ", i))
      #recompute user-factor X 
      Xu <-  t(solve( t(Yi)%*%Yi + lambda*diag(1, nrow = f, ncol = f) ) %*% t(Yi) %*% t(train)) 
      #recomputeitem-factor Y
      Yi <- t(solve( t(Xu)%*% Xu + lambda*diag(1, nrow = f, ncol = f) ) %*% t(Xu) %*% train )
    }
    predmat <- Xu %*% t(Yi)
    fit <- list("reduced" = predmat, "Xu"= Xu, "Yi" = Yi)
    loss <- loss_fn(test_mat = train, fit_SVD = fit , lambda = lambda, confidence = F)
    }
  
  
  if(confidence == T){ #recompute using confidence measures
    print("Computing using confidence measures"); print(unlist(conf_params))
    conf_mat <- confidence(imp_mat, conf_params = conf_params )
    C_user <- conf_user(conf_mat)
    C_item <- conf_item(conf_mat)
    nuser <- dim(conf_mat)[1] ; nitem <- dim(conf_mat)[2]
    for (i in 1:iter){
      print(paste("iter run: ", i))
      cl <- makeCluster(ncoresB, outfile = "")
      #recompute user-factor X 
      tY <- t(Yi)
      tYY <- tY %*% Yi #f x f matrix
      Xu <- t(parSapply(cl,1:nuser, FUN = function(u){
        print(u)
        solve(tYY + tY %*% (C_user[[u]] - diag(1, nrow = nitem, ncol = nitem) ) %*% Yi + lambda*diag(1, nrow = f, ncol = f)) %*%
          tY %*% C_user[[u]] %*% train[u,]

      }))
      stopCluster(cl)
      cl <- makeCluster(ncoresB, outfile = "")
      #recomputeitem-factor Y
      tX <- t(Xu)
      tXX <- tX %*% Xu
      Yi <- t(parSapply(cl,1:nitem, FUN = function(i){
        print(i)
        solve(tXX + tX %*% (C_item[[i]] - diag(1, nrow = nuser, ncol = nuser) ) %*% Xu + lambda*diag(1, nrow = f, ncol = f)) %*%
          tX %*% C_item[[i]] %*% train[,i]

      }))
      stopCluster(cl)
      
      #predmat <- Xu %*% t(Yi)
      #fit <- list("reduced" = predmat, "Xu"= Xu, "Yi" = Yi)
      #loss <- loss_fn(test_mat = train, fit_SVD = fit , lambda = lambda, confidence = T, conf_mat = conf_mat)
      #loss_values <- c(loss_values, loss)
    }
    predmat <- Xu %*% t(Yi)
    fit <- list("reduced" = predmat, "Xu"= Xu, "Yi" = Yi)
    loss <- loss_fn(test_mat = train, fit_SVD = fit , lambda = lambda, confidence = T, conf_mat = conf_mat)
  }
  
  return(list("loss" = loss, "lambda" = lambda, "fit" = fit))
}




rank.CFMF <- function(train  , test , f = 50, lambda = 0.1, eval_method = mean_rank,iter=15, confidence = F, imp_mat = NULL,...){
  #train is a ratings matrix: user x item
  #test is a data.frame triples (user,item,rating) 
  #This function evaluates the predictions according to e.g. mean ranking or optimistic ranking
  CFMF_fit <- CFMF(train , f , loss_fn, lambda, iter = iter , confidence = confidence, imp_mat = imp_mat)
  pred <- setNames(melt(CFMF_fit$fit$reduced), c('user','item','rating'))
  print("evaluating ranking")
  ranking_results <- sapply(sort(unique(pred$user)), FUN = eval_method, pred = pred, test = test, ...)
  return(list("evaluation_results" = ranking_results,"pred" = pred, "CFMF_fit" = CFMF_fit) )
  
}

optimise.CFMF <- function(fulldata, k = 5, seed = 123, f ,lambda = c(0.1,0.5,1,2),iter=10, ncores = 2 , confidence = F, 
                          imp_mat = NULL, ncoresB = 1,...){
  set.seed(seed)
  #optimises lambda penalisation coefficient through RMSE 
  #fulldata is a ratings matrix users x items (typically preference matrix - 1,0) 
  rmse_mean <- c()
  for(lambdavalue in lambda){
    rows <- dim(fulldata)[1]; cols <- dim(fulldata)[2]
    folds <- sample(rep(1:k, length.out = rows*cols))
    foldmat <- matrix(folds, nrow = rows, ncol = cols)
  
    registerDoParallel(cores = ncores) #parallel
    result <- foreach(i = 1:k, .combine = rbind.data.frame, .packages = "reshape2", 
                      .export = c("rank.CFMF", "CFMF", "matreduce_svd", "loss_fn")) %dopar% {
                        testData <- fulldata
                        testData[foldmat != i] <- 0 
                        trainData <- fulldata
                        trainData[foldmat == i] <- 0
                        implicitMat <- imp_mat
                        implicitMat[foldmat == i] <- 0
                        
                        CFMF <- CFMF( train = trainData, f = f , loss_fn =loss_fn,  lambda= lambdavalue,
                                      iter = iter, confidence = confidence, imp_mat = implicitMat,
                                      ncoresB = ncoresB )$fit
                        rmse <- sqrt(mean(((testData - CFMF$reduced)[foldmat == i])^2))
                        cbind.data.frame(chain = i,rmse)
                        
                      }
    print(paste("rmse for lambda = ", lambdavalue, "is ", mean(result$rmse)))
    rmse_mean <- c(rmse_mean, mean(result$rmse))
  }
  min_rmse <- min(rmse_mean)
  lambda.opt <- lambda[rmse_mean == min_rmse]
  output <- list("rmse" = rmse_mean, "lambda" = lambda, "min_rmse" = min_rmse, "lambda.opt" = lambda.opt)
  
  return(output)
}



######
kfoldcv.CFMF <- function(fulldata, k = 5, seed = 123, f ,lambda = 0.1,iter=10, eval_method = mean_rank,
                         type = "random", ncores = 5, confidence = F, imp_mat = NULL, ...){
  set.seed(seed)
  #fulldata is a ratings matrix users x items 
  if(type == "random"){
    rows <- dim(fulldata)[1]; cols <- dim(fulldata)[2]
    folds <- sample(rep(1:k, length.out = rows*cols))
    foldmat <- matrix(folds, nrow = rows, ncol = cols)
  }
  registerDoParallel(cores = ncores) #parallel
  result <- foreach(i = 1:k, .combine = rbind.data.frame, .packages = "reshape2", 
                    .export = c("rank.CFMF", "CFMF", "matreduce_svd", "loss_fn", "mean_rank")) %dopar% {
    testData <- fulldata
    testData[foldmat != i] <- 0 
    testData <- setNames(melt(testData), c('user','item','rating')) #test in triple form
    trainData <- fulldata
    trainData[foldmat == i] <- 0
    implicitMat <- imp_mat
    implicitMat[foldmat == i] <- 0
    
    fit <- rank.CFMF( train = trainData, test = testData, f = f, lambda = lambda, eval_method = eval_method,
                      iter = iter, confidence = confidence, imp_mat = implicitMat,...)
    cbind.data.frame(chain = i,fit$evaluation_results)
    }
  CV_avg <- tapply(result$`fit$evaluation_results`, INDEX = rep(1:rows,k), FUN = mean, na.rm = T)
  
  return(CV_avg)
}


