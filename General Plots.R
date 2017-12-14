###########NOTE TO SELF##############
#Group final recommendations together by proximity of predicted scores
#Weight relevance of final recommendations e.g. through frequency of consumption
#####################################
library(plotly)
library(ggplot2)

setwd("/Users/jeremyseow/Documents/OneDrive/ETH/Thesis inCube/Scripts/") #mac
#setwd("C:/Jeremy Seow/OneDrive/ETH/Thesis inCube/Scripts/") #win
source("Benchmark_Models_Functions.R")
source("CFMF_Functions.R")
source("plot_fn.R")


#########Pre-Process/Import Data#########
###########Prepare Full Data into Ratings matrix########### 
#(Don't split data into training/testing yet)
df.data <- read.csv("Italian Dataset/paperdata.csv")
df.data$id_client <- as.factor(df.data$id_client)
levels(df.data$id_client) <- 1:length(levels(df.data$id_client))
df.data$id_product <- as.factor(df.data$id_product)
levels(df.data$id_product) <- 1:length(levels(df.data$id_product))
rmatrix <- as.matrix(table(df.data$id_client, df.data$id_product)) #ratings matrix

pmatrix <- rmatrix #preferences matrix coded in 1,0 only
pmatrix[pmatrix != 0] <- 1

#Convert full ratings matrix to implicit triple form: user, item, rating (here rating is implicit)
rtriple <- setNames(melt(rmatrix), c('user','item','rating'))
rtriple[rtriple$rating != 0, "rating"] = 1 #comment on/off depending on binary encoding preferred.


###########
#Visualise full pmatrix (preferences are binary)
image(1:ncol(pmatrix), 1:nrow(pmatrix), t(pmatrix), col = c("black","white"), axes = F,
      xlab ="",ylab = "", srt=45)
mtext("Item", side = 3, line = 3)
mtext("User", side = 2, line = 3)
axis(3, at = 1:ncol(pmatrix), labels = colnames(pmatrix), srt = 45, tick = F)
axis(2, at = 1:nrow(pmatrix), labels = rownames(pmatrix), srt=45, tick = F)
###########


#######
#Shows that the cost function (with the confidence levels) are minimised over 10 iterations

load("Euler Core/Saved From Core/Archived/Check Loss 01.Rdata")
str(checkloss)
plot(checkloss$loss_values)

p1 <- ggplot(data.frame("cost" = checkloss$loss_values, "iteration" = 1:15 ), aes(iteration, cost)) +
  geom_point() + geom_smooth(method = "lm", aes(color = "red"), formula = (y~exp(-x))) +
  ggtitle("Cost Function Minimised") + 
  theme(plot.title = element_text(hjust = 0.5)) ; p1

#########

#Checks the optimal penalisation lambda parameter for 2nd confidence model
load("Output/CFMF Confidence/optimise_CFMF_result2.Rdata")
str(opt_result2)
opt_result2$text
plot(opt_result2$opt_result$lambda, opt_result2$opt_result$rmse, ylab = "rmse", xlab = "lambda", main = "Optimal Lambda for Regularisation")
opt_result2$opt_result$lambda[opt_result2$opt_result$rmse == min(opt_result2$opt_result$rmse)]
p2 <- ggplot(opt_result2$opt_result,  aes(lambda,rmse)) + geom_point() +
  ggtitle("CFMF with Confidence Lvl - Search for Best Lambda") + 
  theme(plot.title = element_text(hjust = 0.5)) ; p2

######################################################################
###############Evaluations of CFMF with Confidence ###################
######################################################################

#Uses the optimal penalisation lamba parameter and evaluates 2nd confidence model using 5 fold CV

load("Output/CFMF Confidence/CV_CFMFconf2_meanrank.Rdata")
plot_conf2_1 <- plot.evaluate(CV_CFMFconf2_meanrank$meanrank, title_txt = "Mean Rankings - Confidence Weighted"); plot_conf2_1

load("Output/CFMF Confidence/CV_CFMFconf2_optrank.Rdata")
plot_conf2_2 <- plot.evaluate(CV_CFMFconf2_optrank$optrank, title_txt = "Optimistic Rankings - Confidence Weighted") ; plot_conf2_2


load("Output/CFMF Confidence/CV_CFMFconf2_hitratio.Rdata")
plot_conf2_3 <- plot.evaluate(CV_CFMFconf2_hitratio$hitratio, title_txt = "Hit Ratio - Confidence Weighted", 
                              xlab = "Hit Ratio", binwdth= 0.02) ; plot_conf2_3

load("Output/CFMF Confidence/CV_CFMFconf2_nDCG.Rdata")
plot_conf2_4 <- plot.evaluate(CV_CFMFconf2_nDCG$nDCG, title_txt = "nDCG - Confidence Weighted", xlab = "Hit Ratio", 
                              binwdth= 0.02); plot_conf2_4

load("Output/CFMF Confidence/CV_CFMFconf2_liftindex.Rdata")
plot_conf2_5 <- plot.evaluate(CV_CFMFconf2_liftindex$liftindex, title_txt = "Lift Index - Confidence Weighted", xlab = "Hit Ratio", 
                              binwdth= 0.02) ; plot_conf2_5

multiplot(plot_conf2_1, plot_conf2_2, plot_conf2_3, plot_conf2_4,plot_conf2_5, cols=2)

#########

######################################################################
###############CFMF with Confidence 3rd Model #### ###################
######################################################################
#Checks the optimal penalisation lambda parameter for 3rd confidence model

load("Output/CFMF Confidence/optimise_CFMF_result3.Rdata")
plot(opt_result3$lambda, opt_result3$rmse)
opt_result3$lambda[opt_result3$rmse == min(opt_result3$rmse)]



######################################################################
###############CFMF with Confidence 4th Model #### ###################
######################################################################
#Checks the optimal penalisation lambda parameter for 3rd confidence model
load("Output/CFMF Confidence/optimise_CFMF_result4.Rdata")

plot(rmse ~ lambda, data=opt_result4$opt_result)
#optimal lambda
opt_result4$opt_result$lambda[opt_result4$opt_result$rmse == min(opt_result4$opt_result$rmse)]



##### Evaluation of 4th Confidence Model - using lambda = 27
#results computed by repeating 5-fold CV 10 times

load("Output/CFMF Confidence/CV_CFMFconf4_meanrank.Rdata")
plot_conf4_1 <- plot.evaluate(CV_CFMFconf4_meanrank$meanrank, title_txt = "Mean Rankings - Confidence Weighted"); plot_conf4_1

load("Output/CFMF Confidence/CV_CFMFconf4_nDCG.Rdata")
plot_conf4_4 <- plot.evaluate(CV_CFMFconf4_nDCG$nDCG, title_txt = "nDCG - Confidence Weighted", xlab = "Hit Ratio", 
                              binwdth= 0.02); plot_conf4_4






######################################################################
############# Evaluations of CFMF without Confidence #################
######################################################################
# Results below are generated by aggregating 10 runs of 5-fold CV.
# Results have been checked that they stabilise after with 10 runs.
# Seeds used: 123, 500, 777, 999, 1000, 1123, 1500, 1777, 1999, 2000

#Check the optimal svd model without confidence measures
load("Output/CFMF Simple/CV_CFMFsimple_meanrank.Rdata")
plot_CFMFs_1 <- plot.evaluate(CV_CFMFsimple_meanrank$simple_meanrank, title_txt = "Mean Rankings - CFMF Simple") ; plot_CFMFs_1

load("Output/CFMF Simple/CV_CFMFsimple_optrank.Rdata")
plot_CFMFs_2 <- plot.evaluate(CV_CFMFsimple_optrank$simple_optrank, title_txt = "Optimistic Rankings - CFMF Simple") ; plot_CFMFs_2



load("Output/CFMF Simple/CV_CFMFsimple_hitratio.Rdata")
plot_CFMFs_3 <- plot.evaluate(CV_CFMFsimple_hitratio$simple_hitratio, title_txt = "Hit Ratio - CFMF Simple",
                              xlab = "Hit Ratio", binwdth = 0.02) ; plot_CFMFs_3


load("Output/CFMF Simple/CV_CFMFsimple_ndcg.Rdata")
plot_CFMFs_4 <- plot.evaluate(CV_CFMFsimple_ndcg$simple_ndcg, title_txt = "nDCG - CFMF Simple",
                              xlab = "nDCG", binwdth = 0.02) ; plot_CFMFs_4

load("Output/CFMF Simple/CV_CFMFsimple_lift.Rdata")
plot_CFMFs_5 <- plot.evaluate(CV_CFMFsimple_lift$simple_lift, title_txt = "Lift Index - CFMF Simple",
                              xlab = "Lift Index", binwdth = 0.02) ; plot_CFMFs_5

multiplot(plot_CFMFs_1, plot_CFMFs_2, plot_CFMFs_3, plot_CFMFs_4,plot_CFMFs_5, cols=2)


#CV_CFMFsimple_meanrank <- kfoldcv.CFMF(pmatrix, k = 5, seed = 123, lambda = 13.5,f = 50, eval_method = mean_rank, type = "random",iter = 15, ncores = 8)
#CV_CFMFsimple_optrank <- kfoldcv.CFMF(pmatrix, k = 5, seed = 123, lambda = 13.5,f = 50, eval_method = opt_rank, type = "random",iter = 15, ncores = 8)
#CV_CFMFsimple_hitratio <- kfoldcv.CFMF(pmatrix, k = 5, seed = 123, lambda = 13.5,f = 50, eval_method = hit_ratio, 
#topN = 100, type = "random",iter = 15, ncores = 8)
#CV_CFMFsimple_ndcg <- kfoldcv.CFMF(pmatrix, k = 5, seed = 123, lambda = 13.5,f = 50, eval_method = nDCG, 
                                  # topN = 100, type = "random",iter = 15, ncores = 8)
#CV_CFMFsimple_lift <- kfoldcv.CFMF(pmatrix, k = 5, seed = 123, lambda = 13.5,f = 50, eval_method = lift_index,topN = 100,
                                  # n_deciles = 100, type = "random",iter = 15, ncores = 8)
CV_CFMFsimple_auc <- mean(kfoldcv.CFMF(pmatrix, k = 5, seed = 123, lambda = 13.5,f = 50, eval_method = AUC_user, type = "random",iter = 15, ncores = 8), na.rm = T)



######################################################################
###############Evaluations of Global Popular Model####################
######################################################################
# Results below are generated by aggregating 10 runs of 5-fold CV.
# Results have been checked that they stabilise after with 10 runs.
# Seeds used: 123, 500, 777, 999, 1000, 1123, 1500, 1777, 1999, 2000


#Global Popular Model
load("Output/Popular/CV_popular_meanrank.Rdata")
plot_pop1 <- plot.evaluate(CV_popular_meanrank$popular_meanrank, 
                           title_txt = "Mean Rankings - Popular Model") ; plot_pop1


load("Output/Popular/CV_popular_optrank.Rdata")
plot_pop2 <- plot.evaluate(CV_popular_optrank$popular_optrank, title_txt = "Optimistic Rankings - Popular Model") ; plot_pop2


load("Output/Popular/CV_popular_hitratio.Rdata")
plot_pop3 <- plot.evaluate(CV_popular_hitratio$popular_hitratio, title_txt = "Hit Ratio - Popular Model",
                              xlab = "Hit Ratio", binwdth = 0.02) ; plot_pop3

load("Output/Popular/CV_popular_ndcg.Rdata")
plot_pop4 <- plot.evaluate(CV_popular_ndcg$popular_ndcg, title_txt = "nDCG - Popular Model",
                              xlab = "nDCG", binwdth = 0.02) ; plot_pop4


load("Output/Popular/CV_popular_lift.Rdata")
plot_pop5 <- plot.evaluate(CV_popular_lift$popular_lift, title_txt = "Lift Index - Popular Model",
                              xlab = "Lift Index", binwdth = 0.02) ; plot_pop5


multiplot(plot_pop1, plot_pop2, plot_pop3, plot_pop4,plot_pop5, cols=2)

#CV_popular_meanrank <- kfoldcv(fulldata = rtriple,k=5,seed = 123, model = global_popular,eval_method= mean_rank)
#CV_popular_optrank <- kfoldcv(fulldata = rtriple,k=5,seed = 123, model = global_popular,eval_method= opt_rank)
#CV_popular_hitratio <- kfoldcv(fulldata = rtriple,k=5,seed = 123,  
                               #model = global_popular,eval_method= hit_ratio, topN = 100)
#CV_popular_ndcg <- kfoldcv(fulldata = rtriple,k=5,seed = 123, model = global_popular,topN = 100,eval_method= nDCG)
#CV_popular_lift <- kfoldcv(fulldata = rtriple,k=5,seed = 123, model = global_popular,eval_method= lift_index, topN = 100, n_deciles = 100)
CV_popular_auc <- mean(kfoldcv(fulldata = rtriple,k=5,seed = 123, model = global_popular,eval_method= AUC_user), na.rm = T)


######################################################################
###############Evaluations of Random Recomm. Model####################
######################################################################
# Results below are generated by aggregating 10 runs of 5-fold CV.
# Results have been checked that they stabilise after with 10 runs.
# Seeds used: 123, 500, 777, 999, 1000, 1123, 1500, 1777, 1999, 2000


load("Output/Random/CV_random_meanrank.Rdata")
plot_random1 <- plot.evaluate(CV_random_meanrank$random_meanrank, 
                           title_txt = "Mean Rankings - Random") ; plot_random1


load("Output/Random/CV_random_optrank.Rdata")
plot_random2 <- plot.evaluate(CV_random_optrank$random_optrank, title_txt = "Optimistic Rankings - Random") ; plot_random2


load("Output/Random/CV_random_hitratio.Rdata")
plot_random3 <- plot.evaluate(CV_random_hitratio$random_hitratio, title_txt = "Hit Ratio - Random",
                           xlab = "Hit Ratio", binwdth = 0.02) ; plot_random3



load("Output/Random/CV_random_ndcg.Rdata")
plot_random4 <- plot.evaluate(CV_random_ndcg$random_ndcg, title_txt = "nDCG - Random",
                           xlab = "nDCG", binwdth = 0.02) ; plot_random4


load("Output/Random/CV_random_lift.Rdata")
plot_random5 <- plot.evaluate(CV_random_lift$random_lift, title_txt = "Lift Index - Random",
                           xlab = "Lift Index", binwdth = 0.02) ; plot_random5



multiplot(plot_random1, plot_random2, plot_random3, plot_random4,plot_random5, cols=2)


#CV_random_meanrank <- kfoldcv(fulldata = rtriple,k=5,seed = 123, model = random_recom,eval_method= mean_rank)
#CV_random_optrank <- kfoldcv(fulldata = rtriple,k=5,seed = 123, model = random_recom,eval_method= opt_rank)
#CV_random_hitratio <- kfoldcv(fulldata = rtriple,k=5,seed = 123, model = random_recom,eval_method= hit_ratio, topN= 100)
#CV_random_ndcg <- kfoldcv(fulldata = rtriple,k=5,seed = 123, model = random_recom,eval_method= nDCG, topN = 100)
#CV_random_lift <- kfoldcv(fulldata = rtriple,k=5,seed = 123, model = random_recom,eval_method= lift_index, topN = 100, n_deciles = 100)
CV_random_auc <- mean(kfoldcv(fulldata = rtriple,k=5,seed = 123, model = random_recom,eval_method= AUC_user), na.rm = T)

#################

#Comparing Evaluation Results of Confidence Model and Popular Model

df.diff <- data.frame("User" = 1:1171, "meanrank" = CV_CFMFconf2_meanrank$meanrank - CV_popular_meanrank$popular_meanrank,
                                        "optrank" = CV_CFMFconf2_optrank$optrank - CV_popular_optrank$popular_optrank,
                                        "hitratio" = CV_CFMFconf2_hitratio$hitratio - CV_popular_hitratio$popular_hitratio,
                                        "nDCG" = CV_CFMFconf2_nDCG$nDCG - CV_popular_ndcg$popular_ndcg,
                                        "lift" = CV_CFMFconf2_liftindex$liftindex - CV_popular_lift$popular_lift)
c1 <- ggplot(df.diff,  aes(User,meanrank)) + geom_point() +
  ggtitle("Confidence Model - Popular Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in Mean Ranks") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1); c1
c2 <- ggplot(df.diff,  aes(User,optrank)) + geom_point() +
  ggtitle("Confidence Model - Popular Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in Optimistic Ranks") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1); c2
c3 <- ggplot(df.diff,  aes(User,hitratio)) + geom_point() +
  ggtitle("Confidence Model - Popular Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in Hit Ratios") + 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1); c3
c4 <- ggplot(df.diff,  aes(User,nDCG)) + geom_point() +
  ggtitle("Confidence Model - Popular Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in nDCG") + 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1); c4
c5 <- ggplot(df.diff,  aes(User,lift)) + geom_point() +
  ggtitle("Confidence Model - Popular Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in Lift Index") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1); c5


multiplot(c1, c2, c3, c4,c5, cols=2)



#Comparing Evaluation Results of Confidence Model and CFMF Simple Model

df.diff2 <- data.frame("User" = 1:1171, "meanrank" = CV_CFMFconf2_meanrank$meanrank - CV_CFMFsimple_meanrank$simple_meanrank,
                      "optrank" = CV_CFMFconf2_optrank$optrank - CV_CFMFsimple_optrank$simple_optrank,
                      "hitratio" = CV_CFMFconf2_hitratio$hitratio - CV_CFMFsimple_hitratio$simple_hitratio,
                      "nDCG" = CV_CFMFconf2_nDCG$nDCG - CV_CFMFsimple_ndcg$simple_ndcg,
                      "lift" = CV_CFMFconf2_liftindex$liftindex - CV_CFMFsimple_lift$simple_lift)
d1 <- ggplot(df.diff2,  aes(User,meanrank)) + geom_point() +
  ggtitle("Confidence Model - CFMF Simple Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in Mean Ranks") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1)
d2 <- ggplot(df.diff2,  aes(User,optrank)) + geom_point() +
  ggtitle("Confidence Model - CFMF Simple Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in Optimistic Ranks") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1)
d3 <- ggplot(df.diff2,  aes(User,hitratio)) + geom_point() +
  ggtitle("Confidence Model - CFMF Simple Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in Hit Ratios") + 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1)
d4 <- ggplot(df.diff2,  aes(User,nDCG)) + geom_point() +
  ggtitle("Confidence Model - CFMF Simple Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in nDCG") + 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1)
d5 <- ggplot(df.diff2,  aes(User,lift)) + geom_point() +
  ggtitle("Confidence Model - CFMF Simple Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in Lift Index") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1)


multiplot(d1, d2, d3, d4,d5, cols=2)



#Comparing Evaluation Results of CFMF Simple Model and Popular Model


df.diff3 <- data.frame("User" = 1:1171, "meanrank" = CV_CFMFsimple_meanrank$simple_meanrank - CV_popular_meanrank$popular_meanrank,
                      "optrank" = CV_CFMFsimple_optrank$simple_optrank - CV_popular_optrank$popular_optrank,
                      "hitratio" = CV_CFMFsimple_hitratio$simple_hitratio - CV_popular_hitratio$popular_hitratio,
                      "nDCG" = CV_CFMFsimple_ndcg$simple_ndcg - CV_popular_ndcg$popular_ndcg,
                      "lift" = CV_CFMFsimple_lift$simple_lift - CV_popular_lift$popular_lift)
e1 <- ggplot(df.diff3,  aes(User,meanrank)) + geom_point() +
  ggtitle("CFMF Simple Model - Popular Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in Mean Ranks") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1)
e2 <- ggplot(df.diff3,  aes(User,optrank)) + geom_point() +
  ggtitle("CFMF Simple Model - Popular Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in Optimistic Ranks") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1)
e3 <- ggplot(df.diff3,  aes(User,hitratio)) + geom_point() +
  ggtitle("CFMF Simple Model - Popular Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in Hit Ratios") + 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1)
e4 <- ggplot(df.diff3,  aes(User,nDCG)) + geom_point() +
  ggtitle("CFMF Simple Model - Popular Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in nDCG") + 
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1)
e5 <- ggplot(df.diff3,  aes(User,lift)) + geom_point() +
  ggtitle("CFMF Simple Model - Popular Model") + stat_smooth() + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Difference in Lift Index") +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1)


multiplot(e1, e2, e3, e4,e5, cols=2)


##########

files <- paste("Euler Core/Saved From Core/kfoldcv_out2_meanrank_run",1:10,".Rdata", sep = "")
#seeds <- c( 123, 500, 777, 999, 1000, 1123, 1500, 1777, 1999, 2000)
results <- matrix(nrow = 10, ncol = 1171)
for (i in 1:length(files)){
  load(files[i])
  #print(seeds[i])
  #results[i,] <-  kfoldcv(fulldata = rtriple,k=5,seed = seeds[i], model = global_popular,eval_method= mean_rank)
  results[i,] <- kfoldcv_out2_meanrank$cv_meanrank
}
result <- apply(results, MARGIN =2, FUN = mean, na.rm = T)



#Combining 10 file results into 1
files <- paste("Euler Core/Saved From Core/Archived/kfoldcv_out4_meanrank_run",1:10,".Rdata", sep = "")
result <- matrix(nrow = 10, ncol = 1171)
for(i in 1:length(files)){
  load(files[i])
  result[i,] <- kfoldcv_out4_meanrank$cv_meanrank
  rm(kfoldcv_out4_meanrank)
}
meanrank <- apply(result, MARGIN = 2, FUN = mean,na.rm = T)
text <- "5 fold CV repeated 10 times"
CV_CFMFconf4_meanrank <- list("meanrank" = meanrank, "text"=text)
#save(CV_CFMFconf4_meanrank, file = "Output/CFMF Confidence/CV_CFMFconf4_meanrank.Rdata")
