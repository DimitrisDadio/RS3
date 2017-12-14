#Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#Histogram plots for CV evaluation results

plot.evaluate <- function(cv_results, threshold = 100, title_txt = NULL, xlab = "rank", binwdth = 10){
  avg_results <- round(mean(cv_results, na.rm = T),2)
  med_results <- round(median(cv_results, na.rm = T), 2)
  #accept_results <- sum(cv_results <= threshold)
  
  plot <- ggplot(data.frame("rank"=cv_results), aes(rank) )  +
    geom_histogram(binwidth = binwdth) +ggtitle(title_txt) + 
    theme(plot.title = element_text(hjust = 0.5)) #+
    #annotate("text", x = max(cv_results)/2, y = Inf, label = paste("\n \n No. of users with recommendations \n within top 100 items: ", accept_results))
  
  ymax_anot <- max(ggplot_build(plot)$data[[1]]$count) #maximum limits on yaxis
  
  plot <- plot + labs(x = xlab) +  annotate("text", x = avg_results , y = ymax_anot, label = paste("mean", avg_results), colour = "red") +
    annotate("pointrange", x = c(avg_results,med_results), y = 0, ymin = 0, ymax = c(ymax_anot-5,ymax_anot-45), colour = c("red","blue"), size = 0.5) + 
    annotate("text", x = med_results, y = ymax_anot - 40, label = paste("med", med_results), colour = "blue") 
  
}

