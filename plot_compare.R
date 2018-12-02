plot_compare <- function(){
    
  dday_df <- data.frame(out_align$df_dday_aggbydday)
  scaledata <- dday_df
  ## Recognize value of dday 0
  data0 <- dday_df[which(dday_df$dday==0),]
  
  ## Normalized by mean of dday 0
  for(i in 2:length(scaledata)){
    data0 <- dday_df[which(dday_df$dday==0), i]
    scaledata[,i] <- scaledata[,i] / data0   
  }
  
  library(reshape2)
  library(ggplot2)
  scaledata_melt <- melt(scaledata, id.vars = "dday")
  var <- unique(scaledata_melt$variable)
  
  ## Graphics
  # Mean: var[7:12] or S meanvar[2:6]
  mean <- var[7:12]
  scalemean <- scaledata_melt[which(scaledata_melt$variable %in% mean),]
  p <- ggplot(scalemean, aes(x=dday, y=value, col=variable)) +
    geom_line(size=1) + geom_vline(xintercept=0) + theme_classic()
 
  psmooth <- ggplot(scalemean, aes(x=dday, y=value, col=variable)) +
    geom_point(size=1) + geom_vline(xintercept=0) + 
    geom_smooth(method=loess) + scale_y_continuous(limits=c(0,1.2)) +
    theme_classic()
  
  # Q33-Q66
  # qq <- var[c(19:24,31:36)]
  # scaleqq <- scaledata_melt[which(scaledata_melt$variable %in% qq),]
  # pq <- ggplot(scaleqq, aes(x=dday, y=value,group=variable, colour=variable)) + 
  #   geom_line(size=1) + geom_vline(xintercept=0) + theme_classic() +
  #   scale_color_manual(name="Variable",
  #                      breaks=c("ndvi_q33","evi_q33","cci_q33","pri_q33","ndsi_q33","wateri_q33"),
  #                      labels=c("ndvi","evi","cci","pri","ndsi","wateri"),
  #       values=c("#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3",
  #                "#F8766D","#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3")) +
  #   scale_y_continuous(limits=c(0.5,1.2)) 
  
  ### Other ways to scale
  # mean to subtract a constant from every value of a variable. center is FALSE, no centering is done
  center_scale <- scale(scaledata, scale = FALSE)
  # Center in 0: substract value in dday 0 of all data points from each individual data point
  center_scale <- scale(scaledata, center=data0, scale = FALSE)
  
  # z-scoring: values in the transformed variable have the same relationship to one another as in the untransformed variable, but the transformed variable has mean 0 and standard deviation 1
  # Subtract the mean of all data points from each individual data point, then divide those points by the standard deviation of all point
  center_scale <- scale(scaledata, center=TRUE, scale = TRUE)
  # Subtract the value in dday 0 of all data points from each individual data point, then divide those points by the standard deviation of all point
  center_scale <- scale(scaledata, center=data0, scale = TRUE)
  
  # # Graphics
  # scaledata_melt <- melt(data.frame(center_scale), id.vars = "dday")
  # scaledata_melt <- scaledata_melt[which(scaledata_melt$variable %in% mean),]
  # ggplot(scaledata_melt, aes(x=dday, y=value, col=variable)) +
  #   geom_line(size=1) + geom_vline(xintercept=0) + theme_classic()
  # ggplot(scaledata_melt, aes(x=dday, y=value, col=variable)) +
  #   geom_point(size=1) + geom_smooth() + geom_vline(xintercept=0) + theme_classic()

return(p)
}



