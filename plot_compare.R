plot_compare <- function(){
  library(reshape2)
  library(ggplot2)
  
  dday_df <- data.frame(out_align$df_dday_aggbydday)
  scaledata <- dday_df
  ## Variabiles: Median: var[8:13] or svar[2:7]
  median <- colnames(scaledata)[8:13]
  
  ## Center in 0 dday: Recognize value of dday 0 
  data0 <- scaledata[which(scaledata$dday==0),]
  
  ## 1) Substract value in dday 0 of all data points from each individual data point
  center_scale <- scale(scaledata, center=data0, scale = FALSE)
  # Graphics
  scaledata_melt <- melt(data.frame(center_scale), id.vars = "dday")
  scaledata_melt <- scaledata_melt[which(scaledata_melt$variable %in% median),]
  psub <- ggplot(scaledata_melt, aes(x=dday, y=value, col=variable)) +
    geom_line(size=1) + geom_vline(xintercept=0) + theme_classic() + labs(title="Substract")
  
  
  ## 2) Quotient: Normalized by mean of dday 0
  center_scaleq <- scaledata
  for(i in 2:length(scaledata)){
    center_scaleq[,i] <- scaledata[,i] / as.numeric(data0[i])   
  }
  
  scaledata_melt <- melt(center_scaleq, id.vars = "dday")
  scalemean <- scaledata_melt[which(scaledata_melt$variable %in% median),]
  pq <- ggplot(scalemean, aes(x=dday, y=value, col=variable)) +
    geom_line(size=1) + geom_vline(xintercept=0) + theme_classic() + labs(title="Quotient")
 
  psmooth <- ggplot(scalemean, aes(x=dday, y=value, col=variable)) +
    geom_point(size=1) + geom_vline(xintercept=0) + 
    geom_smooth(method=loess) + scale_y_continuous(limits=c(0,1.2)) +
    theme_classic()
  
  
  ## 3) z-scoring: values in the transformed variable have the same relationship to one another as in the untransformed variable, but the transformed variable has mean 0 and standard deviation 1
  # Subtract the value in dday 0 of all data points from each individual data point, then divide those points by the standard deviation of all point
  center_scalez <- scale(scaledata, center=data0, scale = TRUE)
  # Graphics
  scaledata_melt <- melt(data.frame(center_scalez), id.vars = "dday")
  scaledata_melt <- scaledata_melt[which(scaledata_melt$variable %in% median),]
  pz <- ggplot(scaledata_melt, aes(x=dday, y=value, col=variable)) +
    geom_line(size=1) + geom_vline(xintercept=0) + theme_classic() + labs(title="Z scoring")
  # ggplot(scaledata_melt, aes(x=dday, y=value, col=variable)) +
  #   geom_point(size=1) + geom_smooth() + geom_vline(xintercept=0) + theme_classic()
  
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

return(list(pq, pz))
}



