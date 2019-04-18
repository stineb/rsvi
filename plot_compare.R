plot_compare <- function(df_dday_aggbydday, scale = FALSE, method = "zscore"){ 
 
  library(reshape2)
  library(ggplot2)
  
  scaledata <- data.frame(df_dday_aggbydday)
  
  if (scale==FALSE){
    median <- colnames(scaledata)[c(7:11)]     # Median var
  } else {
    median <- colnames(scaledata)[c(2:6)]
  } # Median svar

  ## Center in 0 dday: Recognize value of dday 0 
  data0 <- scaledata[which(scaledata$dday==0),-1]
  
  #### Method ####
  if (method=="substract"){
    
    ## 1) Substract value in dday 0 of all data points from each individual data point
    center_scale <- scale(scaledata, center=data0, scale = FALSE)
    
    # Graphics
    scaledata_melt <- melt(data.frame(center_scale), id.vars = "dday")
    scaledata_melt <- scaledata_melt[which(scaledata_melt$variable %in% median),]
    
    pz <- ggplot( scaledata_melt, aes(x=dday, y=value, col=variable)) +
      geom_line(size=1) + 
      geom_vline(xintercept=0) + 
      theme_classic() + 
      labs(title="Substract")

   } else if (method=="quotient"){
     
    ## 2) Quotient: Normalized by median in dday 0
    center_scaleq <- scaledata
    for(i in 2:length(scaledata)){
      center_scaleq[,i] <- scaledata[,i] / as.numeric(data0[i])   
    }

    scaledata_melt <- melt(center_scaleq, id.vars = "dday")
    scalemean <- scaledata_melt[which(scaledata_melt$variable %in% median),]
    pz <- ggplot(scalemean, aes(x=dday, y=value, col=variable)) +
      geom_line(size=1) + geom_vline(xintercept=0) + theme_classic() + labs(title="Quotient")
    
    # psmooth <- ggplot(scalemean, aes(x=dday, y=value, col=variable)) +
    #   geom_point(size=1) + geom_vline(xintercept=0) + 
    #   geom_smooth(method=loess) + scale_y_continuous(limits=c(0,1.2)) +
    #   theme_classic()
    
  } else if (method=="zscore_default"){
    
    ## 3.a) Center default: (data-mean_data)/sd
    center_scalezb <- scale(scaledata, center=TRUE, scale = TRUE)
    
  } else {
    
    ## 3.b) z-scoring: values in the transformed variable have the same relationship to one another as in the untransformed variable, but the transformed variable has mean 0 and standard deviation 1
    # Subtract the value in dday 0 of all data points from each individual data point, then divide those points by the standard deviation of all point
    center_scalez <- data.frame(scale(scaledata[,-1], center=data0, scale = TRUE))
    center_scalez$dday <- scaledata[,1]
    
    # Graphics
    scaledata_melt <- melt(data.frame(center_scalez), id.vars = "dday")
    scaledata_melt <- scaledata_melt[which(scaledata_melt$variable %in% median),]
    pz <- ggplot(scaledata_melt, aes(x=dday, y=value, col=variable)) +
      geom_line(size=1) + geom_vline(xintercept=0) + theme_classic() + labs(title="Z scoring") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            legend.title=element_text(size=12), 
            legend.text=element_text(size=11))
    # ggplot(scaledata_melt, aes(x=dday, y=value, col=variable)) +
    #   geom_point(size=1) + geom_smooth() + geom_vline(xintercept=0) + theme_classic()

  }
  return(pz)
}

# ERROR BARS
#### ---- PLOT_COMPARE ALL with error bars ---- ####

# scaledata <- data.frame(out_align$df_dday_aggbydday)
# scaledata_sd <- data.frame(out_align$df_dday_aggbydday_sd)
# 
# ## Variabiles: Median: var[8:13] or svar[2:7]
# median <- colnames(scaledata_mean)[7:11]
# variables <- median
# 
# ## Center in 0 dday: Recognize value of dday 0 
# data0 <- scaledata[which(scaledata$dday==0),]
# # z-scoring: values in the transformed variable have the same relationship to one another as in the untransformed variable, but the transformed variable has mean 0 and standard deviation 1
# # Subtract the value in dday 0 of all data points from each individual data point, then divide those points by the standard deviation of all point
# center_scalez <- scale(scaledata, center=data0, scale = TRUE)
# center_scalez <- data.frame(center_scalez)
# center_scalez$dday <- scaledata$dday
# 
# # Same for sd
# data0sd <- scaledata_sd[which(scaledata_sd$dday==0),]
# # z-scoring: values in the transformed variable have the same relationship to one another as in the untransformed variable, but the transformed variable has mean 0 and standard deviation 1
# # Subtract the value in dday 0 of all data points from each individual data point, then divide those points by the standard deviation of all point
# center_scalezsd <- scale(scaledata_sd, center=data0sd, scale = TRUE)
# center_scalezsd <- data.frame(center_scalezsd)
# center_scalezsd$dday <- scaledata_sd$dday
# 
# scaledata_melt <- melt(center_scalezsd, id.vars = "dday")
# variables <- colnames(scaledata_sd)[7:11]
# scaledata_melt_sd <- scaledata_melt[which(scaledata_melt$variable %in% variables),]
# 
# # Graphics
# scaledata_melt <- melt(center_scalez, id.vars = "dday")
# scaledata_melt_mean <- scaledata_melt[which(scaledata_melt$variable %in% variables),]
# # scaledata_error <- scaledata_melt[which(scaledata_melt$variable %in% error),]
# pz <- ggplot(scaledata_melt_mean, aes(x=dday, y=value, col=variable)) +
#   geom_line(size=1) + geom_vline(xintercept=0) + theme_classic() + labs(title="Z scoring")
# # ggplot(scaledata_melt, aes(x=dday, y=value, col=variable)) +
# #   geom_point(size=1) + geom_smooth() + geom_vline(xintercept=0) + theme_classic()
# 
# pz
# 
# # merge mean y sd
# melt <- cbind(scaledata_melt_mean, scaledata_melt_sd[,3])
# colnames(melt) <- c("dday","variable","mean","sd")
# 
# ggplot(melt, aes(x=dday, y=mean, col=variable)) +
#   geom_point() + geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=1, position=position_dodge(0.5)) +
#   geom_vline(xintercept=0) + theme_classic() + labs(title="Z scoring")

  
