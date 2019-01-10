rsvi_sites <- function(df_dday, name_site){ # out_align$df_dday

  library(dplyr)
  library(reshape2)
  library(ggplot2)
  
  df_dday <- data.frame(out_align$df_dday)
  # Data frame with site and dday
  df_dday_bysite <- df_dday %>%  group_by( dday, site ) %>% 
    summarise_at( vars(one_of(dovars)), funs(median( ., na.rm=TRUE), q33( ., na.rm=TRUE), q66( ., na.rm=TRUE) ) ) %>%
    filter( !is.na( dday ) )
  
  # Nº Sites = 22
  df_dday_bysite$site <- as.factor(df_dday_bysite$site)
  by_site <- df_dday_bysite %>% group_by(site)
  nsites <- n_groups(by_site)
  # ndata= 38 52 43 37 71 65 37 57 77 81 43 81 81 51 81 70 33 81 81 81 81 46
  gsize <- group_size(by_site)
  sites <- unique(by_site$site)
  
  pdf("Sites_variables.pdf",width=6.84,height=5.30)
  median <- colnames(df_dday_bysite)[10:16]
  for (i in 1:nsites){
    site1 <- data.frame(df_dday_bysite[which(df_dday_bysite$site==sites[i]),])
    ## Scale and Center in 0=dday: Recognize value of dday 0 
    data0 <- site1[which(site1$dday==0),-1:-2]
    center_scalez <- scale(site1[-1:-2], center=data0, scale = TRUE)
    center_scalez <- cbind(site1[,1:2],center_scalez)
    scaledata_melt <- melt(data.frame(center_scalez), id.vars = c("dday","site"))
    scaledata_melt <- scaledata_melt[which(scaledata_melt$variable %in% median),]
    pz <- ggplot(scaledata_melt, aes(x=dday, y=value, col=variable)) +
      geom_line(size=1) + geom_vline(xintercept=0) + theme_classic() + labs(title=paste("Site:",sites[i]," - Size:",gsize[i],sep=" "),
    sub="Z scoring")
    print(pz)
  }
  dev.off()
}
