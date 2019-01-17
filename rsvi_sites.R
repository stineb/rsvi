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
  sites <- unique(by_site$site)
  
  # Number of events of each site
  site_cero <- df_dday[which(df_dday$dday==0),] 
  cero_by_site <- site_cero %>% group_by(site)
  gsize <- group_size(cero_by_site)
  # 1 3 5 1 10 8 3 10 9 19 6 4 10 4 11 3 1 10 2 13 7 9
  
  # Data 
  df_dday_bysite_class <- merge(df_dday_bysite, metainfo_sites_fluxnet2015[,c(1,5)],by.x="site", by.y="mysitename") 
  nclass <- unique(df_dday_bysite_class$classid)
  
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
  
  # SMOOTHING by class
  pdf("Sites_variables_byclass_v2.pdf",width=6.84,height=5.30)
  for (c in 1:length(nclass)){
    class1  <- data.frame(df_dday_bysite_class[which(df_dday_bysite_class$classid==nclass[c]),])
    nsites_class <- unique(class1$site) 
    for (i in 1:length(nsites_class)){
      site1 <- data.frame(class1[which(class1$site==nsites_class[i]),])
      ## Scale and Center in 0=dday: Recognize value of dday 0 
      data0 <- site1[which(site1$dday==0),c(-1,-2,-45)]
      center_scalez <- scale(site1[c(-1,-2,-45)], center=data0, scale = TRUE)
      center_scalez <- cbind(site1[,c(1,2,45)],center_scalez)
      scaledata_melt <- melt(data.frame(center_scalez), id.vars = c("dday","site"))
      scaledata_melt <- scaledata_melt[which(scaledata_melt$variable %in% median),]
      scaledata_melt$value <- as.numeric(scaledata_melt$value) 
      pz <- ggplot(scaledata_melt, aes(x=dday, y=value, col=variable)) +
        # geom_point(size=1) +
        geom_smooth(method=loess) + geom_vline(xintercept=0) +
        theme_classic() + labs(title=paste("Class",nclass[c]," - Site:",nsites_class[i]," - Size:",gsize[i],sep=" "), sub="Z scoring")
     
       print(pz)
       
       ## Index vs Index ##
       pcci <- ggplot(center_scalez,aes(x=ndvi_median, y=cci_median)) +
         geom_point(size=1) +
         geom_smooth(method=lm) + 
         theme_classic() + labs(title=paste("Class",nclass[c]," - Site:",nsites_class[i],sep=" "))
       print(pcci) 
       
       ppri <- ggplot(center_scalez,aes(x=ndvi_median, y=pri_median)) +
         geom_point(size=1) +
         geom_smooth(method=lm) + 
         theme_classic() + labs(title=paste("Class",nclass[c]," - Site:",nsites_class[i],sep=" "))
       print(ppri)
       
    }
  }
  dev.off()
}


#### Indice vs Indice #### 
df_dday2 <- merge(df_dday, metainfo_sites_fluxnet2015[,c(1,5)],by.x="site", by.y="mysitename") 

# NO rows with "is event" = NA
df_dday2 <- df_dday2[which(!is.na(df_dday2$isevent)),]
nclass <- unique(df_dday2$classid)

pdf("indices",width=6.84,height=5.30)
for (c in 1:length(nclass)){
  class1  <- data.frame(df_dday2[which(df_dday2$classid==nclass[c]),])
  nsites_class <- unique(class1$site) 
  for (i in 1:length(nsites_class)){
    site1 <- data.frame(class1[which(class1$site==nsites_class[i]),])
    
    ## temporal series
    site1$isevent<-as.factor(site1$isevent) 
    
    center_scalez <- scale(site1[c(10:17)])
    center_scalez <- cbind(site1[,c(1,2,18)],center_scalez)
    
    # # points
    # plot(as.Date(center_scalez$date), center_scalez$cci, col=center_scalez$isevent, pch=16, xlab="Date",
    #      ylab="CCI",main=paste(nclass[c],nsites_class[i],sep=" - "))
    # points(as.Date(center_scalez$date), center_scalez$ndvi, col=center_scalez$isevent, pch=2)
    # # Lines 
    # plot(as.Date(center_scalez$date), center_scalez$cci, type="l", xlab="Date",
    #      ylab="CCI",main=paste(nclass[c],nsites_class[i],sep=" - "))
    # lines(as.Date(center_scalez$date), center_scalez$ndvi, col=2)
    # 
    # plot(site1$ndvi, site1$cci, col=site1$isevent, pch=16, xlab="NDVI",
    #      ylab="CCI",main=paste(nclass[c],nsites_class[i],sep=" - "))
    # plot(site1$ndvi, site1$pri, col=site1$isevent, pch=16, xlab="NDVI",
    #      ylab="PRI", main=paste(nclass[c],nsites_class[i],sep=" - "))
    
    
    # pdf(paste(nclass[c],"_",nsites_class[i],"cci_ndvi",".pdf",sep=""),width=6.84,height=5.30)
    
    pc <- ggplot(site1, aes(x=ndvi, y=cci, group=isevent)) +
      geom_point(size=1) +
      geom_smooth(method=lm) + facet_grid(~isevent) +
      theme_classic() + labs(title=paste("Class",nclass[c]," - Site:",nsites_class[i],sep=" "))
    print(pc)
    
    # dev.off()
    
    # pdf(paste("Boxplot_",nclass[c],"_",nsites_class[i],"cci_ndvi",".pdf",sep=""),width=6.84,height=5.30)
    pcboxplot <- ggplot(site1, aes(x=ndvi, y=cci, group=isevent, fill=isevent)) +
      geom_boxplot() + theme_classic() + labs(title=paste("Class",nclass[c]," - Site:",nsites_class[i],sep=" "))
    print(pcboxplot)
    # dev.off()
  }
}
dev.off()




  #   
  ### normalized by value before drought
  #   data0 <- site1[which(site1$dday==0),c(-1,-2,-45)]
  #   center_scalez <- scale(site1[c(-1,-2,-45)], center=data0, scale = TRUE)
  #   center_scalez <- cbind(site1[,c(1,2,45)],center_scalez)
  #   scaledata_melt <- melt(data.frame(center_scalez), id.vars = c("dday","site"))
  #   scaledata_melt <- scaledata_melt[which(scaledata_melt$variable %in% median),]
  #   scaledata_melt$value <- as.numeric(scaledata_melt$value) 
  #   pz <- ggplot(scaledata_melt, aes(x=dday, y=value, col=variable)) +
  #     # geom_point(size=1) +
  #     geom_smooth(method=loess) + geom_vline(xintercept=0) +
  #     theme_classic() + labs(title=paste("Class",nclass[c]," - Site:",nsites_class[i]," - Size:",gsize[i],sep=" "), sub="Z scoring")
  #   
  #   print(pz)
  
#### Relation with fLUE ####

## Normalise re-arranged data relative to a certain bin's median
##--------------------------------------------------------
if (do_norm){
  ## add bin information based on dday to expanded df
  df_dday <- df_dday %>% mutate( inbin  = cut( as.numeric(dday), breaks = bins ) )
  
  tmp <- df_dday %>% group_by( inbin ) %>% 
    summarise_at( vars(one_of(dovars)), funs(median( ., na.rm=TRUE )) ) %>% 
    filter( !is.na(inbin) )
  
  norm <- slice(tmp, normbin)
  
  ## subtract from all values
  df_dday <- df_dday %>% mutate_at( vars(one_of(dovars)), funs(. - norm$.) )
}