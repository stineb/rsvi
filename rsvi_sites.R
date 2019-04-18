#' Plot rsvi by site
#'
#' Uses a data frame with drought day and indices values for all FLUXNET homogeneous sites
#' 
#' @param df_dday A data frame containing all data continuously along time, required column named \code{site, dday}. 
#' @param name_site A character string specifying name of the site of interest from the list of homogeneous sites

rsvi_sites <- function(df_dday, name_site){ # out_align$df_dday

  library(dplyr)
  library(reshape2)
  library(ggplot2)
  library(plyr)
  library(stringi)
  
  if(!name_site%in%homo_sites){print("It is not a homogeneous site or it is misspelled")}
  
  df_dday <- data.frame(df_dday)
  
  # Data frame with site and dday
  df_dday_bysite <- df_dday %>%  group_by( dday, site ) %>% 
    summarise_at( vars(one_of(dovars)), funs(median( ., na.rm=TRUE), q33( ., na.rm=TRUE), q66( ., na.rm=TRUE) ) ) %>%
    filter( !is.na( dday ) )
  
  
  df_dday_bysite$site <- as.factor(df_dday_bysite$site)
  by_site <- df_dday_bysite %>% group_by(site)
  nsites <- n_groups(by_site) # Nº Sites = 22
  # Unique sites:
  sites <- unique(by_site$site)
  # Number of events of each site
  gsize <- df_dday[which(df_dday$dday==0),] %>% group_by(site)  %>% group_size()
  # 1 3 5 1 10 8 3 10 9 19 6 4 10 4 11 3 1 10 2 13 7 9
  # Site and number of events
  site_events <- data.frame(sites,gsize)
  
  # Data 
  df_dday_bysite_class <- merge(df_dday_bysite, metainfo_sites_fluxnet2015[,c(1,5)],by.x="site", by.y="mysitename") 
  nclass <- unique(df_dday_bysite_class$classid)

# Variables for each site, order by Class ID  
  # Pick varibales of interest
  median <- colnames(df_dday_bysite)[8:12] 
  
  # NDVI:1 , EVI:2, CCI: 3, PRI:4, sPRI: 5, NIRV: 6
  val  <- c(1:6) # All: c(1:6)
  median <- median[val]
  
  library(scales)
  color <- hue_pal()(5)[val]
  
  # Graphic for one site
  site1 <- data.frame(df_dday_bysite_class[which(df_dday_bysite_class$site==name_site),])
  data0 <- site1[which(site1$dday==0),c(-1,-2,-33)]
  center_scalez <- scale(site1[c(-1,-2,-33)], center=data0, scale = TRUE)
  center_scalez <- cbind(site1[,c(1,2,33)],center_scalez)
  scaledata_melt <- melt(data.frame(center_scalez), id.vars = c("dday","site"))
  scaledata_melt <- scaledata_melt[which(scaledata_melt$variable %in% median),]
  scaledata_melt$value <- as.numeric(scaledata_melt$value) 
  
  pz <- ggplot(scaledata_melt, aes(x=dday, y=value, col=variable)) +
    geom_line(size=1) + geom_vline(xintercept=0) + theme_classic() + scale_color_manual(values=color) +
    labs(title=paste("Class",site1$classid[1]," - Site:",name_site,sep=" ")) 
  return(pz)
  
  # All sites:
  
} # end function

