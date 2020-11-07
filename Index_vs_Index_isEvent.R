
Index_vs_Index_isEvent <- function(x="evi", y="cci"){
  # Index 1:
  ind1 <- x
  # Index 2 :
  ind2 <- y
  
  ## Function ecuations for each group
  lm_eqn = function(df){
    m = lm(cci ~ evi, df);
    eq <- substitute(italic(R)^2~"="~r2*","~~italic(p)~"="~pv, # italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~2*","~~italic(p)~"="~pv, # # italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~2*","~~italic(p)~"="~pv
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(coef(m)[2], digits = 2), 
                          r2 = format(summary(m)$r.squared, digits = 2),
                          pv = format(summary(m)$coefficients[8], digits=2)))
    
    as.character(as.expression(eq));                 
  }
  
  
  # Regression by vegetation type
  for (i in 1:length(nclass)){
    
    data <- ddf_veg %>%
      filter(!is.na(is_flue_drought)) %>%
      filter(temp > 5) %>% 
      filter(classid == nclass[i]) %>%
      select(classid,site,date,flue,is_flue_drought,evi,cci)
    
    library(plyr) 
    eq <- ddply(data,.(site,is_flue_drought),lm_eqn)
    
    # detach(plyr)
    index_veg <- ggplot(data, aes(x=evi, y=cci, group=is_flue_drought)) +
      geom_point(size=1) +
      geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) + 
      facet_grid(is_flue_drought ~ site, scales="free_y") +
      theme_classic() + labs(title=nclass[i]) +
      geom_text(data=eq,aes(x = 0.35, y = 0.025,label=V1), cex=3, parse = TRUE, inherit.aes=FALSE) 
    
    print(index_veg)
  }


# Regressions for site
nsites <- unique(ddf$site)
coef <- data.frame() # relation, site, slope, r2, pvalue
relation <- paste(ind1,"_vs_",ind2,sep="") 
  for (i in 1:length(nsites)){
    
    data <- ddf %>%
      filter(!is.na(is_flue_drought)) %>%
      filter(temp > 5) %>% 
      filter(site == nsites[i]) %>%
      select(site,date,flue,is_flue_drought,evi,cci)
    
    eq <- ddply(data,.(is_flue_drought),lm_eqn)
    eq$relation <- relation
    eq$site  <- nsites[i]
    coef <- rbind(coef,eq) 
  }

  
}

pdf(paste("Regressions_CCI_EVI",".pdf",sep=""),width=10.84,height=5.30)
Index_vs_Index_isEvent()
dev.off()
