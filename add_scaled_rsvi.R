add_scaled_rsvi <- function(ddf, dovars, method="range"){

  scale_range_vec <- function( vec ){
    vec <- (vec - min(vec, na.rm=TRUE)) / (max(vec, na.rm=TRUE) - min(vec, na.rm=TRUE))
    return(vec)
  }
  
  scale_range <- function(ddf){
    ddf <- ddf %>%
      dplyr::select(site, date, one_of(dovars)) %>%
      mutate_at(
        vars( one_of(dovars) ),
        list( ~scale_range_vec(.) ) ) %>%
      setNames( c( "site", "date", paste0("s", dovars) ) ) %>%
      right_join( ddf, by = c("site", "date") )
    return(ddf)
  }
  
  ## Normalise by median value in dday-bin before drought onset ("zero-bin")
  ## Get median in zero-bin
  if (method=="range"){
    ## Scale to between 0 and 1 => add 's' to column names
    ## This is the same way as I did to produce figure pri_vs_fvar_ALL.R
    ## (/alphadata01/bstocker/sofun/utils_sofun/analysis_sofun/fluxnet2015/plot_agg_nn_fluxnet2015.R)
    ddf <- ddf %>%
      scale_range()
    
  } else if (method=="range_bysite"){
    
    ddf <- purrr::map_dfr( as.list(unique(ddf$site)), ~dplyr::filter(ddf, site==.) %>% scale_range() )    
    
  }
  
  # else if (method=="zscore") {
  #   ## This is what Paula did - CANNOT BE DONE HERE. NEEDS TO BE DONE AFTER ALIGNING.
  #   ddf <- ddf %>% 
  #     dplyr::select(site, date, one_of(dovars)) %>% 
  #     mutate_at(
  #       vars(one_of(dovars)),
  #       list( ~scale(., center=, scale=TRUE))
  #     )
  #     data.frame(scale(scaledata[,-1], center=data0, scale = TRUE))
  # }
  
  return(ddf)   
}