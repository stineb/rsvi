gather_data <- function(){

  require(dplyr)
  require(lubridate)

  ##------------------------------------------------
  ## Get PRI and CCI for all sites
  ##------------------------------------------------
  path <- "C:/Users/Paula/Desktop/Pau/Ecologia terrestre"
  filn <- "C:/Users/Paula/Desktop/Pau/Ecologia terrestre/rsvi/data/MOD09GA_MODOCGA_filter_indices.Rdata" 
  
  ## load the data
  load( filn )  # loads a dataframe called 'data'
  df <- as_tibble(filter_state_data)  # copy 'filter_data' to 'df' and convert it to a 'tibble' (like a dataframe but with nicer properties)
  rm(filter_state_data) # delete object 'data'
  
  # No filtering by QC_500m
  # df <- read.csv("C:/Users/Paula/Desktop/Pau/Ecologia terrestre/rsvi/data/MOD09GA_MODOCGA_filter_onlyState1km_indices.csv")
  # df <- as_tibble(df)
  # 
  # Create date
  df$date <- as.Date(with(df, paste(YY, MM, DD,sep="-")), "%Y-%m-%d")
  
  ## rename variable
  df <- df %>%  dplyr::rename (site = sites_id) %>% mutate (site = as.character(site)) # otherwise creates problems with summarising. I don't understand why.

  ## aggregate to daily mean values (multiple data points given per day in original dataset)
  ## This is the short version
  ddf <- df %>% group_by( site, date ) %>%
                summarise_at( vars( one_of( c( "ndvi", "evi", "cci", "pri", "NIRv") ) ), funs( mean(., na.rm=TRUE) ) ) %>%
                mutate_at( vars( one_of( c( "ndvi", "evi", "cci", "pri", "NIRv"))), funs(remove_outliers(., coef=3.0)) )

  # ## This is the long version doing the same but using more lines (uncool)
  # ddf_test <- df %>% group_by( site, date ) %>%
  #               summarise(  ndvi   = mean( ndvi,   na.rm=TRUE ),
  #                           evi    = mean( evi ,   na.rm=TRUE ),
  #                           cci    = mean( cci ,   na.rm=TRUE ),
  #                           pri    = mean( pri ,   na.rm=TRUE ),
  #                           ndsi   = mean( ndsi,   na.rm=TRUE ),
  #                           wateri = mean( wateri, na.rm=TRUE ) ) %>%
  #               mutate( cci    = remove_outliers( cci,    coef=3.0 ),
  #                       pri    = remove_outliers( pri,    coef=3.0 ),
  #                       evi    = remove_outliers( evi,    coef=3.0 ),
  #                       ndvi   = remove_outliers( ndvi,   coef=3.0 ),
  #                       ndsi   = remove_outliers( ndsi,   coef=3.0 ),
  #                       wateri = remove_outliers( wateri, coef=3.0 ),
  #                       pri    = remove_outliers( pri,    coef=3.0 ) )

  # ## quickly check if the two give equal results - they don't. I don't understand why.
  # print(all.equal(ddf, ddf_test))

  ## Add PRI and CCI scaled to within 0 and 1 by range
  scale_range <- function( vec ){
    vec <- (vec - min(vec, na.rm=TRUE)) / (max(vec, na.rm=TRUE) - min(vec, na.rm=TRUE))
    return(vec)
  }

  ddf <- ddf %>%  mutate_at( vars( one_of( c("ndvi", "evi", "cci", "pri", "NIRv"))), funs(scale_range(.)) ) %>%
                  setNames( c( "site", "date", paste0("s", names(ddf)[-(1:2)] ) ) ) %>%
                  right_join( ddf, by = c("site", "date"))

  # range_cci <- range( ddf$cci, na.rm=TRUE )
  # range_pri <- range( ddf$pri, na.rm=TRUE )

  # ddf <- ddf %>% mutate(  scci = as.vector( scale( cci, center=range_cci[1], scale=( range_cci[2] - range_cci[1] ) ) ),
  #                         spri = as.vector( scale( pri, center=range_pri[1], scale=( range_pri[2] - range_pri[1] ) ) ) )

  return(ddf)
  
}

