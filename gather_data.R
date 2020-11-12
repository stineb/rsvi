gather_data <- function( filn, add_scaled = FALSE ){

  require(dplyr)
  require(lubridate)

  ##------------------------------------------------
  ## Get PRI and CCI for all sites
  ##------------------------------------------------

  # path <- "C:/Users/Paula/Desktop/Pau/Ecologia terrestre"
  # filn <- "C:/Users/Paula/Desktop/Pau/Ecologia terrestre/rsvi/data/MOD09GA_MODOCGA_filter_indices.Rdata" 

  ## load the data
  # load( filn )  # loads a dataframe called 'data'
  df <- read_csv(filn)
  
  # df <- as_tibble( filter_state_data )  # copy 'filter_data' to 'df' and convert it to a 'tibble' (like a dataframe but with nicer properties)
  # rm( filter_state_data ) # delete object 'data'
  
  # Create date
  df <- df %>% 
    mutate( date = ymd(paste(as.character(YY), as.character(MM), as.character(DD), sep="-")) ) %>% 
    rename (site = sites_id) %>% 
    mutate (site = as.character(site)) # otherwise creates problems with summarising. I don't understand why.

  ## aggregate to daily mean values (multiple data points given per day in original dataset)
  ## This is the short version
  ddf <- df %>% group_by( site, date ) %>%
                summarise_at( vars( one_of( c( "ndvi", "evi", "cci", "pri", "NIRv") ) ), list( ~mean(., na.rm=TRUE) ) ) %>%
                mutate_at( vars( one_of( c( "ndvi", "evi", "cci", "pri", "NIRv"))), list( ~remove_outliers(., coef=3.0) ) )

  print(paste("Number of rows in df:  ", nrow(df)))
  print(paste("Number of rows in ddf: ", nrow(ddf)))

  ## Add scaled to within 0 and 1 by range
  if (add_scaled){
    
    scale_range <- function( vec ){
      vec <- (vec - min(vec, na.rm=TRUE)) / (max(vec, na.rm=TRUE) - min(vec, na.rm=TRUE))
      return(vec)
    }
    
    ddf <- ddf %>%  mutate_at( vars( one_of( c("ndvi", "evi", "cci", "pri", "NIRv"))), list( ~scale_range(.) ) ) %>%
      setNames( c( "site", "date", paste0("s", names(ddf)[-(1:2)] ) ) ) %>%
      right_join( ddf, by = c("site", "date"))
  }

  return(ddf)
  
}

