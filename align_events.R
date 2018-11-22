#' Aligns data by events
#'
#' Uses a vectory specifying whether data falls into an event to reshape data, aligning by the onset of the event
#' 
#' @param df A data frame containing all data continuously along time.
#' @param truefalse A boolean vector with \code{length(truefalse)==nrow(df)} specifying whether the data points fall into an event.
#' @param dovars
#' @param before 
#' @param after 
#' @param do_norm
#' @param normbin
#'
#' @return An aligned data frame
#' @export
#'
#' @examples df_alg <- align_events( df, truefalse, before=30, after=300 )
#' 
align_events <- function( df, truefalse, dovars, before=20, after=100, do_norm=FALSE, normbin=2 ){

  require( dplyr )
  require( tidyr )

  ## Bins for different variables
  bins  <- seq( from=-before, to=after, by=(after+before+1)/4 )

  ##--------------------------------------------------------
  ## Identify events ()
  ##--------------------------------------------------------
  events <- get_consecutive( 
              truefalse, 
              leng_threshold = 10, 
              do_merge       = FALSE
              )

  ##--------------------------------------------------------
  ## Re-arrange data, aligning by beginning of events
  ## Creates data frame where not all rows are retained from df
  ## and columns added for 'dday' (number of day relative to onset of event)
  ## and 'iinst' number of event to which row belongs.
  ##--------------------------------------------------------
  if (nrow(events)>1){

    df_dday <- tibble()
    for ( iinst in 1:nrow(events) ){
      after_inst <- min( after, events$len[iinst] )
      dday <- seq( from=-before, to=after_inst, by=1 )
      idxs <- dday + events$idx_start[iinst]
      drophead <- which( idxs < 1 )
      if (length(drophead)>0){
        idxs <- idxs[ -drophead ]
        dday <- dday[ -drophead ]
      }
      addrows <- df %>% slice( idxs ) %>% mutate( dday=dday, inst=iinst )
      df_dday <- df_dday %>% bind_rows( addrows )              
    }

    ##--------------------------------------------------------
    ## Normalise re-arranged data relative to a certain bin's median
    ##--------------------------------------------------------
    ## add bin information based on dday to expanded df
    df_dday <- df_dday %>% mutate( inbin  = cut( as.numeric(dday), breaks = bins ) )

    tmp <- df_dday %>% group_by( inbin ) %>% 
                       summarise_at( vars(one_of(dovars)), funs(median( ., na.rm=TRUE )) ) %>%
                       complete( inbin, fill = list( vpd  = NA ) ) %>% 
                       dplyr::select( vpd )

    tmp <- unlist( tmp )[1:(length(bins)-1)]
    df_dday$nvpd <- df_dday$vpd / tmp[normbin]

    ##--------------------------------------------------------
    ## Aggregate accross events
    ##--------------------------------------------------------
    df_dday_aggbydday <- df_dday %>%  group_by( dday ) %>% 
                                      summarise_at( vars(one_of(dovars)), funs(median( ., na.rm=TRUE), quantile( ., 0.25, na.rm=TRUE), quantile( ., 0.75, na.rm=TRUE) ) ) %>%
                                      filter( !is.na( dday ) )

    ## or if this doesn't work, do this:
    df_dday_aggbydday <- df_dday %>%  group_by( dday ) %>% 
                                      select( dday, one_of(dovars) ) %>%
                                      summarise_all( funs(median( ., na.rm=TRUE), quantile( ., 0.25, na.rm=TRUE), quantile( ., 0.75, na.rm=TRUE) ) ) %>%
                                      filter( !is.na( dday ) )

    ## or if this doesn't work, do this:
    ## Get quantiles for variable 'bias'
    df_dday_agg_med <- df_dday %>%  group_by( dday ) %>% 
                                    summarise_at( vars( one_of(dovars) ), funs(  median(.) ), na.rm = TRUE ) %>%
                                    setNames( c("dday", paste0( names(.)[-1], "_med" ) ) )

    df_dday_agg_q25 <- df_dday %>%  group_by( dday ) %>% 
                                    summarise_at( vars( one_of(dovars) ), funs(  quantile(., probs=0.25) ), na.rm = TRUE ) %>%
                                    setNames( c("dday", paste0( names(.)[-1], "_q25" ) ) )

    df_dday_agg_q75 <- df_dday %>%  group_by( dday ) %>% 
                                    summarise_at( vars( one_of(dovars) ), funs(  quantile(., probs=0.75) ), na.rm = TRUE ) %>%
                                    setNames( c("dday", paste0( names(.)[-1], "_q75" ) ) )

    df_dday_aggbydday <- df_dday_agg_med %>%  left_join( df_dday_agg_q25, by = "dday" ) %>% left_join( df_dday_agg_q75, by = "dday" ) %>%
                                              filter( !is.na( dday ) )

  } else {

    df_dday           <- NULL
    df_dday_aggbydday <- NULL

  }

  out <- list( df_dday=df_dday, df_dday_aggbydday=df_dday_aggbydday )
  return( out )

}


get_consecutive <- function( dry, leng_threshold=5, anom=NULL, do_merge=FALSE ){
  ##------------------------------------
  ## Returns a dataframe that contains information about events (starting index and length) 
  ## of consecutive conditions (TRUE) in a boolean vector ('dry' - naming is a legacy).
  ##------------------------------------

  ## replace NAs with FALSE (no drought). This is needed because of NAs at head or tail
  dry[ which(is.na(dry)) ] <- FALSE

  ## identifies periods where 'dry' true for consecutive days of length>leng_threshold and 
  ## creates data frame holding each instance's info: start of drought by index in 'dry' and length (number of days thereafter)
  instances <- data.frame( idx_start=c(), len=c() )
  consecutive_dry <- rep( NA, length( dry ) )
  ndry  <- 0
  ninst <- 0
  for ( idx in 1:length( dry ) ){
    if (dry[idx]){ 
      ndry <- ndry + 1 
    } else {
      if (ndry>=leng_threshold) { 
        ## create instance
        ninst <- ninst + 1
        addrow <- data.frame( idx_start=idx-(ndry), len=ndry )
        instances <- rbind( instances, addrow )
      }
      ndry <- 0
    }
    consecutive_dry[idx] <- ndry
  }
  if (ndry>leng_threshold){
    ## create a last instance if the last dry period extends to the end of the time series
    ninst <- ninst + 1
    addrow <- data.frame( idx_start=idx-(ndry), len=ndry )
    instances <- rbind( instances, addrow )
  }


  if (nrow(instances)>0){

    ## Get cumulative deficit per instance (deficit w.r.t. 1, where 'anom' is a vector with values 0-1)
    if (!is.null(anom)){
      instances$deficit <- rep( NA, nrow(instances) )
      for ( idx in 1:nrow(instances) ){
        instances$deficit[idx] <- sum( anom[ instances$idx_start[idx]:(instances$idx_start[idx]+instances$len[idx]-1) ] )
      }
    }

    ## merge events interrupted by short non-drought periods
    ## if in-between non-drought period is shorter than both of the drought periods
    ## before and after non-drought period
    if (do_merge){
      
      print("dimensions of instances before merging short periods")
      print(dim(instances))

      ninst_save <- nrow( instances ) + 1
      ninst      <- nrow( instances )

      while (ninst < ninst_save){

        ninst_save <- nrow( instances )

        instances_merged <- data.frame( idx_start=c(), len=c() )

        idx <- 0
        while (idx<(nrow(instances)-1)){
          idx <- idx + 1

          len_betweendrought <- instances$idx_start[idx+1] - (instances$idx_start[idx] + instances$len[idx] + 1)
          
          if (len_betweendrought<instances$len[idx] && len_betweendrought<instances$len[idx+1]){
            addrow <- data.frame( idx_start=instances$idx_start[idx], len=(instances$idx_start[idx+1] + instances$len[idx+1]) - instances$idx_start[idx] )
            instances_merged <- rbind( instances_merged, addrow )
            idx <- idx + 1
          } else {
            instances_merged <- rbind( instances_merged, instances[idx,] )
            if (idx==(nrow(instances)-1)){
              instances_merged <- rbind( instances_merged, instances[idx+1,] )
            }
          }
        }

        instances <- instances_merged    

        ninst <- nrow( instances )

        print( "dimensions of instances after merging short periods" )
        print( dim( instances ) )

      }

    }

  }

  return( instances )
}

  