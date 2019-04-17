add_normalised_rsvi <- function(df, sdovars, norm){
  
  ## Divide by median in zero-bin
  get_dsdovar <- function(df, sdovar){
    dsdovar <- paste0("d", sdovar)
    df[[dsdovar]] <- df[[sdovar]] / df[[dsdovar]]
    return(select(df, dsdovar))
  }
  
  dsdovars <- paste0("d", sdovars)
  
  df <- df %>% 
    left_join(norm, by="site") %>% 
    ungroup()
  
  df <- purrr::map_dfc(as.list(sdovars), ~get_dsdovar(df, .)) %>% 
    bind_cols( select(df, -one_of(dsdovars)), .)
  
}