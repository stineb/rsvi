calc_performance <- function(df){
  # df_sum <- df %>% 
  #   dplyr::mutate(good = obs==mod) %>% 
  #   dplyr::summarise(good = sum(good))
  # df_sum$good / nrow(df)
  df <- df %>%
    dplyr::mutate(data = as.factor(mod), reference = as.factor(obs)) %>% 
    dplyr::select(data, reference)
  cm_tmp <- confusionMatrix( data = df$data, 
                             reference = df$reference )
  return(cm_tmp$overall)
}