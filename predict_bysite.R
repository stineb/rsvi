predict_bysite <- function(site, df, model, nam_target, all = FALSE){
  if (!all){
    df <- df %>% dplyr::filter(site == site)
  } else {
    df <- df
  }
  pred_test <- predict(model, newdata = df)
  out <- df %>%
    dplyr::select(obs = {{nam_target}}) %>%
    dplyr::mutate(mod = as.vector(pred_test))
  return(out)
}