require(randomForest)
require(caret)
require(tidyverse)
require(rlang)

# load data and RF function
load("./data/ddf_v4.Rdata")
source("wrap_ml.R")
source("calc_performance.R")
source("predict_bysite.R")

# complement info using the meta info of FLUXNET sites provided through rsofun
ddf <- ddf %>% 
  left_join(rsofun::metainfo_Tier1_sites_kgclimate_fluxnet2015 %>% select(site=sitename, classid), by = "site")

# Predictors combinations
complete <- c("ndvi",    "evi",     "cci",     "pri",     "NIRv",    "APAR", "temp", "classid")
uno <- c("ndvi",    "evi",     "cci",     "pri",     "NIRv")
dos <- c("ndvi",    "evi",     "cci",     "pri",     "NIRv",    "APAR")
tres <- c("ndvi",    "evi",     "cci",     "pri",     "NIRv",    "APAR", "temp")
# cuatro <- c("ndvi",    "evi")
# cinco <- c("ndvi",    "evi",  "APAR", "temp", "classid")
# seis <- c("pri",    "cci",  "APAR", "temp", "classid")
nueve <- c("ndvi",    "cci",  "temp", "classid")
josep <- c("ndvi", "cci", "temp", "APAR")

# Each RSVI alone
ndvi <- c("ndvi")
evi <- c("evi")
cci <- c( "cci")
pri <- c("pri")
NIRv <- c("NIRv")

predictores <-list(complete, uno, nueve, ndvi, evi, cci, pri, NIRv)

# Select sites (homogeneous and with drought periods)
# XXX update this with new site selection XXX
sitios <- c("AR-Vir", "AU-Ade", "AU-ASM", "AU-DaP", "AU-DaS", "AU-Dry", "AU-Stp", "AU-Whr",
            "DE-Hai", "FR-LBr", "FR-Pue", "IT-Cpz", "IT-Ro1", "IT-SRo", "RU-Fyo", "SD-Dem",
            "US-SRG", "US-SRM", "US-WCr")

# Prepare data set: complement with fluxnet-provided variables
load("~/eval_pmodel/data/v3/obs_eval_NT.Rdata")
ddf_rf <- ddf %>% 
  left_join(rename(obs_eval_NT$ddf, site=sitename), by=c("site", "date")) %>%
  mutate (APAR = ppfd_fluxnet2015 * fapar) %>%  # xxx comment: what you call PPFD here is actually APAR
  filter(!is.na(flue)) %>%
  select(site, is_flue_drought, flue, complete, APAR, temp, classid) %>% 
  mutate(classid=factor(classid), is_flue_drought = factor(is_flue_drought))  %>% 
  drop_na()
  
# XXX Test: Take only subset for testing
subsites <- ddf_rf$site %>% unique() %>% head(4)
ddf_sub <- ddf_rf %>% 
  dplyr::filter(site %in% subsites)

##----------------------------------
## Classification
##----------------------------------
## Leave-site-out with all predictors
rf_mylgocv <- wrap_ml( df = ddf_sub, 
                       nam_target = "is_flue_drought", 
                       nam_group = "site",
                       method = "nnet",
                       train_method = "myLGOCV",
                       predictors = predictores[[1]],  # use double square bracket here to access element of list
                       tune = FALSE,
                       inner = TRUE,
                       all = TRUE
)

## "LGOCV" does the same thing as "myLGOCV"
rf_lgocv <- wrap_ml( df = ddf_sub, 
                     nam_target = "is_flue_drought", 
                     nam_group = "site",
                     method = "nnet",
                     train_method = "LGOCV",
                     predictors = predictores[[1]],  # use double square bracket here to access element of list
                     tune = FALSE,
                     inner = TRUE,
                     all = TRUE
)
#save(rf_mylgocv, file = "./data/rf_mylgocv.Rdata")

## Accuracy of the main model seems to be (almost) identical to 
## the mean accuracy of the leave-site-out models. Makes sense.
print(paste("Accuracy of main model:", rf_mylgocv$rf$results$Accuracy))
print(paste("Mean accuracy across leave-site-out models:", purrr::map_dbl(rf_mylgocv$list_rf, "myaccuracy") %>% mean()))

print(paste("Accuracy of main model:", rf_lgocv$rf$results$Accuracy))
print(paste("Mean accuracy across leave-site-out models:", purrr::map_dbl(rf_lgocv$list_rf, "myaccuracy") %>% mean()))

## Test: How is accuracy calculated?
get_modobs <- function(df){
  tibble(mod = as.vector(df$pred), obs = df$obs)
}
list_modobs_listmodels <- purrr::map(rf_lgocv$list_rf, ~get_modobs(.))

acc_test <- purrr::map_dbl(list_modobs_listmodels, ~calc_performance(.)) %>% purrr::map(., "Accuracy") %>% mean()  # that's NOT EXACTLY the same as given above as 'Mean accuracy across leave-site-out models'
print(paste("Re-calculated mean accuracy across leave-site-out models:", acc_test))

## GAVIN METHOD:
## produce a model ensemble where each model is trained on a slightly different dataset due to the different 
## LOSO groups, then we take the median prediction of the ensemble as the best for the flux.
nam_target <- "is_flue_drought"
sites <- unique(ddf_sub$site)
get_most_frequent <- function(vec){
  ## warning: if equal number of times false and true, then false is returned
  out <- sort(table(vec), decreasing=TRUE)[1] %>% 
    names() %>% 
    as.logical()
  return(out)
}

## predict for all sites with each individual model from left-group-out trainings
df_test_listmodels <- purrr::map(as.list(sites), ~predict_bysite(., ddf_sub, rf_mylgocv$list_rf[[.]], nam_target, all = TRUE))

## average across predictions (take most frequent for categorical variables)
df_gavin <- purrr::map_dfc(df_test_listmodels, ~select(., mod) %>% mutate(mod = as.logical(mod))) %>% 
  mutate(mod_average = apply(., 1, get_most_frequent)) %>% 
  mutate(obs = df_test_listmodels[[1]] %>% dplyr::pull(obs))

## evaluate, based on confusion matrix
cm <- confusionMatrix( data = as.factor(df_gavin$mod_average), 
                       reference = as.factor(df_gavin$obs) )
print(cm)

## should be the same as returned by wrap_ml()$cm_median_inner
print(rf_mylgocv$rf$cm_median_inner)

## OK!

# ## Question: 'rf_mylgocv$list_rf' is now a list of 'nsites' RF-models, while 'rf_mylgocv$rf' is a single model.
# ## While the accuracy of the single model is given as rf_mylgocv$rf$results$Accuracy, when applying this model
# ## for predicting at each site separately, we get an accuracy of 1 for each prediction. I don't understand
# ## how the single model relates to the list of models given by 'rf_mylgocv$list_rf'. This is shown here:
# nam_target <- "is_flue_drought"
# sites <- unique(ddf_sub$site)
# df_test_listmodels <- purrr::map(as.list(sites), ~predict_bysite(., ddf_sub, rf_mylgocv$list_rf[[.]], nam_target))
# df_test_globlmodel <- purrr::map(as.list(sites), ~predict_bysite(., ddf_sub, rf_mylgocv$rf, nam_target))
# 
# acc_listmodels <- purrr::map(df_test_listmodels, ~calc_performance(.)) %>% purrr::map_dbl(., "Accuracy") %>% mean()
# acc_globlmodel <- purrr::map(df_test_globlmodel, ~calc_performance(.)) %>% purrr::map_dbl(., "Accuracy") %>% mean()
# 
# print(acc_listmodels)
# print(acc_globlmodel)
# ## --> I don't get that.

## Construct confusion matrix
cm_simple <- list_modobs_listmodels %>% bind_rows() %>% table()
df_modobs_listmodels <- list_modobs_listmodels %>% bind_rows()
cm <- confusionMatrix( data = as.factor(df_modobs_listmodels$mod), 
                       reference = as.factor(df_modobs_listmodels$obs) )

## Calculate Acccuracy and Kappa as the mean across inner-loop predictions
print(paste("Mean accuracy across leave-site-out models:", purrr::map_dbl(rf_lgocv$list_rf, "myaccuracy") %>% mean()))
print(paste("Mean kappa across leave-site-out models:", purrr::map_dbl(rf_lgocv$list_rf, "mykappa") %>% mean()))

# xxx
# df_tmp <- list_modobs_listmodels[[1]] %>% 
#   mutate(data = as.factor(mod), reference = as.factor(obs)) %>% 
#   select(data, reference) 
# # cm_simple <- df_tmp %>% 
# #   table()
# # vcd::Kappa(cm_simple) not necessary
# cm_tmp <- confusionMatrix( data = df_tmp$data, 
#                            reference = df_tmp$reference )
# xxxxx

## Show results for each site (prediction trained at all other sites)
vec_acc <- purrr::map_dbl(rf_lgocv$list_rf, "myaccuracy")
df_acc <- tibble(site = names(rf_lgocv$list_rf), accuracy = vec_acc )
print("Accuracy for each site (prediction trained at all other sites):")
print(df_acc)

## Get variable importance (see https://topepo.github.io/caret/variable-importance.html)
var_imp <- varImp(rf_lgocv$rf)
list_var_imp <- purrr::map(rf_lgocv$list_rf, ~varImp(.))

## let's try now for the full dataset and josep's suggested predictors - for NN only because it's faster and better!!!
rf_lgocv_class_full <- wrap_ml( df = ddf_rf, 
                                nam_target = "is_flue_drought", 
                                nam_group = "site",
                                method = "nnet",
                                train_method = "LGOCV",
                                predictors = josep,  # use double square bracket here to access element of list
                                tune = FALSE,
                                inner = FALSE,
                                classification = TRUE
)
print(rf_lgocv_class_full$rf$results) # Promising: has accuracy of 0.81

## And the same using veg class (classid) instead of site as group
rf_lgocv_class_full <- wrap_ml( df = ddf_rf, 
                                nam_target = "is_flue_drought", 
                                nam_group = "classid",
                                method = "nnet",
                                train_method = "LGOCV",
                                predictors = josep,  # use double square bracket here to access element of list
                                tune = FALSE,
                                inner = FALSE,
                                classification = TRUE
                                )
print(rf_lgocv_class_full$rf$results) # Has lower accuracy: 0.7526834

## Recursive feature elimination (see https://topepo.github.io/caret/recursive-feature-elimination.html)
## XXX doesn't work. Better do this "by hand": Look at how model performance changes for different sets of predictors.
subsets <- predictores[[1]] %>% length() %>% seq() %>% rev()
sites <- unique(ddf_rf[[ "site" ]]) # 'nam_group' is site in this case (i.e. split the data by sparing data from one site for testing)
group_folds <- caret::groupKFold( ddf_rf[[ "site" ]], k = length(sites) )

ctrl <- rfeControl(functions = lmFuncs,
                   method = "LGOCV",
                   index = group_folds,
                   repeats = 1,
                   verbose = FALSE
                   )

forml  <- as.formula(  paste( "is_flue_drought", "~", paste( complete, collapse=" + " ) ) )

set.seed(10)
lmProfile <- caret::rfe( forml, 
                         ddf_rf,
                         sizes = subsets,
                         rfeControl = ctrl
                         )

##--------------------------------
## Quantitative model
##--------------------------------
rf_lgocv_flue <- wrap_ml( df = ddf_sub, 
                          method = "nnet",
                          nam_target = "flue", 
                          nam_group = "site",
                          train_method = "LGOCV",
                          predictors = josep,  # use double square bracket here to access element of list
                          tune = FALSE,
                          inner = TRUE,
                          classification = FALSE
                          )

print(paste("Results of main model:"))
print(rf_lgocv_flue$rf$results)

## A: Mod vs obs for single model, evaluated at all data
# rf_lgocv_flue$rf$pred_test <- predict(rf_lgocv_flue$rf, newdata = ddf_sub, preProcess = pre_process ) ## yes, that's the same
rf_lgocv_flue$rf$obs  <- ddf_sub[["flue"]]
out_modobs <- tibble(mod = as.vector(rf_lgocv_flue$rf$pred), 
                     obs = rf_lgocv_flue$rf$obs) %>%  
  analyse_modobs2(mod = "mod", obs = "obs")
out_modobs$gg

## B: Mod vs obs for leave-group-out models, evaluated at left-out site data
## Note: it's necessary to set 'inner' = TRUE in the wrap_ml() call to get all this data
## ==> Note: much worse performance than suggested above. 
## Important: The performance of the pooled model (shown here) is not the same as the 
## mean across individual models from left-out sites.
get_modobs <- function(df){
  tibble(mod = as.vector(df$pred), obs = df$obs)
}
list_modobs_listmodels <- purrr::map(rf_lgocv_flue$list_rf, ~get_modobs(.))
out_modobs <- list_modobs_listmodels %>%
  bind_rows %>% 
  rbeni::analyse_modobs2(mod = "mod", obs = "obs")
out_modobs$gg

# This gives almost the same results as the summary of the main model 
results_by_listmodels <- purrr::map(list_modobs_listmodels, ~analyse_modobs2(.)) %>% 
  purrr::map_dfr(., "results") %>% 
  dplyr::summarise_all(.funs = mean)

print(results_by_listmodels)
print(rf_lgocv_flue$rf$results)

## C: Is this the same like we would get from predicting at all left-out sites
## with the single fitted model?
get_modobs <- function(df){
  tibble(mod = as.vector(df$pred_main), obs = df$obs)
}
list_modobs_listmodels_main <- purrr::map(rf_lgocv_flue$list_rf, ~get_modobs(.))
out_modobs <- list_modobs_listmodels_main %>%
  bind_rows %>% 
  rbeni::analyse_modobs2(mod = "mod", obs = "obs")
out_modobs$gg
## ==> This is the same as A


## knowing this, we can do the same for reduced predictors ('uno')
rf_lgocv_flue2 <- wrap_ml( df = ddf_sub, 
                           nam_target = "flue", 
                           nam_group = "site",
                           train_method = "LGOCV",
                           predictors = uno,  # use double square bracket here to access element of list
                           tune = FALSE,
                           inner = FALSE,
                           classification = FALSE
)
print(rf_lgocv_flue2$rf$results)

