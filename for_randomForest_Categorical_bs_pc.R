require( randomForest )
require( caret )
require( tidyverse )

# load data and RF function
load("./data/ddf_v4.Rdata")
load("./data/metainfo_Tier1_sites_kgclimate_fluxnet2015.Rdata")
load("./data/obs_eval_NT.Rdata") #

source("analyse_modobs2.R")
source("wrap_ml.R")

# complement info using the meta info of FLUXNET sites provided through rsofun
ddf <- ddf %>% 
  left_join(metainfo_Tier1_sites_kgclimate_fluxnet2015 %>% select(site=sitename, classid), by = "site")

dovars <- c("cci","evi","ndvi","NIRv","pri") 

# Prepare data set
ddf_rf <- ddf %>% 
  left_join(rename(obs_eval_NT$ddf, site=sitename), by=c("site", "date")) %>%
  mutate (APAR = ppfd_fluxnet2015 * fapar) %>%  
  filter(!is.na(flue)) %>%
  select(site, is_flue_drought, flue, dovars, APAR, temp, classid) %>% 
  mutate(classid=factor(classid), is_flue_drought = factor(is_flue_drought))  %>% 
  drop_na()

# Select sites (N?19): Homogeneous and with drought periods
# sites <- c("AR-Vir", "AU-Ade", "AU-ASM", "AU-DaP", "AU-DaS", "AU-Dry", "AU-Stp", "AU-Whr",
#            "DE-Hai", "FR-LBr", "FR-Pue", "IT-Cpz", "IT-Ro1", "IT-SRo", "RU-Fyo", "SD-Dem",
#            "US-SRG", "US-SRM", "US-WCr")


# Class id:
ddf_rf$classid[ddf_rf$classid=="WSA"] <- "SAV"
cv <- c("ENF","DBF","GRA","EBF","SAV")

sites <- ddf_rf$site %>% unique() #%>% head(4)
ddf_sub <- ddf_rf %>% 
  dplyr::filter(site %in% sites) %>% #subsites
  filter(classid %in% cv) %>% droplevels()
sites <- ddf_sub$site %>% unique() #%>% head(4)

# Predictors combinations
complete <- c("ndvi",    "evi",     "cci",     "pri",     "NIRv",    "APAR", "temp", "classid")
rsvi <- c("ndvi",    "evi",     "cci",     "pri",     "NIRv")
# dos <- c("ndvi",    "evi",     "cci",     "pri",     "NIRv",    "PPFD")
# tres <- c("ndvi",    "evi",     "cci",     "pri",     "NIRv",    "PPFD", "temp")
# cuatro <- c("ndvi",    "evi")
# cinco <- c("ndvi",    "evi",  "PPFD", "temp", "classid")
# seis <- c("pri",    "cci",  "PPFD", "temp", "classid")
josep <- c("ndvi",  "cci",  "temp", "classid")
josep1 <- c("ndvi", "cci", "temp", "APAR")
josep2 <- c("NIRv", "cci", "pri", "ndvi", "temp", "classid")
josep3 <- c("NIRv", "pri", "temp", "classid")
estructura <- c("ndvi",    "evi",  "temp", "classid")
actividad <- c("pri",    "cci",  "temp", "classid")

# Each RSVI alone
ndvi <- c("ndvi")
evi <- c("evi")
cci <- c( "cci")
pri <- c("pri")
NIRv <- c("NIRv")

predictores <-list(complete, rsvi, josep, josep1, ndvi, evi, cci, pri, NIRv, josep2, josep3, estructura, actividad)

##----------------------------------
## Classification
##----------------------------------
## Leave-site-out with all predictors
rf_mylgocv <- wrap_ml( df = ddf_sub, 
                       nam_target = "is_flue_drought", 
                       nam_group = "site",
                       train_method = "myLGOCV",
                       predictors = predictores[[1]],  # use double square bracket here to access element of list
                       tune = FALSE,
                       inner = TRUE
)

## "LGOCV" does the same thing as "myLGOCV"
rf_lgocv <- wrap_ml( df = ddf_sub, 
                     nam_target = "is_flue_drought",
                     nam_group = "site",
                     train_method = "LGOCV",
                     predictors = predictores[[1]],  # use double square bracket here to access element of list
                     tune = FALSE,
                     inner = TRUE
)
# save(rf_mylgocv, file = "./data/rf_mylgocv.Rdata")

## NN_isflue
NNcompleto <- NN_isFlue[[1]]
NNRSVI <- NN_isFlue[[2]]
NNjosep <- NN_isFlue[[3]]
NNjosep1 <- NN_isFlue[[4]]
NNndvi <- NN_isFlue[[5]]
NNevi <- NN_isFlue[[6]]
NNcci <- NN_isFlue[[7]]
NNpri <- NN_isFlue[[8]]
NNNIRv <- NN_isFlue[[9]]

# cambio de nombre para hacer pruebas
rf_lgocv <- NNcompleto

## Accuracy of the main model seems to be (almost) identical to 
## the mean accuracy of the leave-site-out models. Makes sense.
print(paste("Accuracy of main model:", rf_mylgocv$rf$results$Accuracy))
print(paste("Mean accuracy across leave-site-out models:", purrr::map_dbl(rf_mylgocv$list_rf, "myresults") %>% mean()))

print(paste("Accuracy of main model:", rf_lgocv$rf$results$Accuracy))
print(paste("Mean accuracy across leave-site-out models:", purrr::map_dbl(rf_lgocv$list_rf, "myresults") %>% mean()))

## Test: How is accuracy calculated?
get_modobs <- function(df){
  tibble(mod = as.vector(df$pred), obs = df$obs)
}
list_modobs_listmodels <- purrr::map(rf_lgocv$list_rf, ~get_modobs(.))

calc_performance <- function(df){
  df_sum <- df %>% 
    dplyr::mutate(good = obs==mod) %>% 
    dplyr::summarise(good = sum(good))
  df_sum$good / nrow(df)
}
acc_test <- purrr::map_dbl(list_modobs_listmodels, ~calc_performance(.)) %>% mean()  # that's the same as given above as 'Mean accuracy across leave-site-out models'
print(paste("Re-calculated mean accuracy across leave-site-out models:", acc_test))

# ## Question: 'rf_mylgocv$list_rf' is now a list of 'nsites' RF-models, while 'rf_mylgocv$rf' is a single model.
# ## While the accuracy of the single model is given as rf_mylgocv$rf$results$Accuracy, when applying this model
# ## for predicting at each site separately, we get an accuracy of 1 for each prediction. I don't understand 
# ## how the single model relates to the list of models given by 'rf_mylgocv$list_rf'. This is shown here:
# predict_bysite <- function(site, df, model, nam_target){
#   df_test <- df %>% dplyr::filter(site == site)
#   pred_test <- predict(model, newdata = df_test)
#   out <- df_test %>% 
#     dplyr::select(obs = {{nam_target}}) %>% 
#     dplyr::mutate(mod = as.vector(pred_test))
#   return(out)
# }
# df_test_listmodels <- purrr::map(as.list(sites), ~predict_bysite(., ddf_sub, rf_mylgocv$list_rf[[.]], nam_target))
# df_test_globlmodel <- purrr::map(as.list(sites), ~predict_bysite(., ddf_sub, rf_mylgocv$rf, nam_target))
# 
# acc_listmodels <- purrr::map_dbl(df_test_listmodels, ~calc_performance(.)) %>% mean()
# acc_globlmodel <- purrr::map_dbl(df_test_globlmodel, ~calc_performance(.)) %>% mean()
# 
# print(acc_listmodels)
# print(acc_globlmodel)
# ## --> I don't get that.

## Construct confusion matrix
cm_simple <- list_modobs_listmodels %>% bind_rows() %>% table()
df_modobs_listmodels <- list_modobs_listmodels %>% bind_rows()
cm <- confusionMatrix( data = as.factor(df_modobs_listmodels$mod), 
                       reference = as.factor(df_modobs_listmodels$obs), positive="TRUE")

## Show results for each site (prediction trained at all other sites)
vec_acc <- purrr::map_dbl(rf_lgocv$list_rf, "myresults")
df_acc <- tibble(site = sites, accuracy = vec_acc )
print("Accuracy for each site (prediction trained at all other sites):")
print(df_acc)

## Get variable importance (see https://topepo.github.io/caret/variable-importance.html)
var_imp <- varImp(rf_lgocv$rf)
list_var_imp <- purrr::map(rf_lgocv$list_rf, ~varImp(.))
plot(var_imp)

## Alternative: GARSON FOR NN
# library(NeuralNetTools)
# garson(rf_lgocv$rf$finalModel)

## let's try now for the full dataset and josep's suggested predictors - for NN only because it's faster and better!!!
NN_isFlue<- list()
for(i in 1:length(predictores)){ # lenght(predictores)
rf_lgocv <- wrap_ml( df = ddf_sub, 
                                nam_target = "is_flue_drought", 
                                nam_group = "site",
                                # method =  "nnet",
                                train_method = "LGOCV",
                                predictors = predictores[[i]],  # use double square bracket here to access element of list
                                tune = F,
                                inner = F,
                                classification = TRUE
)
save(rf_lgocv , file = paste("./data/RF_isFlue_mod",i,".Rdata",sep=""))
#NN_isFlue [[i]] <- rf_lgocv
print(rf_lgocv$rf$results) # Promising: has accuracy of 0.81. 
}

## Real Josep suggestion
rf_lgocv <- wrap_ml( df = ddf_rf, 
                                nam_target = "is_flue_drought", 
                                nam_group = "site",
                                method = "nnet",
                                train_method = "LGOCV",
                                predictors = josep1,  # use double square bracket here to access element of list
                                tune = FALSE,
                                inner = TRUE,
                                classification = TRUE
)
print(rf_lgocv$rf$results) # Accuracy 0.81    4 0.8034787 0.120381


## And the same using veg class (classid) instead of site as group
rf_lgocv_class_full <- wrap_ml( df = ddf_sub, 
                                nam_target = "is_flue_drought", 
                                nam_group = "classid",
                                method = "nnet",
                                train_method = "LGOCV",
                                predictors = josep1,  # use double square bracket here to access element of list
                                tune = FALSE,
                                inner = FALSE,
                                classification = TRUE
)
print(rf_lgocv_class_full$rf$results) # Josep 1 Has lower accuracy: 0.75 / Josep 0.69


### Inside vegetation class
veg <- unique(ddf_sub$classid)

NN_isFlue_InsideVeg_4 <- list()
for(v in 1:length(veg)){
  ddf_veg <- ddf_sub %>% filter(classid == veg[v])
  
  # for(i in 1:length(predictores)){ # lenght(predictores)
    rf_lgocv <- wrap_ml( df = ddf_veg, 
                         nam_target = "is_flue_drought", 
                         nam_group = "site",
                         method = "nnet",
                         train_method = "LGOCV",
                         predictors = predictores[[4]],  # use double square bracket here to access element of list
                         tune = FALSE,
                         inner = TRUE,
                         classification = TRUE
    )
    NN_isFlue_InsideVeg_4 [[v]] <- rf_lgocv
    print(rf_lgocv$rf$results) # Promising: has accuracy of 0.81. 
  # }
}


## Recursive feature elimination (see https://topepo.github.io/caret/recursive-feature-elimination.html)
## XXX doesn't work. Better do this "by hand": Look at how model performance changes for different sets of predictors.

subsets <- complete %>% length() %>% seq() %>% rev()
# sites <- unique(df[[ "site" ]]) # 'nam_group' is site in this case (i.e. split the data by sparing data from one site for testing)
group_folds <- vector(mode = "list", length = length(sites))

nam_group = "site"
for (i in seq_along(sites)) group_folds[[i]] <- which(ddf_sub[[nam_group]] != sites[i])
names(group_folds) <- paste0("Subject", sites)

# proProces for Quantitative analysis
normalization <- preProcess(ddf_sub, c("center", "scale"))
x <- predict(normalization, ddf_sub)
x <- as.data.frame(x)

#XXXX Doesn't work:include ROC metric
# rfFuncs$summary <- twoClassSummary # 
# trainctrl <- trainControl(classProbs= TRUE, summaryFunction = twoClassSummary)

ctrl <- rfeControl(functions = lrFuncs,
                   method = "LGOCV",
                   index = group_folds,
                   repeats = 5,
                   verbose = FALSE,
                   rerank = TRUE
)

forml  <- as.formula(  paste( "is_flue_drought", "~", paste( complete, collapse=" + " ) ) )

df <- ddf_sub # x : is_flue_drought / flue
set.seed(10)
lmProfile <- caret::rfe( forml,
                         x,
                         method="nnet",
                         sizes = c(1:12),
                         rfeControl = ctrl
                         # metric = "ROC",
                         # trControl = trainctrl
)

lmProfile$fit
lmProfile$optVariables
head(lmProfile$variables)
predictors(lmProfile)
# head(lmProfile$resample)
trellis.par.set(caretTheme())
plot(lmProfile, type = c("g", "o"))
lmProfile$metric

### So we average the importance/ranking score of all variables in set, and choose the highest ranked 10 variables
set.size = 11 #you want set-size of 10
lm.vars <- lmProfile$variables 

lm.set <- lm.vars[lm.vars$Variables==set.size,  ] # selects variables of set-size (= 10 here)
#use aggregate to calculate mean ranking score (under column "Overall")
lm.set <- aggregate(lm.set[, c("Overall")], list(lm.set$var), mean)

#order from highest to low, and select first 10:
lm.order <- order(lm.set[, c("x")], decreasing = TRUE)[1:set.size]
lm.set[lm.order, ]

varImp(lmProfile)
varImp(lmProfile$fit)



#### Barras de clasificaci?n ####
library(graphics)
mosaicplot(cm$table)


##--------------------------------
## Quantitative model
##--------------------------------
# NN_Flue<- list()
for(i in 10:length(predictores)){ # lenght(predictores)
rf_lgocv_flue <- wrap_ml( df = ddf_sub, 
                          method = "nnet",
                          nam_target = "flue", 
                          nam_group = "site",
                          train_method = "LGOCV",
                          predictors = predictores[[i]],  # use double square bracket here to access element of list
                          tune = TRUE,
                          inner = TRUE,
                          classification = FALSE
)
print(paste("Results of main model:"))
print(rf_lgocv_flue$rf$results)   # RMSE 0.1765139   R2 0.3186167
print(i)
save(rf_lgocv_flue , file = paste("./data/NN_Flue_mod",i,".Rdata",sep=""))
# NN_Flue [[i]] <- rf_lgocv_flue
}

rf_lgocv_flue_josep1 <- wrap_ml( df = ddf_sub, 
                          method = "nnet",
                          nam_target = "flue", 
                          nam_group = "site",
                          train_method = "LGOCV",
                          predictors = josep1,  # use double square bracket here to access element of list
                          tune = FALSE,
                          inner = TRUE,
                          classification = FALSE
)

print(paste("Results of main model:"))
print(rf_lgocv_flue_josep1$rf$results)   # DIFFICULT: has r-squared of only 0.291, RRMSE 0.18

# Scatterplots
## A: Mod vs obs for single model, evaluated at all data
# rf_lgocv_flue$rf$pred_test <- predict(rf_lgocv_flue$rf, newdata = ddf_sub, preProcess = pre_process ) ## yes, that's the same
rf_lgocv_flue$rf$obs  <- ddf_sub[["flue"]]
out_modobs <- tibble(mod = as.vector(rf_lgocv_flue$rf$pred), 
                     obs = rf_lgocv_flue$rf$obs) 
out_modobs <- out_modobs %>%
  analyse_modobs2(mod = "mod", obs = "obs")
out_modobs$gg


## Density  ####
library(viridis)
theme_set(theme_bw(base_size = 16))
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

out_modobs$density <- get_density(out_modobs$obs, out_modobs$mod, n = 100)

library(Metrics)
bias_lab <- round(percent_bias(out_modobs$mod, out_modobs$obs),3) 
rsq_lab <- round(caret::postResample(out_modobs$mod, out_modobs$obs)['Rsquared'],2)
rmse_lab <- round(caret::postResample(out_modobs$mod, out_modobs$obs)['RMSE'],2)
n_lab <- length(out_modobs$mod)

ggplot(out_modobs, aes(x=mod, y=obs)) +
  geom_point(aes(x=mod, y=obs, color = density)) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x, cex=0.7) + 
  geom_abline(intercept=0, slope=1, linetype = "dashed", color="gray") +
  scale_color_viridis(option="magma") +
  theme_classic() +
  theme(axis.text=element_text(size=12, color="black"),
        axis.title=element_text(size=14),
        panel.border = element_rect(colour = "black", fill=NA)) +
  xlab("Predicted fLUE") + ylab("Observed fLUE") + 
  ylim(-0.1,1.5) + xlim(-0.1,1.5)  +
  annotate("text",x=1.25, y=0.30, label = paste("N ==", n_lab), parse=TRUE) +
  annotate("text",x=1.25, y=0.20, label = paste("R^2 ==" , rsq_lab), parse=TRUE) +
  annotate("text",x=1.25, y=0.10, label = paste("RMSE ==", rmse_lab), parse=TRUE) +
  annotate("text",x=1.25, y=0.0, label = paste("bias ==", bias_lab), parse=TRUE) 
  # annotate("text",x=1.25, y=0.25, label = paste("Slope ==", slope_lab), parse=TRUE) 


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
  analyse_modobs2(mod = "mod", obs = "obs")
out_modobs$gg

# This gives almost the same results as the summary of the main model 
results_by_listmodels <- purrr::map(list_modobs_listmodels, ~analyse_modobs2(.)) %>% 
  purrr::map_dfr(., "results") %>% 
  dplyr::summarise_all(.funs = mean)

print(results_by_listmodels)
print(rf_lgocv_flue$rf$results)


# RESULTADOS
rf_lgocv_flue <- NNflue[[1]]
NNRSVI <- NNflue[[2]]
NNjosep <- NNflue[[3]]
NNjosep1 <- NNflue2[[4]]
NNndvi <- NNflue[[5]]
NNevi <- NNflue[[6]]
NNcci <- NNflue[[7]]
NNpri <- NNflue[[8]]
NNNIRv <- NNflue[[9]]

# PLOT

#####

## Evaluate "by hand":
# Plot mod vs obs from all sub-models pooled (necessary to set 'inner' = TRUE to get all this data)
get_modobs <- function(df){
  tibble(mod = as.vector(df$pred), obs = df$obs)
}

list_modobs_listmodels <- purrr::map(rf_lgocv_flue$list_rf, ~get_modobs(.))
out_modobs <- list_modobs_listmodels %>%
  bind_rows %>% 
  analyse_modobs2(mod = "mod", obs = "obs")
out_modobs$gg

# this gives almost the same results
results_by_listmodels <- purrr::map(list_modobs_listmodels, ~analyse_modobs2(.)) %>% 
  purrr::map_dfr(., "results") %>% 
  dplyr::summarise_all(.funs = mean)

print(results_by_listmodels)
print(rf_lgocv_flue$rf$results)

## 


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

#### Vegetation class #### 
NN_flue_InsideVeg_4 <- list()
for(v in 1:length(veg)){
  ddf_veg <- ddf_sub %>% filter(classid == veg[v])
  
  # for(i in 1:length(predictores)){ # lenght(predictores)
  rf_lgocv <- wrap_ml( df = ddf_veg, 
                       nam_target = "flue", 
                       nam_group = "site",
                       method = "nnet",
                       train_method = "LGOCV",
                       predictors = predictores[[4]],  # use double square bracket here to access element of list
                       tune = FALSE,
                       inner = TRUE,
                       classification = FALSE
  )
  NN_flue_InsideVeg_4 [[v]] <- rf_lgocv
  print(rf_lgocv$rf$results) # Promising: has accuracy of 0.81. 
  # }
}


#### Serie de tiempo de un sitios en particular: ###
sitename <- "FR-Pue"
one <- list_modobs_listmodels$`FR-Pue` 
ts_one <- ddf %>% filter(site == sitename ) %>% left_join(rename(one, flue=obs), by="flue") %>% 
  select(site, date, flue, is_flue_drought, mod)
plot(ts_one$flue~ts_one$date, type="l", ylab="Observed fLUE", xlab="Predicted fLUE", main="FR-Pue")
lines(ts_one$mod~ts_one$date, col=2 )

library(reshape2)
ts <- melt(ts_one,id.vars = c("site","date","is_flue_drought"), measure.vars = c("flue", "mod") )
print(ggplot(ts, aes(x=date, y=value, group=variable)) +
        geom_line(aes(color=variable)) +
        scale_color_manual(values=c("black","red"),
                           name = NULL, labels = c("Observed fLUE","Predicted fLUE")) +
        theme_classic() + ggtitle(sitename)  +
        labs(x="Date", y="Unitless") + 
        theme(axis.text=element_text(size=12, color="black"),
              axis.title=element_text(size=14),
              panel.border = element_rect(colour = "black", fill=NA)) +
        scale_x_date(limits = as.Date(c('2000-01-01','2015-01-01'))) + labs(x="Date"))

#### 

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
  analyse_modobs2(mod = "mod", obs = "obs")
out_modobs$gg

# This gives almost the same results as the summary of the main model 
results_by_listmodels <- purrr::map(list_modobs_listmodels, ~analyse_modobs2(.)) %>% 
  purrr::map_dfr(., "results") %>% 
  dplyr::summarise_all(.funs = mean)

print(results_by_listmodels)
print(rf_lgocv_flue$rf$results)

#####--------------
# Compare model performances using resample()
models_compare <- resamples(list(RF=model_rf, NNET=model_nn))

# Summary of the models performances
summary(models_compare)
