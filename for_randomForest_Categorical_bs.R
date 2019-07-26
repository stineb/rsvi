require( randomForest )
require( caret )
require( tidyverse )

# Target
nam_target <- "is_flue_drought"

# load data and RF function
load("data/ddf_v3_flue1.RData")
source("wrap_random_forest.R")

# Data set
ddf_rf <- ddf %>% 
  left_join(rename(obs_eval_NT$ddf, site=sitename), by=c("site", "date")) %>%
  mutate (PPFD = ppfd_fluxnet2015 * fapar) %>%  # xxx comment: what you call PPFD here is actually APAR
  filter(!is.na(flue)) %>%
  select(site,nam_target, dovars, PPFD, temp, classid) %>% 
  mutate(classid=factor(classid))  %>% 
  drop_na()
  
if (nam_target == "is_flue_drought"){
  ddf_rf <- ddf_rf %>%  mutate(is_flue_drought = factor(is_flue_drought))
}

# Predictors combinations
complete <- c("ndvi",    "evi",     "cci",     "pri",     "NIRv",    "PPFD", "temp", "classid")
uno <- c("ndvi",    "evi",     "cci",     "pri",     "NIRv")
dos <- c("ndvi",    "evi",     "cci",     "pri",     "NIRv",    "PPFD")
tres <- c("ndvi",    "evi",     "cci",     "pri",     "NIRv",    "PPFD", "temp")
# cuatro <- c("ndvi",    "evi")
# cinco <- c("ndvi",    "evi",  "PPFD", "temp", "classid")
# seis <- c("pri",    "cci",  "PPFD", "temp", "classid")
nueve <- c("ndvi",    "cci",  "temp", "classid")

# Each RSVI alone
ndvi <- c("ndvi")
evi <- c("evi")
cci <- c( "cci")
pri <- c("pri")
NIRv <- c("NIRv")

predictores <-list(complete, uno, nueve, ndvi, evi, cci, pri, NIRv)

# Select sites (homogeneous and with drought periods)
sitios <- c("AR-Vir", "AU-Ade", "AU-ASM", "AU-DaP", "AU-DaS", "AU-Dry", "AU-Stp", "AU-Whr",
            "DE-Hai", "FR-LBr", "FR-Pue", "IT-Cpz", "IT-Ro1", "IT-SRo", "RU-Fyo", "SD-Dem",
            "US-SRG", "US-SRM", "US-WCr")


#### LEAVE-ONE-OUT APPROACH

# N? Drought dates per site
# num_datos <- ddf_rf %>% 
#   group_by(site, is_flue_drought) %>%
#   filter(site %in% sitios) %>%
#   summarise(no_rows = length(is_flue_drought)) 
# num_datos <-  reshape2::dcast(data = num_datos,formula = site~is_flue_drought,fun.aggregate = sum,value.var = "no_rows")


##----------------------------------
## Leave-group-out with all predictors
##----------------------------------
rf_lgocv <- wrap_random_forest( df = ddf_rf, 
                                nam_target = nam_target, 
                                nam_instance = "site",
                                train_method = "lgocv",
                                predictors = predictores[[1]],  # use double square bracket here to access element of list
                                tune = FALSE   
                                )


resultados <- list()
for (i in 1:length(sitios)){

  test <- sitios[i]

  ddf_train <- ddf_rf %>%
    filter(site != test)

  ddf_test <- ddf_rf %>%
    filter(site == test)

  rf_predict <- wrap_random_forest( train = ddf_train,
                                    test = ddf_test,
                                    target = nam_target,
                                    predictors = predictores[[1]],  # use double square bracket here to access element of list
                                    tune = TRUE
                                    )

  resultados[[i]] <- rf_predict
}


##----------------------------------
## Complete Model
## xxx comment: this seems to use an old 'wrap_random_forest()' functions. 'database' is not an argument in the function defined above.
##              and 'Bestmtry' is not an element of the returned list.
##----------------------------------
models <- list()
predictors <- predictores[[1]]
models[[1]] <- wrap_random_forest(database = ddf_rf, target = nam_target, 
                              predictors = predictors, sites = sitios, tune = TRUE)

#Save parameters tuned to apply in the others models
mtry <- models[[1]]$Bestmtry
  
# ALL MODELS
start <- Sys.time()
for (p in 2:length(predictores)){
  predictors <- predictores[[p]]
  models[[p]] <- wrap_random_forest(database = ddf_rf, target= nam_target, predictors = predictors, sites=sitios, tune=FALSE)
}
end <- Sys.time()
start-end

# Global model: All predicted and all observed for each combinations of predictors
Matrices <- list()
for(m in 1:length(models)){
  matrix <- confusionMatrix(table(unlist(modelos[[m]][3]), unlist(modelos[[m]][2])))
  Matrices[[m]] <-matrix
  # ov <- matrix$overall
  # plot(t(matrix$table), main="Leave-one-out approach - ALL - nueve", xlab="Obs",ylab="Pred")
}

# # Compare predicted outcome and true outcome
# modelo_final <- lm(formula = observados ~ predichos)
# modelo_final$coefficients

### BY VEGETATION CLASS ####
clasification <- NULL
resultados <- list()
importancia <- list()
resultados_clase <- list()

for (c in 1:length(unique(ddf_rf$classid))){
predichos <- NULL
observados <- NULL
sitios_clas <- unique(ddf_rf$site[which(ddf_rf$classid==unique(ddf_rf$classid)[c])])

for (i in 1:length(sitios_clas)){
  test <- sitios_clas[i]
  
  ddf_train <- ddf_rf %>%
    filter(site != test) %>%
    filter(classid == unique(ddf_rf$classid)[c])
  
  ddf_test <- ddf_rf %>%
    filter(site == test) %>%
    filter(classid == unique(ddf_rf$classid)[c])
  
  forml  <- as.formula(  paste( nam_target, "~", paste( predictors, collapse=" + " ) ) )
  
  rf <- randomForest(forml, data = ddf_train, preProc   = "range")
  
  if (length(ddf_test$is_flue_drought)!=0){
    ddf_test <- rbind(ddf_train[1, ] , ddf_test)
    ddf_test <- ddf_test [-1,]
    
    pred <- predict(model, newdata=ddf_test,  preProc   = "range")
    
    # CLASIFICACION
    matrix <- confusionMatrix(table(pred, ddf_test$is_flue_drought))
    ov <- matrix$overall
    
    clasification <- rbind(clasification,ov)
    
    predichos <- c(predichos, pred)
    observados <- c(observados, ddf_test$is_flue_drought)
    resultados[[i]] <- model
    importancia[[i]] <- importance(model) 
  }
}  

## CLASIFICACION
matrix <- confusionMatrix(table(observados, predichos))
resultados_clase[[c]] <- matrix
# plot(t(matrix$table), main="Leave-one-out approach - ALL", xlab="Obs",ylab="Pred")
}

