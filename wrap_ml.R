# RANDOM FOREST FUNCTION
# XXX comment: argument declaration should use values directly, not objects
wrap_ml <- function(df, nam_target, predictors, nam_group = "site", train_method="myLGOCV", 
                    method = "rf", tune = FALSE, seed = 1982, classification = TRUE, inner = FALSE,
                    all = FALSE){
  
  require(caret)
  require(recipes)
  
  # Construct formula
  forml  <- as.formula(  paste( nam_target, "~", paste( predictors, collapse=" + " ) ) )
  
  # get groups (sites)
  sites <- df[[ nam_group ]] %>% unique()
  
  # Specify model training parameters  
  if (train_method=="myLGOCV"){

    # This follows the tutorial on http://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/
    # Do a n_sites-fold cross validation leaving one site out in each "fold"
    n_sites <- sites %>% length()
    group_folds <- caret::groupKFold( df[[ nam_group ]], k = n_sites )
    traincotrlParams <- caret::trainControl( index = group_folds, 
                                             method = "repeatedcv", 
                                             number = 5, 
                                             repeats = 3,
                                             savePredictions = "final"   # predictions on each validation resample are then available as modl$pred$Resample
                                             )

  } else if (train_method=="LGOCV") {

    # This follows the suggestion by Max Kuhn (https://stats.stackexchange.com/questions/109340/leave-one-subject-out-cross-validation-in-caret)
    # and https://stats.stackexchange.com/questions/68472/lgocv-caret-package-r
    sites <- unique(df[[ nam_group ]]) # 'nam_group' is site in this case (i.e. split the data by sparing data from one site for testing)
    group_folds <- vector(mode = "list", length = length(sites))
    for (i in seq_along(sites)) group_folds[[i]] <- which(df[[nam_group]] != sites[i])
    names(group_folds) <- paste0("Subject", sites)
    
    traincotrlParams <- trainControl(method = "LGOCV",
                                     # summaryFunction = twoClassSummary,
                                     classProbs = FALSE,
                                     index = group_folds,
                                     savePredictions = TRUE
                                     )
    
  } else if (train_method=="repeatedcv") {

    # Repeated cross validation:
    # Take best of 'prepeats' repetitions of training with 75% used for training (25% for testing), using 
    # 'number' different data splits
    traincotrlParams <- caret::trainControl( method="repeatedcv", number=5, repeats=5, verboseIter=FALSE, p=0.75 )

  }

  ## Pre processing (scaling)
  if (classification){
    metric <- "Accuracy"
  } else {
    metric <- "RMSE" 
  }
  
  # Training algorithm parameter sampling
  if (tune){
    if (method=="rf"){
      tune_grid <- expand.grid( .mtry=c(1:length( predictors )) ) # , ntree=c(500, 750, 1000) or use the Custom RF
    } else if (method=="nnet"){
      tune_grid <- expand.grid( .decay = c(0.01, 0.1, 0.5), .size = 3:20 )
    }
  } else {
    # tune_grid <- NULL
    if (method=="rf"){
      tune_grid <- expand.grid( .mtry=c(length( predictors )) ) # , ntree=c(500, 750, 1000) or use the Custom RF
    } else if (method=="nnet"){
      tune_grid <- expand.grid( .decay = c(0.01), .size = 4 )
    }
  }
  
  ## pre-process
  if (method=="nnet"){
    
    myrecipe <- recipe(forml, data = df) %>%
      step_center(all_numeric(), -all_outcomes()) %>%
      step_scale(all_numeric(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)
    
  } else if (method=="rf"){
    
    myrecipe <- recipe(forml, data = df)
    
  }
  
  # Execute the training
  # saved predictions of the final model ('final' w.r.t. tune-grid) are saved in .$pred
  set.seed(seed)
  print("training full model ...")
  modl <- caret::train(
    myrecipe,
    data            = df,
    method          = method,
    metric          = metric,
    tuneGrid        = tune_grid,
    # preProcess      = pre_process,
    trControl       = traincotrlParams,
    trace           = FALSE,
    importance      = TRUE
    )
  
  # save(modl, file = "./data/modl.Rdata")
  
  # predict
  modl$pred <- predict(modl, newdata = df)
  modl$obs  <- df[[nam_target]]
  
  # if (inner){
  #   # Check if it makes sense
  #   wrap_ml_inner <- function(df, rf_main, predictors, nam_target, forml, isite){
  #     
  #     print(paste("training model with leave-site-out:", isite, "..."))
  #     
  #     # split data
  #     df_train <-  df %>% dplyr::filter(site != isite)
  #     if (all){
  #       df_test <- df
  #     } else {
  #       df_test <- df %>% dplyr::filter(site == isite)
  #     }
  #     
  #     # train
  #     sites <- df_train[[ nam_group ]] %>% unique()
  #     n_sites <- sites %>% length()
  #     group_folds <- caret::groupKFold( df_train[[ nam_group ]], k = n_sites )
  #     traincotrlParams <- caret::trainControl( index = group_folds, method = "cv" )
  # 
  #     set.seed(seed)
  #     rf <- caret::train(
  #       forml,
  #       data            = df_train,
  #       method          = method,
  #       metric          = metric,
  #       tuneGrid        = tune_grid,
  #       preProcess      = pre_process,
  #       trControl       = traincotrlParams,
  #       trace           = FALSE,
  #       importance      = TRUE,
  #       savePredictions = "final"
  #     )
  #     
  #     # predict
  #     rf$pred <- predict(rf, newdata = df_test, preProcess = pre_process)
  #     rf$obs  <- df_test[[nam_target]]
  #     
  #     rf$pred_main <- predict(rf_main, newdata = df_test, preProcess = pre_process)
  #     
  #     # summarise
  #     if (classification){
  #       
  #       ## calculate confusion matrix and add to output
  #       df_tmp <- tibble(
  #         obs = as.factor(df_test[[nam_target]]), 
  #         mod = as.factor(as.vector(rf$pred))
  #         )
  #       rf$cm <- confusionMatrix( data = df_tmp$mod, reference = df_tmp$obs )
  #       
  #       ## get Accuracy
  #       rf$myaccuracy <- rf$cm$overall["Accuracy"]
  #       
  #       ## get Kappa
  #       rf$mykappa <- rf$cm$overall["Kappa"]
  #       
  #     } else {
  #       
  #       ## calculate RMSE
  #       rf$myrmse <- tibble(obs = df_test[[nam_target]], mod = as.vector(rf$pred)) %>% 
  #         yardstick::rmse(obs, mod) %>% 
  #         dplyr::select(.estimate) %>% 
  #         dplyr::pull()
  # 
  #       ## calculate R-squared
  #       rf$myrsq <- tibble(obs = df_test[[nam_target]], mod = as.vector(rf$pred)) %>% 
  #         yardstick::rsq(obs, mod) %>% 
  #         dplyr::select(.estimate) %>% 
  #         dplyr::pull()
  #       
  #     }
  #     
  #     return(rf)      
  #   }
  #   
  #   list_rf <- purrr::map(as.list(sites), ~wrap_ml_inner(df, rf, predictors, nam_target, forml, .))
  #   names(list_rf) <- sites
  #   
  #   ## Follow "Gavin method": 
  #   ## produce a model ensemble where each model is trained on a slightly different dataset due to the different
  #   ## LOSO groups (this part is done inside the inner loop), then we take the median prediction of the ensemble 
  #   ## as the best for the flux.
  #   
  #   ## Now, average across predictions (take most frequent for categorical variables)
  #   if (classification){
  #     ## Categorical data
  #     get_mod_from_inner <- function(rf){
  #       out <- tibble(mod = as.logical(rf$pred))
  #       return(out)
  #     }
  #     get_most_frequent <- function(vec){
  #       ## warning: if equal number of times false and true, then false is returned
  #       out <- sort(table(vec), decreasing=TRUE)[1] %>% 
  #         names() %>% 
  #         as.logical()
  #       return(out)
  #     }
  #     df_gavin <- purrr::map_dfc(list_rf, ~get_mod_from_inner(.)) %>% 
  #       mutate(mod_average = apply(., 1, get_most_frequent)) %>% 
  #       mutate(obs = list_rf[[1]]$obs )
  #     rf$pred_median_inner <- df_gavin$mod_average
  #     
  #     ## evaluate, based on confusion matrix
  #     rf$cm_median_inner <- confusionMatrix( 
  #       data = as.factor(df_gavin$mod_average), 
  #       reference = as.factor(df_gavin$obs) )
  #     
  #   } else {
  #     ## Continuous data
  #     get_modobs_from_inner <- function(rf){
  #       out <- tibble(mod = rf$pred, obs = rf$obs)
  #       return(out)
  #     }
  #     df_gavin <- purrr::map_dfc(list_rf, ~get_modobs_from_inner(.)) %>% 
  #       mutate(mod_average = apply(., 1, median)) %>% 
  #       mutate(obs = list_rf[[1]]$obs )
  # 
  #     ## calculate RMSE
  #     rf$myrmse <- df_gavin %>% 
  #       yardstick::rmse(obs, mod_average) %>% 
  #       dplyr::select(.estimate) %>% 
  #       dplyr::pull()
  #     
  #     ## calculate R-squared
  #     rf$myrsq <- df_gavin %>% 
  #       yardstick::rsq(obs, mod_average) %>% 
  #       dplyr::select(.estimate) %>% 
  #       dplyr::pull()
  #     
  #   }
  #   
  #   
  # } else {
  #   
  #   list_rf <- NA
  # 
  # }

  # save(list_rf, file = "./data/list_rf.Rdata")
  
  # pred_test <- predict(rf, newdata = df) # , preProc = c("center", "scale")
  # all.equal(rf$pred, pred_test) # TRUE !!!! luckily
  # 
  # # get predictions if not saved
  # if (is.null(rf$pred)){
  #   rf$pred <- predict(rf, newdata = df, preProc = c("center", "scale"))
  #   }
  # 
  # if (classification){
  #   # get confusion matrix -- weird, it's 100% accuracy
  #   cm <- caret::confusionMatrix( 
  #     table( as.vector( rf$pred ), as.factor( df[[ nam_target ]] ) ),
  #     prevalence = sum(as.logical(df[[ nam_target ]])) / nrow(df)
  #     )
  # 
  #   # # get confusion matrix (from https://stackoverflow.com/questions/46816174/confusion-matrix-for-random-forest-in-r-caret)
  #   # cm <- confusionMatrix( rf$pred[order( rf$pred$rowIndex ),2], df[[ nam_group ]] )
  # 
  #   }
  # 
  # 
  # if (length(test$is_flue_drought)!=0){ # Need to have all levels, if not error
  #   
  #   test <- rbind(train[1, ] , test)
  #   test <- ddf_test [-1,]
  #   
  #   pred <- predict(rf, newdata = test, preProc = "range")
  #   
  #   # CLASIFICACION
  #   pred <- as.vector( pred ) 
  #   
  #   # Compare predicted outcome and true outcome
  #   cm <- confusionMatrix(table(pred, as.factor(test$is_flue_drought)))
  #   ov <- cm$overall
  #   importance <- importance(rf)
  #   
  # }
  # return(list(clasification = ov, predicted = pred, observed = test$is_flue_drought, randomForest = rf, importance = importance))
  # return(list(rf = rf, list_rf = list_rf))
  return(modl)
  }