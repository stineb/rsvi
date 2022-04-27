# RANDOM FOREST FUNCTION
# XXX comment: argument declaration should use values directly, not objects
wrap_ml <- function(df, nam_target, predictors, nam_group = "site", train_method="myLGOCV", 
                    method = "rf", tune = FALSE, seed = 1982, classification = TRUE, inner = FALSE, cores = 1){
  # implement parallel processing
  c1 <- makePSOCKcluster(cores)
  registerDoParallel(c1) 

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
    # Data points from different groups should be not in both training and validation sets. 
    # spits should be made along group delineations (groupFold helps here)
    group_folds <- caret::groupKFold( df[[ nam_group ]], k = n_sites )
    traincotrlParams <- caret::trainControl( index = group_folds, 
                                             # method = "repeatedcv", 
                                             # number = 5, 
                                             method = "cv",
                                             number = n_sites,
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
      tune_grid <- expand.grid( .mtry=c(1:length( predictors )) ) # ntree=c(500, 750, 1000) or use the Custom RF
    } else if (method=="nnet"){
      tune_grid <- expand.grid( .decay = c(0.01, 0.1, 0.5), .size = 3:20 )
    }
  } else {
    # tune_grid <- NULL
    if (method=="rf"){
      tune_grid <- expand.grid( .mtry=c(length( predictors )) ) # ntree=c(500, 750, 1000) or use the Custom RF
    } else if (method=="nnet"){
      if (classification){
        tune_grid <- expand.grid( .decay = c(0.01), .size = 4 )
      } else {
        tune_grid <- expand.grid( .decay = c(0.01), .size = 8 )
      }
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

  
  if (inner){
    # Check if it makes sense
    wrap_ml_inner <- function(df, isite, myrecipe, nam_group, nam_target, metric, method, tune_grid){
      
      print(paste("training model with leave-site-out:", isite, "..."))
      
      ## leave data from single site out for training
      df_train <-  df %>% dplyr::filter(site != isite)
      df_test <- df %>% dplyr::filter(site == isite)
      
      ## train, corresponding to myLGOCV
      n_sites <- df_train %>% pull(site) %>% unique() %>% length()
      group_folds <- caret::groupKFold( df_train[[ nam_group ]], k = n_sites )
      traincotrlParams <- caret::trainControl( index = group_folds, 
                                               # method = "repeatedcv", 
                                               # number = 5, 
                                               method = "cv",
                                               number = n_sites,
                                               savePredictions = "final"   # predictions on each validation resample are then available as modl$pred$Resample
                                               )

      ## train model
      set.seed(seed)
      modl <- caret::train(
        myrecipe,
        data            = df_train,
        method          = method,
        metric          = metric,
        tuneGrid        = tune_grid,
        trControl       = traincotrlParams,
        trace           = FALSE,
        importance      = TRUE
        )
      
      # predict at left-out site
      df_test$pred <- predict(modl, newdata = df_test)
      
      # summarise
      if (classification){
        tmp <- postResample(pred = df_test$pred, obs = df_test[[nam_target]])

        results <- list( 
          accuracy = tmp[1],
          kappa = tmp[2],
          cm = confusionMatrix(data = df_test$pred, reference = df_test[[nam_target]]) 
          )

      } else {
        
        results <- yardstick::metrics(df_test, nam_target, "pred") %>% 
          mutate(leftout = isite)
          
      }
      
      return(list(results = results, df_test = df_test, modl = modl))  

    }
    
    out_inner <- purrr::map(as.list(sites), ~wrap_ml_inner(df, ., myrecipe, nam_group, nam_target, metric, method, tune_grid))
    names(out_inner) <- sites

    return(out_inner)

  } else {

    # Execute the training
    set.seed(seed)
    print("training full model ...")
    modl <- caret::train(
      myrecipe,
      data            = df,
      method          = method,
      metric          = metric,
      tuneGrid        = tune_grid,
      trControl       = traincotrlParams,
      trace           = FALSE,
      importance      = TRUE
      )

    return(modl)

  }
  stopCluster(c1)
}
