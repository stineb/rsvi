# RANDOM FOREST FUNCTION
# XXX comment: argument declaration should use values directly, not objects
wrap_ml <- function(df, nam_target, predictors, nam_group = "site", train_method="myLGOCV", 
                    method = "rf", tune = FALSE, seed = 1982, classification = TRUE, inner = FALSE){
  
  # Construct formula
  forml  <- as.formula(  paste( nam_target, "~", paste( predictors, collapse=" + " ) ) )
  
  # get groups (sites)
  sites <- df[[ nam_group ]] %>% unique()
  
  # Specify model training parameters  
  if (train_method=="myLGOCV"){

    # Leave-one-out cross validation
    # In this case, the 'train' data is the all the avilable data.
    # traincotrlParams <- caret::trainControl( method="LOOCV" )

    # This follows the tutorial on http://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/
    # Do a n_sites-fold cross validation leaving one site out in each "fold"
    n_sites <- sites %>% length()
    group_folds <- caret::groupKFold( df[[ nam_group ]], k = n_sites )
    traincotrlParams <- caret::trainControl( index = group_folds, method = "cv" )

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
                                     savePredictions = TRUE)
    
  } else if (train_method=="repeatedcv") {

    # Repeated cross validation:
    # Take best of 'prepeats' repetitions of training with 75% used for training (25% for testing), using 
    # 'number' different data splits
    traincotrlParams <- caret::trainControl( method="repeatedcv", number=5, repeats=5, verboseIter=FALSE, p=0.75 )

  }

  ## Pre processing (scaling)
  if (classification){
    metric <- "Accuracy"
    pre_process <- NULL
  } else {
    metric <- "RMSE" 
    pre_process <- c("center", "scale")
  }
  # # Specify data scaling
  # preprocessParams <- caret::preProcess( df, method=c("range") )
  # 
  
  # Training algorithm parameter sampling
  if (tune){
    if (method=="rf"){
      tune_grid <- expand.grid( .mtry=c(1:length( predictors )) ) # , ntree=c(500, 750, 1000) or use the Custom RF
    } else if (method=="nnet"){
      tune_grid <- expand.grid( .decay = c(0.1), .size = seq(11) )
    }
  } else {
    # tune_grid <- NULL
    if (method=="rf"){
      tune_grid <- expand.grid( .mtry=c(length( predictors )) ) # , ntree=c(500, 750, 1000) or use the Custom RF
    } else if (method=="nnet"){
      tune_grid <- expand.grid( .decay = c(0.1), .size = 4 )
    }
  }
  
  # Execute the training
  # saved predictions of the final model ('final' w.r.t. tune-grid) are saved in .$pred
  set.seed(seed)
  print("training full model ...")
  rf <- caret::train(
    forml,
    data            = df,
    method          = method,
    metric          = metric,
    tuneGrid        = tune_grid,
    preProcess      = pre_process,
    trControl       = traincotrlParams,
    trace           = FALSE,
    importance      = TRUE,
    savePredictions = "final"
    )
  # save(rf, file = "./data/rf.Rdata")
  
  # predict
  rf$pred <- predict(rf, newdata = df)
  rf$obs  <- df[[nam_target]]
  
  if (inner){
    # Check if it makes sense
    wrap_ml_inner <- function(df, rf_main, predictors, nam_target, forml, isite){
      
      print(paste("training model with leave-site-out:", isite, "..."))
      
      # split data
      df_train <-  df %>% dplyr::filter(site != isite)
      df_test <- df %>% dplyr::filter(site == isite)
      
      # train
      sites <- df_train[[ nam_group ]] %>% unique()
      n_sites <- sites %>% length()
      group_folds <- caret::groupKFold( df_train[[ nam_group ]], k = n_sites )
      traincotrlParams <- caret::trainControl( index = group_folds, method = "cv" )

      set.seed(seed)
      rf <- caret::train(
        forml,
        data            = df_train,
        method          = method,
        metric          = metric,
        tuneGrid        = tune_grid,
        preProcess      = pre_process,
        trControl       = traincotrlParams,
        trace           = FALSE,
        importance      = TRUE,
        savePredictions = "final"
      )
      
      # predict
      rf$pred <- predict(rf, newdata = df_test, preProcess = pre_process)
      rf$obs  <- df_test[[nam_target]]
      
      rf$pred_main <- predict(rf_main, newdata = df_test, preProcess = pre_process)
      
      # summarise
      if (classification){
        df_sum <- tibble(obs = df_test[[nam_target]], mod = as.vector(rf$pred)) %>% 
          dplyr::mutate(good = obs==mod) %>% 
          dplyr::summarise(good = sum(good))
        rf$myresults <- df_sum$good / nrow(df_test)
      } else {
        df_sum <- tibble(obs = df_test[[nam_target]], mod = as.vector(rf$pred)) %>% 
          analyse_modobs2(mod = "mod", obs = "obs")
        rf$myresults <- sqrt( mean( ( df_sum$mod - df_sum$obs )^2 ) )
        
      }
      
      return(rf)      
    }
    
    list_rf <- purrr::map(as.list(sites), ~wrap_ml_inner(df, rf, predictors, nam_target, forml, .))
    names(list_rf) <- sites
    
  } else {
    
    list_rf <- NA

  }

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
  return(list(rf = rf, list_rf = list_rf))
  }