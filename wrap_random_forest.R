# RANDOM FOREST FUNCTION
# XXX comment: argument declaration should use values directly, not objects
wrap_random_forest <- function(df, nam_target, predictors, nam_instance = "site", train_method="lgocv", tune = FALSE, seed = 1982, classification = TRUE){
  
  # Construct formula
  forml  <- as.formula(  paste( nam_target, "~", paste( predictors, collapse=" + " ) ) )
  
  # Specify data scaling
  preprocessParams <- caret::preProcess( df, method=c("range") )

  # Specify model training parameters  
  if (train_method=="lgocv"){

    # Leave-one-out cross validation
    # In this case, the 'train' data is the all the avilable data.
    # traincotrlParams <- caret::trainControl( method="LOOCV" )

    # This follows the tutorial on http://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/
    # Do a n_sites-fold cross validation leaving one site out in each "fold"
    sites <- df[[ nam_instance ]] %>% unique()
    n_sites <- sites %>% length()
    group_folds <- caret::groupKFold( df[[ nam_instance ]], k = n_sites )
    traincotrlParams <- caret::trainControl( index = group_folds, method = "cv" )
    metric <- "Accuracy"

    # # This follows the suggestion by Max Kuhn (https://stats.stackexchange.com/questions/109340/leave-one-subject-out-cross-validation-in-caret)
    # subs <- unique(train[[ nam_instance ]]) # 'nam_instance' is site in this case (i.e. split the data by sparing data from one site for testing)
    # model_these <- vector(mode = "list", length = length(subs))
    # for (i in seq_along(subs)) model_these[[i]] <- which(train[[nam_instance]] != subs[i])
    # names(model_these) <- paste0("Subject", subs)
    # traincotrlParams <- caret::trainControl(method = "cv", index = model_these, classProbs =  TRUE)

    # Unclear: How does this work with the implemented method LGOCV?

  } else if (train_method=="repeatedcv") {

    # Repeated cross validation:
    # Take best of 'prepeats' repetitions of training with 75% used for training (25% for testing), using 
    # 'number' different data splits
    traincotrlParams <- caret::trainControl( method="repeatedcv", number=5, repeats=5, verboseIter=FALSE, p=0.75 )

  }

  ## Pre processing (scaling)
  if (classification){
    pre_process <- NULL
  } else {
    pre_process <- c("center", "scale")
  }

  
  # Training algorithm parameter sampling
  if (tune){
    tune_grid <- expand.grid( .mtry=c(1:length( predictors )) ) # , ntree=c(500, 750, 1000) or use the Custom RF
  } else {
    tune_grid <- expand.grid( .mtry = length( predictors ) )
    # tune_grid <- NULL # expand.grid(.mtry=4)
  }
  
  # Execute the training
  # saved predictions of the final model ('final' w.r.t. tune-grid) are saved in .$pred
  set.seed(seed)
  rf <- caret::train(
    forml,
    data            = df,
    method          = "rf",
    metric          = metric,
    tuneGrid        = tune_grid,
    preProcess      = pre_process,
    trControl       = traincotrlParams,
    trace           = FALSE,
    importance      = TRUE,
    savePredictions = "final"
    )

  # Check if it makes sense
  calc_accuracy <- function(df, rf, predictors, nam_target, forml, isite){
    
    # split data
    train <-  df %>% dplyr::filter(site != isite)
    test <- df %>% dplyr::filter(site == isite)
    
    # train
    sites <- train[[ nam_instance ]] %>% unique()
    n_sites <- sites %>% length()
    group_folds <- caret::groupKFold( train[[ nam_instance ]], k = n_sites )
    traincotrlParams <- caret::trainControl( index = group_folds, method = "cv" )
    metric <- "Accuracy"
    tune_grid <- expand.grid( .mtry = length( predictors ) )
    
    set.seed(seed)
    rf <- caret::train(
      forml,
      data            = train,
      method          = "rf",
      metric          = metric,
      tuneGrid        = tune_grid,
      preProcess      = pre_process,
      trControl       = traincotrlParams,
      trace           = FALSE,
      importance      = TRUE,
      savePredictions = "final"
      )

    # predict
    pred_test <- predict(rf, newdata = test)

    # summarise
    df_test <- tibble(obs = test[[nam_target]], mod = pred_test) %>% 
      dplyr::mutate(good = obs==mod) %>% 
      dplyr::summarise(good = sum(good))
    rf$myaccuracy <- df_test$good / nrow(test)

    return(rf)      
  }
  
  vec_rf <- purrr::map(as.list(sites), ~calc_accuracy(df, rf, predictors, nam_target, forml, .))
  vec_accuracy <- purrr::map_dbl(vec_rf, "myaccuracy")
  accuracy <- mean(vec_accuracy)
  
  pred_test <- predict(rf, newdata = df) # , preProc = c("center", "scale")
  all.equal(rf$pred, pred_test) # TRUE !!!! luckily
  
  # get predictions if not saved
  if (is.null(rf$pred)){
    rf$pred <- predict(rf, newdata = df, preProc = c("center", "scale"))
    }
  
  if (classification){
    # get confusion matrix -- weird, it's 100% accuracy
    cm <- caret::confusionMatrix( 
      table( as.vector( rf$pred ), as.factor( df[[ nam_target ]] ) ),
      prevalence = sum(as.logical(df[[ nam_target ]])) / nrow(df)
      )

    # # get confusion matrix (from https://stackoverflow.com/questions/46816174/confusion-matrix-for-random-forest-in-r-caret)
    # cm <- confusionMatrix( rf$pred[order( rf$pred$rowIndex ),2], df[[ nam_instance ]] )

    }
  
  
  if (length(test$is_flue_drought)!=0){ # Need to have all levels, if not error
    
    test <- rbind(train[1, ] , test)
    test <- ddf_test [-1,]
    
    pred <- predict(rf, newdata = test, preProc = "range")
    
    # CLASIFICACION
    pred <- as.vector( pred ) 
    
    # Compare predicted outcome and true outcome
    cm <- confusionMatrix(table(pred, as.factor(test$is_flue_drought)))
    ov <- cm$overall
    importance <- importance(rf)
    
  }
  return(list(clasification = ov, predicted = pred, observed = test$is_flue_drought, randomForest = rf, importance = importance))
}