#### Code for grouping sites by proximity (for folds along groups)
# redefine site.names variable, after wetland subsetting
site.names <- train %>% select("ID") %>% distinct() %>% pull()

# create a tibble where each site name row is assocaited  with its adjacent 'close by' sites
#  this is clunky bit of coding, could try to automate based a LAT LON threshold
close.sites <- list()
close.sites[[1]] <- c("BCFEN","YFBsf") #BCBog
close.sites[[2]] <- c("BCBog","YFBsf") #BCFEN
close.sites[[3]] <- c("CASCC") #CASCB
close.sites[[4]] <- c("CASCB") #CASCC
close.sites[[5]] <- c("DESfN") #placeholder (i.e. no close sites, site removed as validation site)
close.sites[[6]] <- c("DEZrk") #placeholder
close.sites[[7]] <- c("FILom") #placeholder
close.sites[[8]] <- c("FISi2") #FISi1
close.sites[[9]] <- c("FISi1") #FISi2
close.sites[[10]] <- c("JPBBY") #placeholder

close.sites[[11]] <- c("MYMLM") #MYMLM
close.sites[[12]] <- c("NZKop") #placeholder
close.sites[[13]] <- c("RUChe") #RUCh2
close.sites[[14]] <- c("RUCh2") #RUChe
close.sites[[15]] <- c("RUSAM") #placeholder
close.sites[[16]] <- c("RUVrk") #placeholder
close.sites[[17]] <- c("SEDeg") #placeholder
close.sites[[18]] <- c("SESto") #SESt1
close.sites[[19]] <- c("SESt1") #SESto
close.sites[[20]] <- c("USAtq") #placeholder
close.sites[[21]] <- c("USBes", "USNGB","USBrw") # USBeo

# close.sites[[22]] <- c("USBgl") #placeholder
close.sites[[22]] <- c("USFwm") #USBms
close.sites[[23]] <- c("USBms") #USFwm
close.sites[[24]] <- c("USIcs") #placeholder
close.sites[[25]] <- c("USIvo") #placeholder
close.sites[[26]] <- c("USLos") #placeholder
close.sites[[27]] <- c("USSne","USSnd","USBi1","USTwt","USBi2") #USMyb
close.sites[[28]] <- c("USNC4") #placeholder
close.sites[[29]] <- c("USBeo","USBes","USBrw") #USNGB

close.sites[[30]] <- c("USNGC") #placeholder
close.sites[[31]] <- c("USORv", "USOWC") #placeholder
close.sites[[32]] <- c("USWPT", "USCRT", "USORv") # USOWC
close.sites[[33]] <- c("USMyb","USSnd","USBi1","USBi2") #USSne
close.sites[[34]] <- c("USStj") #placeholder
close.sites[[35]] <- c("USSnd","USBi1","USTw4","USTwt","USBi2") #USTw1
close.sites[[36]] <- c("USSnd","USBi1","USTw1","USTwt","USBi2") #USTw4
close.sites[[37]] <- c("USOWC", "USCRT", "USORv") #USWPT

close.sites.list <- list()
close.sites.list <- as_tibble(cbind(sites = as.character(site.names), proximate = close.sites))

# create folds for LOOCV without proximate sites
folds_train <- list()
for (i in 1:length(site.names)) {
  folds_train[[i]] <- ungroup(train_x) %>%
    mutate(IDX = 1:n()) %>%
    filter(!ID %in% c(close.sites.list$proximate[[i]], as.character(site.names[i]))) %>%
    select("IDX") %>% pull()
}

# [...]

##### Code for training RF models
## set up lists (for rf)
tgrid <- list()
myControl <- list()

## Create tune-grid
tgrid <- expand.grid(
  .mtry = c(5,8,10),
  .splitrule = "variance",
  .min.node.size = c(2,10,20)
)

## Create trainControl object
myControl <- trainControl(
  method = "oob",
  classProbs = FALSE,
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = folds_train
)

## train rf on folds
rf_model <- list()
for (i in 1:length(site.names)){
  rf_model[[i]] <- train(
    x = train_x[folds_train[[i]],],
    y = train_y[folds_train[[i]]],
    method = 'ranger',
    trControl = myControl,
    tuneGrid =tgrid,
    num.trees = 100,
    importance = 'permutation'
  )
  print(i)
}

## save/output model structure
saveRDS(rf_model, paste(model_output,"/190530_rf_wetlands_monthly_MET_subset.rds",sep=""))


#### Bind all predictions for each LOSOCV site
# get all predictions
rf.pred <- list()
for (i in 1:length(rf_model)) {
  rf.pred[[i]] <- train %>%
    filter(ID == site.names[i]) %>%  
    mutate(FCH4P = predict(rf_model[[i]], .))
}
rf.pred.all <- bind_rows(rf.pred)