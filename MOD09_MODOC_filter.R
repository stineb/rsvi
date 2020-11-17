MOD09_MODOC_filter <- function(dir_raw, QC500_filter = TRUE){

  library(lubridate)
  library(tidyverse)
  library(binaryLogic)

  #### MODIS productos MOD09GA and MODOCGA by site#####
  # path <- "C:/Users/Paula/Desktop/Pau/Ecologia terrestre/rsvi/data/FLUXNET_MODOCGA_MOD09GA1km_2000_2018/"
  # dir_raw <- path
    
  # Filter data:
  # Adri?: "Como bien dices, los bits 12 a 19 de la banda QC_b8_15_1km tienen que ser iguales a 0. 
  # Estos son flags espec?ficos de las bandas 11 y 12 (para el c?lculo del PRI). 
  # Igualmente, si utilizas la banda 1 (para el CCI) deber?as filtrar con los bits 2 a 5 de la banda QC_500m. 
  # Una vez aplicado este filtro, se aplica otro filtro con los bits 0  1  2  8  9  10  12  y 13 de la banda state_1km. 
  # Dicho de otro modo, el filtro de state_1km se aplica a todas las bandas, tanto del producto MODOCGA como MOD09GA".

  # ?Filter by QC_500m? true

  # Creates a unique data frame: 
  # READ DATA (.RData) LINE 35 with the "data/MODOCGA_MOD09GA_1km_raw.Rdata" with the output of this lines ("data")
  files <- list.files(dir_raw, full.names = T)
  lfiles <- length(files)
  namefiles <- list.files(dir_raw, full.names = F)
  namesites <- substr(namefiles,1,6)

  # Raw data
  data <- NULL
  for (i in 1:lfiles){
    site <- read.csv(files[i], header=F, stringsAsFactors = FALSE)
    colnames(site) <- as.character(site[1,])
    site <- site[c(2:nrow(site)),c(-ncol(site),(1-ncol(site)))]
    data <- rbind(data,site)
  }
  
  # ## much faster read-in
  # data <- purrr::map_dfr(as.list(files), ~read_csv(.))

  # load("data/MODOCGA_MOD09GA_1km_raw.Rdata")
  data <- as_tibble(data)  %>% 
    mutate_at(.vars = vars(matches("sur_refl", ignore.case=FALSE)),funs(as.numeric)) # Si es necesario, a veces lee csv como character

  ####  MOD09 ####
  mod09 <- data %>% 
    dplyr::select(YY, MM, DD,  site_num, sites_id, QC_500m, state_1km, 
                  sur_refl_b01, sur_refl_b02, sur_refl_b03, sur_refl_b04, sur_refl_b05, sur_refl_b06, sur_refl_b07, 
                  SolarZenith, SensorZenith, SolarAzimuth, SensorAzimuth) 

  #### Quality ####
  qflags_500 <- sort(unique(mod09$QC_500m))    # 32 bits

  #### pasar a binarios la capa de calidad
  QC_Data_500 <- data.frame(Integer_Value = qflags_500,  
                        Bit31 = NA, 
                        Bit30 = NA, Bit29 = NA, Bit28 = NA, Bit27 = NA, Bit26 = NA, Bit25 = NA, Bit24 = NA, Bit23 = NA, Bit22 = NA, Bit21 = NA, 
                        Bit20 = NA, Bit19 = NA, Bit18 = NA, Bit17 = NA, Bit16 = NA, Bit15 = NA, Bit14 = NA, Bit13 = NA, Bit12 = NA, Bit11 = NA,
                        Bit10 = NA, Bit9 = NA,  Bit8 = NA,  Bit7 = NA,  Bit6 = NA,  Bit5 = NA,  Bit4 = NA,  Bit3 = NA,  Bit2 = NA,  Bit1 = NA,
                        Bit0 = NA)

  r <- 0
  for (i in QC_Data_500$Integer_Value){
    AsInt <- as.integer(intToBits(i))
    # AsInt <- as.integer(as.logical(as.binary(i, n=32, logic=F)))
    QC_Data_500[r+1,2:33]<- AsInt[32:1]
    r <- r+1
  }

  ##### FILTER DATA
  if (QC500_filter){
    # Filter by QC_500m
    my_data <- as_tibble(QC_Data_500)

    # columns with the bits of interest: bit 2:9
    b1 <- c(28:31) 
    b2 <- c(24:27)
    b3 <- c(20:23)
    filter_qflags_qc_b1 <- my_data %>% select(1,b1) %>% filter_at(vars(-Integer_Value), all_vars(.== 0)) %>% select(1)
    filter_qflags_qc_b2 <- my_data %>% select(1,b2) %>% filter_at(vars(-Integer_Value), all_vars(.== 0)) %>% select(1)
    filter_qflags_qc_b3 <- my_data %>% select(1,b3) %>% filter_at(vars(-Integer_Value), all_vars(.== 0)) %>% select(1)

    mod09$sur_refl_b01[!mod09$QC_500m %in% filter_qflags_qc_b1$Integer_Value] <- NA
    mod09$sur_refl_b02[!mod09$QC_500m %in% filter_qflags_qc_b2$Integer_Value] <- NA
    mod09$sur_refl_b03[!mod09$QC_500m %in% filter_qflags_qc_b3$Integer_Value] <- NA
  }

  #### MODOC ####
  modoc <- data %>% 
    dplyr::select(YY, MM, DD,  site_num, sites_id, QC_b8_15_1km, sur_refl_b11, sur_refl_b12) 

  #### Quality ####
  qflags_QCoc <- sort(unique(modoc$QC_b8_15_1km))# 32 bits

  QC_Data <- data.frame(Integer_Value = qflags_QCoc, # In an unsigned representation, these values are the integers between 0 and 65535
                        Bit31 = NA, 
                        Bit30 = NA, Bit29 = NA, Bit28 = NA, Bit27 = NA, Bit26 = NA, Bit25 = NA, Bit24 = NA, Bit23 = NA, Bit22 = NA, Bit21 = NA, 
                        Bit20 = NA, Bit19 = NA, Bit18 = NA, Bit17 = NA, Bit16 = NA, Bit15 = NA, Bit14 = NA, Bit13 = NA, Bit12 = NA, Bit11 = NA,
                        Bit10 = NA, Bit9 = NA,  Bit8 = NA,  Bit7 = NA,  Bit6 = NA,  Bit5 = NA,  Bit4 = NA,  Bit3 = NA,  Bit2 = NA,  Bit1 = NA,
                        Bit0 = NA)

  r <- 0
  for (i in as.numeric(QC_Data$Integer_Value)){
    # AsInt <- as.integer(intToBits(i))
    AsInt <- as.integer(as.logical(as.binary(i, n=32, logic=F)))
    QC_Data[r+1,2:33]<- AsInt[1:32]
    r <- r+1
  }

  # Filter QC_Data
  my_data <- as_tibble(QC_Data)

  # columns with the bits of the bands (bits N?12 to 19): 12-15b11, 16-19 b12
  b11 <- c(18:21) 
  b12 <- c(14:17)
  filter_qflags_qc_b11 <- my_data %>% select(1,b11) %>% filter_at(vars(-Integer_Value), all_vars(.== 0)) %>% select(1)
  filter_qflags_qc_b12 <- my_data %>% select(1,b12) %>% filter_at(vars(-Integer_Value), all_vars(.== 0)) %>% select(1)

  modoc$sur_refl_b11[!modoc$QC_b8_15_1km %in% filter_qflags_qc_b11$Integer_Value] <- NA
  modoc$sur_refl_b12[!modoc$QC_b8_15_1km %in% filter_qflags_qc_b12$Integer_Value] <- NA

  #### MERGE MOD09 y MODOC ####
  filter_data <- merge(mod09, modoc, by=c("YY","MM","DD","sites_id","site_num"),all = T)

  # Filter by state_1km
  qflags_state <- sort(unique(filter_data$state_1km)) # 16 bits

  state_Data <- data.frame(Integer_Value = qflags_state, # In an unsigned representation, these values are the integers between 0 and 65535
                           Bit15 = NA, Bit14 = NA, Bit13 = NA, Bit12 = NA, Bit11 = NA,
                           Bit10 = NA, Bit9 = NA,  Bit8 = NA,  Bit7 = NA,  Bit6 = NA,  
                           Bit5 = NA,  Bit4 = NA,  Bit3 = NA,  Bit2 = NA,  Bit1 = NA,
                           Bit0 = NA)
  r <- 0
  for (i in state_Data$Integer_Value){
    AsInt <- as.integer(intToBits(i)[1:16])
    # AsInt <- as.integer(as.logical(as.binary(i, n=16, logic=F)))
    state_Data[r+1,2:17]<- AsInt[16:1]
    r <- r+1
  }

  my_data <- as_tibble(state_Data)
  bits <- c(4,5,7:9,15:17) # 0  1  2  8  9  10  12  y 13
  filter_qflags <- my_data %>% select(1,bits) %>% 
    filter_at(vars(-Integer_Value), all_vars(.== 0)) %>% 
    select(1)

  filter_state_data <- filter_data %>% 
    filter_at(vars(state_1km), all_vars(.%in% filter_qflags$Integer_Value))

  ### Create Indices: NDVI, EVI, NIRv, CCI, PRI
  filter_state_data <- filter_state_data %>%
    mutate(ndvi = (sur_refl_b02 * 0.0001 - sur_refl_b01 * 0.0001)/(sur_refl_b02 * 0.0001 + sur_refl_b01 * 0.0001)) %>%
    mutate(evi = 2.5 * (sur_refl_b02 * 0.0001 - sur_refl_b01 * 0.0001) / (sur_refl_b02 * 0.0001 + 6 * sur_refl_b01 * 0.0001 - 7.5 * sur_refl_b03* 0.0001 + 1)) %>%
    mutate(NIRv = ndvi * (sur_refl_b02 * 0.0001)) %>%  #NIR
    mutate(cci = (sur_refl_b11 * 0.0001 - sur_refl_b01 * 0.0001)/(sur_refl_b11 * 0.0001 + sur_refl_b01 * 0.0001)) %>%
    mutate(pri = (sur_refl_b11 * 0.0001 - sur_refl_b12 * 0.0001)/(sur_refl_b11 * 0.0001 + sur_refl_b12 * 0.0001))
    # mutate(sPRI = (pri +1 )/2)  # %>%  # PRI solo escalado (0-1)
    # sPRIn: PRI normalizado by APAR y escalado (0-1) (Vicca 2016)
    # mutate(PRIn = PRI - PRI0) %>%  # PRI0 = the intercept of PRI vs APAR for a two-month window
    # mutate(sPRIn = (PRIn + 1)/2) 

  # Generate a CSV and a RData file
  # write.csv(filter_state_data, "data/MOD09GA_MODOCGA_filter_indices.csv", row.names=FALSE)

  return(as_tibble(filter_state_data))
}
