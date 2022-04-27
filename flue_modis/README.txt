This folder contains datasets that can be used in the analysis of the master thesis "Machine learning-enabled detection of soil moisture stress on the physiology of photosynthesis using remotely sensed surface reflectance".

The csv files contain data from different FLUXNET sites such as:
-  the MODIS reflectance data of the bands one to fourteen of the products MODOCGA and MOD09GA
-  the landsurface temperature of the products MYD11A1 and MOD11A1
-  the air temperature and landcovertype 
-  the target variables flue and is_flue_drought

The remotely sensed data passed a quality control and missing data was filled for the two files:
SmallGaps_2_interpolation_LargeGaps_remain_NA_ifSeasonalMean_ndays_5
SmallGaps_2_interpolation_LargeGaps_seasonal_mean_ifSeasonalMean_ndays_9
Where small gaps of max two consecutive days were interpolated and for larger Gaps the data was either deleted or a seasonal mean over 9 days was calculated. 

raw is the csv resulting from no further cleaning of the data after quality control.
