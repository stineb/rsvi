Modelling of flue and is_flue_drought with MODIS data and the land surface temperature

There are 3 main parts added for the modelling of flue based on remotely sensed reflectance data and the land-surface temperature. 

- `00_ImportData.ipynb` creates `'./data/df_lst_and_refl.csv'` which combines MODOCGA (bands 8-16, atm. corr., daily, 1 km) and MOD09GA1 (bands 1-7, atm. corrected, 500 m, daily) (surface reflectance), and MOD11A1 and MYD11A1 (LST), used for subsequent analyses. 
- `01_QualityControl.ipynb`: The quality control of the data. Filter based on quality control. Derives inidces based on reflectances. Creates file `./data/FLUXNET_MODOCGA_MOD09GA_MYD11A1_MOD11A1.csv`
- `02_MissingData.ipynb`: The handelling of the missing data, complement with fLUE, filter to homogenous sites (`sites2.csv`), filling gaps with mean seasonal cycle determined on 9 days around day of interest (best results with interpolation, not using seasonal mean for filling gaps). Creates file `data/FLUXNET_MODOCGA_MOD09GA_MYD11A1_MOD11A1_{method}_{method_gap}_maxgap_{maxgap}_{n_maxgap}.csv` (see Tab. 8 in Martina Buck's thesis for explanation).

Best results:
	
	method_fill = "interpolation"
	method_gap = "remain_NA"
	maxgap = 2
	n_maxgap = 9 (egal da kein seasonal mean)

- `03_Analysis.ipynb`: The NDI/fLUE correlations, the clustering of sites based on the NDI and the modelling of fLUE/is_flue_drought. 


