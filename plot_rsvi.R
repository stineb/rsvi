plot_rsvi_bysite <- function( sitename, ddf ){

	require(dplyr)

	with( dplyr::filter( ddf, site==sitename), plot( date, ndvi, type="l" ) )

}