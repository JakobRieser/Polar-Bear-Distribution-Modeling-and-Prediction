polar_projection = CRS("+init=epsg:3995")

gammap_polar <- projectRaster(gammap, crs=polar_projection, method="bilinear")

plot(gammap_polar)

rm(gammap_df)
