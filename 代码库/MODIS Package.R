# ##Installation

# ##stable release
# ##To install the current stable release use a CRAN repository:
# install.packages("MODISTools")
# library("MODISTools")

# ##development release
# ##To install the development releases of the package run the following commands:
# if(!require(remotes)){install.package("remotes")}
# remotes::install_github("bluegreen-labs/MODISTools")
# library("MODISTools")

# ##Vignettes are not rendered by default, if you want to include additional documentation please use:
# if(!require(remotes)){install.package("remotes")}
# remotes::install_github("bluegreen-labs/MODISTools", build_vignettes = TRUE, force = TRUE)

# Downloading MODIS time series
# To extract a time series of modis data for a given location and its direct environment use the mt_subset() function.

# load the library
library(MODISTools)

products <- mt_products()
View(products)

bands <- mt_bands(product = "MOD15A2H")
View(bands)

dates <- mt_dates(product = "MOD15A2H",
                  lat = 32,
                  lon = 118)
head(dates)
tail(dates)


# download the MODIS land cover (IGBP) and NDVI data
# for a region around the French city and basin of Arcachon
arcachon_lai <- mt_subset(product = "MOD15A2H",
                    lat = 44.656286,
                    lon =  -1.174748,
                    band = "Lai_500m",
                    start = "2004-01-01",
                    end = "2004-12-30",
                    km_lr = 20,
                    km_ab = 20,
                    site_name = "arcachon",
                    internal = TRUE,
                    progress = FALSE
                    )

arcachon_lc <- mt_subset(
  product = "MCD12Q1",
  lat = 44.656286,
  lon =  -1.174748,
  band = "LC_Type1",
  start = "2004-01-01",
  end = "2004-3-20",
  km_lr = 20,
  km_ab = 20,
  site_name = "arcachon",
  internal = TRUE,
  progress = FALSE
  )





# download data
subset <- mt_subset(
  product = "MOD11A2",
  lat = 40,
  lon = -110,
  band = "LST_Day_1km",
  start = "2004-01-01",
  end = "2004-02-01",
  km_lr = 1,
  km_ab = 1,
  site_name = "testsite",
  internal = TRUE,
  progress = FALSE
)
print(str(subset))

# The output format is a tidy data frame, as shown above. When witten to a csv
# with the parameter internal = FALSE this will result in a flat file on disk.

# Note that when a a region is defined using km_lr and km_ab multiple pixels 
# might be returned. These are indexed using the pixel column in the data frame 
# containing the time series data. The remote sensing values are listed in the 
# value column. When no band is specified all bands of a given product are 
# returned, be mindful of the fact that different bands might require different 
# multipliers to represent their true values. To list all available products, 
# bands for particular products and temporal coverage see function descriptions 
# below.

# Batch downloading MODIS time series
# When a large selection of locations is needed you might benefit from using the
# batch download function mt_batch_subset(), which provides a wrapper around the
# mt_subset() function in order to speed up large download batches. This function
# has a similar syntax to mt_subset() but requires a data frame defining site names
# (site_name) and locations (lat / lon) (or a comma delimited file with the same 
# structure) to specify a list of download locations.

# Below an example is provided on how to batch download data for a data frame of
# given site names and locations (lat / lon).

# create data frame with a site_name, lat and lon column
# holding the respective names of sites and their location

df <- data.frame("site_name" = paste("test", 1:2))
df$lat <- 40
df$lon <- -110

# test batch download
subsets <- mt_batch_subset(
  df = df,
  product = "MOD11A2",
  band = "LST_Day_1km",
  internal = TRUE,
  start = "2004-01-01",
  end = "2004-02-01"
)

print(str(subsets))
# Listing products
# To list all available products use the mt_products() function.

products <- mt_products()
head(products)
# Listing bands
# To list all available bands for a given product use the mt_bands() function.

bands <- mt_bands(product = "MOD11A2")
head(bands)
# listing dates
# To list all available dates (temporal coverage) for a given product and location use the mt_dates() function.

dates <- mt_dates(product = "MOD11A2",
                  lat = 42,
                  lon = -110)
head(dates)
# References
# Koen Hufkens. (2023). bluegreen - labs / MODISTools:MODISTools v1.1.5. Zenodo. 
# https:/ doi.org / 10.5281 / zenodo.7551164

library(dplyr)
library(ggplot2)
# merge land cover and lai data
arcachon <- arcachon_lc %>%
  rename("lc" = "value") %>%
  select("lc","pixel") %>%
  right_join(arcachon_lai, by = "pixel")

arcachon <- arcachon %>%
  filter(value <= 100,
         lc %in% c("1","5")) %>% # retain everything but fill values
  mutate(lc = ifelse(lc == 1, "ENF","DBF")) %>%
  group_by(lc, calendar_date) %>% # group by lc and date
  summarize(doy = as.numeric(format(as.Date(calendar_date)[1],"%j")),
            lai_mean = median(value * as.double(scale)))
#> `summarise()` has grouped output by 'lc'. You can override using the `.groups`
#> argument.
#> 
#> # plot LAI by date and per land cover class
ggplot(arcachon, aes(x = doy, y = lai_mean)) +
  geom_point() +
  geom_smooth(span = 0.3, method = "loess") +
  labs(x = "day of year (DOY)",
       y = "leaf area index (LAI)") +
  theme_minimal() +
  facet_wrap(~ lc)
#> `geom_smooth()` using formula = 'y ~ x'
#> 
#> 
#> 
#> 
#> 
#> # convert the coordinates
lat_lon <- sin_to_ll(arcachon_lc$xllcorner, arcachon_lc$yllcorner)

# bind with the original dataframe
subset <- cbind(arcachon_lc, lat_lon)

head(subset)
#>      xllcorner  yllcorner      cellsize nrows ncols     band units
#> 1.1 -111658.35 4946789.87 463.312716528    81    81 LC_Type1 class
#> 1.2 -111658.35 4946789.87 463.312716528    81    81 LC_Type1 class
#> 1.3 -111658.35 4946789.87 463.312716528    81    81 LC_Type1 class
#> 1.4 -111658.35 4946789.87 463.312716528    81    81 LC_Type1 class
#> 1.5 -111658.35 4946789.87 463.312716528    81    81 LC_Type1 class
#> 1.6 -111658.35 4946789.87 463.312716528    81    81 LC_Type1 class
#>             scale latitude longitude     site product      start       end
#> 1.1 Not Available 44.65629 -1.174748 arcachon MCD12Q1 2004-01-01 2004-3-20
#> 1.2 Not Available 44.65629 -1.174748 arcachon MCD12Q1 2004-01-01 2004-3-20
#> 1.3 Not Available 44.65629 -1.174748 arcachon MCD12Q1 2004-01-01 2004-3-20
#> 1.4 Not Available 44.65629 -1.174748 arcachon MCD12Q1 2004-01-01 2004-3-20
#> 1.5 Not Available 44.65629 -1.174748 arcachon MCD12Q1 2004-01-01 2004-3-20
#> 1.6 Not Available 44.65629 -1.174748 arcachon MCD12Q1 2004-01-01 2004-3-20
#>     complete modis_date calendar_date   tile     proc_date pixel value
#> 1.1     TRUE   A2004001    2004-01-01 h17v04 2018054103350     1    17
#> 1.2     TRUE   A2004001    2004-01-01 h17v04 2018054103350     2    17
#> 1.3     TRUE   A2004001    2004-01-01 h17v04 2018054103350     3    17
#> 1.4     TRUE   A2004001    2004-01-01 h17v04 2018054103350     4    17
#> 1.5     TRUE   A2004001    2004-01-01 h17v04 2018054103350     5    17
#> 1.6     TRUE   A2004001    2004-01-01 h17v04 2018054103350     6    17
#>     longitude_ll latitude_ll
#> 1.1    -1.407572     44.4875
#> 1.2    -1.407572     44.4875
#> 1.3    -1.407572     44.4875
#> 1.4    -1.407572     44.4875
#> 1.5    -1.407572     44.4875
#> 1.6    -1.407572     44.4875
#> 
#> # convert to bounding box
bb <- apply(arcachon_lc, 1, function(x){
  mt_bbox(xllcorner = x['xllcorner'],
          yllcorner = x['yllcorner'],
          cellsize = x['cellsize'],
          nrows = x['nrows'],
          ncols = x['ncols'])
})

# plot one bounding box
plot(bb[[1]])

# add the location of the queried coordinate within the polygon
points(arcachon_lc$longitude[1],
       arcachon_lc$latitude[1],
       pch = 20,
       col = "red")


# convert to raster, when reproject is TRUE
# the data is reprojected to lat / lon if FALSE
# the data is shown in its original sinuidal projection
LC_r <- mt_to_terra(df = arcachon_lc, reproject = TRUE)
library(terra)
# plot the raster data as a map
plot(LC_r)

