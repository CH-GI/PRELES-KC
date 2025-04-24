
# find 100 nearest Yulin stations to longitude 1W and latitude 53N :
library(climate)
nearest_stations_ogimet(country = "China",
                        date = Sys.Date(),
                        add_map = TRUE,
                        point = c(108, 37),
                        no_of_stations = 100
)
nearest_stations_ogimet(country = "United Kingdom", 
                        point = c(-2, 50),
                        add_map = TRUE, 
                        no_of_stations = 50, 
                        allow_failure = F,
                        main = "Meteo stations in UK")

o = meteo_ogimet(date = c(Sys.Date() - 5, Sys.Date() - 1), 
                 interval = "daily",
                 coords = FALSE, 
                 station = 12330)
