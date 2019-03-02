library(package = "tidyverse") # bulk frequently-used tidyverse packages
library(package = "lubridate") # tidyverse date-time support'


library(package = "PCICt") # Implementation of POSIXct Work-Alike for 365 and 360 Day Calendars


# Mapping  support
library(package = "maps")
library(package = "mapproj")
library(package = "rgdal")
library(package = "rgeos")

# array -> dataframe
library(package = "reshape2")

# netcdf
library(package = "ncdf4")
library(package = "ncdf4.helpers")

# file

file_url = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/ANNUAL/tasmin/LOCA_NGP_tasmin_allensembles_allscenarios_2006-2099_30Y_MEAN_ANNUAL_MIN.nc"


ncngp  = nc_open(filename = file_url)

lon         =  ncvar_get(nc    = ncngp,
                         varid = "lon")

lat         =  ncvar_get(nc    = ncngp,
                         varid = "lat")

ensemble    = ncvar_get(nc    = ncngp,
                        varid = "ensemble")

scenario    = ncvar_get(nc    = ncngp,
                        varid = "scenario")

time        = nc.get.time.series(f                            = ncngp,
                                 v                            = "tasmin_running_avg",
                                 time.dim.name                = "time",
                                 correct.for.gregorian.julian = FALSE,
                                 return.bounds                = TRUE)

time_bnds   = attributes(time)$bounds


time        = as.POSIXct(x  = time, origin="1970-01-01 00:00:00 UTC",
                         tz ="UTC")





time_bnds0 = seq(from = 1951,
                 to   = 2066,
                 by   = 5)

time_bnds1 = time_bnds0 + 30-1

time_bnds  = str_c(time_bnds0,"-",time_bnds1)



tmin        = ncvar_get(nc    = ncngp,
                        varid = "tasmin_running_avg")

dimnames(tmin) = list("lon"       = lon,
                      "lat"       = lat,
                      "scenario"  = scenario,
                      "ensemble"  = ensemble,
                      "time_bnds" = time_bnds1)


tmin = melt(data       = tmin,
            value.name = "Min_Temp",
            id.vars    = c("lon",
                           "lat",
                           "scenario",
                           "ensemble",
                           "time_bnds"),
            na.rm     = FALSE)


before = nrow(tmin)/length(lon)/length(lat)/length(ensemble)/length(scenario)
tmin = tmin %>% filter(!((scenario == "rcp85") & (time_bnds <=2005)))
after = nrow(tmin)/length(lon)/length(lat)/length(ensemble)/length(scenario)

tmin$scenario = as.character(x = tmin$scenario)
tmin$time_bnds = as.numeric(x = tmin$time_bnds)

after/before

tmin$scenario[(tmin$time_bnds <= 2005)] = "Historical"
tmin$scenario[(tmin$time_bnds  > 2005) & (tmin$scenario == "hist/rcp45")] = "RCP 4.5"
tmin$scenario[(tmin$time_bnds  > 2005) & (tmin$scenario == "rcp85")] = "RCP 8.5"

tmin$time_bnds = str_c(tmin$time_bnds - 29, "-", tmin$time_bnds)

tmin$time_bnds = as_factor(tmin$time_bnds)

scenarios = unique(tmin$scenario)


save(time,
     time_bnds,
     ensemble,
     scenario,
     lon,
     lat,
     average_minimum_temperature = tmin,
     file = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/ANNUAL/tasmin/LOCA_NGP_tasmin_allensembles_allscenarios_2006-2099_30Y_MEAN_ANNUAL_MIN.nc")

remove(ncngp)
