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

time_step = 1

file_url = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/ANNUAL/tasmin/LOCA_NGP_tasmin_allensembles_allscenarios_2006-2099_30Y_MEAN_ANNUAL_MIN_01Y_PRODUCT_INTERVAL.nc"


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
                 by   = time_step)

time_bnds1 = time_bnds0 + 30-1

time_bnds  = str_c(time_bnds0,"-",time_bnds1)


# color LOCA_NGP_tasmin_allensembles_allscenarios_2006


classes_table = c("1a",
                  "1b",
                  "2a",
                  "2b",
                  "3a",
                  "3b",
                  "4a",
                  "4b",
                  "5a",
                  "5b",
                  "6a",
                  "6b",
                  "7a",
                  "7b",
                  "8a",
                  "8b",
                  "9a",
                  "9b",
                  "10a",
                  "10b",
                  "11a",
                  "11b",
                  "12a",
                  "12b",
                  "13a",
                  "13b" )



class2L_cols  = c("#D1D0FE", #  "1a",
                  "#BBBCF0", #  "1b",
                  "#A1A1D4", #  "2a",
                  "#E8A5E8", #  "2b",
                  "#DB86E9", #  "3a",
                  "#C772D6", #  "3b",
                  "#9C61FF", #  "4a",
                  "#506AEB", #  "4b",
                  "#6797FF", #  "5a",
                  "#54C2DD", #  "5b",
                  "#3FB13F", #  "6a",
                  "#6DBE4C", #  "6b",
                  "#A2D05F", #  "7a",
                  "#C8D666", #  "7b",
                  "#E9D479", #  "8a",
                  "#E7C44D", #  "8b",
                  "#D6AD45", #  "9a",
                  "#F5AD6D", #  "9b",
                  "#E89130", #  "10a",
                  "#E26D1C", #  "10b",
                  "#E44B1D", #  "11a",
                  "#E57959", #  "11b",
                  "#CE4F46", #  "12a",
                  "#AD4624", #  "12b",
                  "#A16529", #  "13a",
                  "#BA8554") #  "13b"



class2L_hi = seq(from = -55,
                 to   =  70,
                 by   =   5)

class2L_lo = class2L_hi - 5.


class2L_cols = as.array(class2L_cols)
names(class2L_cols) = as_factor(classes_table)


##
# maps
load(file = "./canadian_provinces.Rdata")
# inported field is canada_data = canadian_provinces


usa_data   = map_data(map="state",
                      xlim=c(-114.2812, -86.21875),
                      ylim=c (33.96875,  52.78125) )
water_data = map_data(map="lakes",
                      xlim=c(-114.2812,-86.21875),
                      ylim=c( 33.96875, 52.78125) )


for (t in 1:length(time_bnds0))
{
      decade_string = time_bnds[t]
      print(decade_string)

     # new((/ n_time, n_ens, n_scen, n_lat, n_lon /), "short")
      tmin        = ncvar_get(nc    = ncngp,
                              varid = "tasmin_running_avg",
                              start = c( 1,  1,  1,  1, t),
                              count = c(-1, -1, -1, -1, 1))

      dimnames(tmin) = list("lon"       = lon,
                            "lat"       = lat,
                            "scenario"  = scenario,
                            "ensemble"  = ensemble)

      tmin = melt(data       = tmin,
                  value.name = "Min_Temp",
                  id.vars    = c("lon",
                                 "lat",
                                 "scenario",
                                 "ensemble",
                                 "time_bnds"),
                  na.rm     = FALSE)

                  tmin$time_bnds = time_bnds1[t]

                  tmin$Min_Temp = (tmin$Min_Temp * 9./5.) + 32.

      before = nrow(tmin)/length(lon)/length(lat)/length(ensemble)/length(scenario)
      tmin = tmin %>% filter(!((scenario == "rcp85") & (time_bnds <=2005)))
      after = nrow(tmin)/length(lon)/length(lat)/length(ensemble)/length(scenario)

      tmin$scenario = as.character(x = tmin$scenario)
      tmin$time_bnds = as.numeric(x = tmin$time_bnds)

      after/before

      tmin$scenario[(tmin$time_bnds <= 2005)] = "Historical"
      tmin$scenario[(tmin$time_bnds  > 2005) & (tmin$scenario == "hist/rcp45")] = "RCP 4.5"
      tmin$scenario[(tmin$time_bnds  > 2005) & (tmin$scenario == "rcp85")] = "RCP 8.5"


      tmin$time_bnds = as_factor(str_c(tmin$time_bnds-29,"-",tmin$time_bnds))




      tmin_ens_mean = tmin %>% group_by(lat,
                                            lon,
                                            scenario) %>%
                                   summarize(Min_Temp = mean(Min_Temp))

      tmin_ens_mean$Zone = "NA"


      for (cat in 1:length(classes_table)) {

        tmin_ens_mean$Zone[(tmin_ens_mean$Min_Temp >  class2L_lo[cat]) &
                           (tmin_ens_mean$Min_Temp <= class2L_hi[cat])]  = classes_table[cat]

      }
      tmin_ens_mean$Zone[tmin_ens_mean$Zone == "NA"] = NA

      tmin_ens_mean$Zone = as_factor(tmin_ens_mean$Zone)


      mymap = ggplot(data   =   tmin_ens_mean)  +

             aes(x     = lon,
                 y     = lat) +

             facet_grid(cols = vars(scenario)) +

             theme_bw() +

             theme(strip.background = element_rect(fill=NA),
                   aspect.ratio     = 1)+

             labs(title    = "CMIP5 LOCA Climate Ensemble Analyses",
                  subtitle = str_c(decade_string,
                                   "USDA Plant Heartiness Zones (Ensemble Mean)",
                                   sep = " "),
                  caption = "South Dakota School of Mines & Technology\nAtmospheric & Environmental Sciences") +

             xlab(label = "Longitude") +

             ylab(label = "Latitude") +

             guides(fill=guide_legend(ncol=2)) +



             coord_cartesian(xlim=c(min(longitude), max(longitude)),
                             ylim=c(min(latitude),  max(latitude)) )  +

            # xlim(min(Longitude), max(Longitude)) +
            # ylim(min(Latitude), max(Latitude)) +

             scale_fill_manual(values = class2L_cols,
                               breaks = classes_table,
                               drop   = TRUE) +

             geom_raster(data = tmin_ens_mean,
                         mapping = aes(x     = lon,
                                       y     = lat,
                                       fill = Zone)) +

             geom_polygon(data  = usa_data,
                                  mapping = aes(x     =   long,
                                                y     =    lat,
                                                group =  group),
                                  fill  = NA,
                                  color = "black")  +

             geom_polygon(data  = water_data,
                                  mapping = aes(x     =   long,
                                                y     =    lat,
                                                group =  group),
                                  fill  = NA,
                                  color = "black")  +

             geom_polygon(data  = canadian_provinces,
                                  mapping = aes(x     =   long,
                                                y     =    lat,
                                                group =  group),
                                  fill  = NA,
                                  color = "black")





      ggsave(filename = str_c("./",
                              decade_string,
                              "_LOCA_GROWING_ZONES_MAP.png",
                              sep = ""),
             plot = mymap,
             device = "png",
           width = 10.5, height = 5.75)





}
