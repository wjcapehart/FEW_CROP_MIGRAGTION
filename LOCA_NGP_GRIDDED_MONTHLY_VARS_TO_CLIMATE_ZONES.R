
  library(package = "tidyverse")
  library(package = "tidypredict")
  library(package = "plyr")

  library(package = "ncdf4")
  library(package = "ncdf4.helpers")


  library(package = "lubridate") # processing dates and time
  library(package = "stringr")


  library(package = "reshape2")  # manipulating data frames
  library(package = "abind")


  
  



  base_start      = 1950 # start year
  base_end        = 2005 # end year  (just before our simulations diverge)
  
  base_string     = str_c(base_start,
                          base_end,
                          sep="-")
  
  
  
  per1_start      = 2006 # start
  per1_end        = 2099 # end year
  
  per1_string     = str_c(per1_start,
                          per1_end,
                          sep="-")
  
  
  Scenario  = c( str_c("Historical (",
                        base_string,
                        ")",
                        sep = ""),
                
                 str_c("RCP 4.5 (",
                        per1_string,
                        ")",
                        sep = ""),
                
                 str_c("RCP 8.5 (",
                        per1_string,
                       ")",
                        sep = "")
                
                )

  
  Scenario = as.factor(Scenario)
  
  remove(per1_end, per1_start,
         base_end, base_start)

ensembles = c( "ACCESS1-0_r1i1p1",
               "ACCESS1-3_r1i1p1",
               "CCSM4_r6i1p1",
               "CESM1-BGC_r1i1p1",
               "CESM1-CAM5_r1i1p1",
               "CMCC-CMS_r1i1p1",
               "CMCC-CM_r1i1p1",
               "CNRM-CM5_r1i1p1",
               "CSIRO-Mk3-6-0_r1i1p1",
               "CanESM2_r1i1p1",
               "FGOALS-g2_r1i1p1",
               "GFDL-CM3_r1i1p1",
               "GFDL-ESM2G_r1i1p1",
               "GFDL-ESM2M_r1i1p1",
               "HadGEM2-AO_r1i1p1",
               "HadGEM2-CC_r1i1p1",
               "HadGEM2-ES_r1i1p1",
               "IPSL-CM5A-LR_r1i1p1",
               "IPSL-CM5A-MR_r1i1p1",
               "MIROC-ESM_r1i1p1",
               "MIROC5_r1i1p1",
               "MPI-ESM-LR_r1i1p1",
               "MPI-ESM-MR_r1i1p1",
               "MRI-CGCM3_r1i1p1",
               "NorESM1-M_r1i1p1",
               "bcc-csm1-1-m_r1i1p1" )

ensembles = as.factor(ensembles)


variable = as.factor(c("pr",
                       "tasmax",
                       "tasmin"))


# Historical

periods = c(base_string,
            per1_string,
            per1_string)


scen_fn = c("historical",
            "rcp45",
            "rcp85")

stat_fn = c("CDO_MONTLY_TOTAL",
            "CDO_MONTLY_MEAN",
            "CDO_MONTLY_MEAN")

  # URL_Root_Directory = "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/LOCA_NGP/climatology/"
  URL_Root_Directory = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/"
  



  # get thematic mask netcdf file.


ncngp  = nc_open(filename = "//Users/wjc/GitHub/FEW_CROP_MiGRAGTION/arch/NGP_Climate_Zones.nc") 

  lon         =  ncvar_get(nc    = ncngp, 
                           varid = "lon")
  lat         =  ncvar_get(nc    = ncngp,
                           varid = "lat")
  
  climate_regions  =  array(data     = NA,
                            dim      = c(length(lon),
                                         length(lat)),
                            dimnames = list("lon" = lon,
                                            "lat" = lat))
  
  climate_regions[,]    = ncvar_get(nc    = ncngp,
                                 varid = "US_CAN_Zones")
  
  climate_regions = t(apply(X      = climate_regions,
                          MARGIN = 1, 
                          FUN    = rev))
nc_close(nc = ncngp)

remove(ncngp)
 
Climate_Zones = unique(as.factor(climate_regions))
 
n_zones       = length(unique(Climate_Zones))

climate_regions_table = melt(data       = climate_regions,
                             value.name = "Climate_Zone")

climate_regions_table$Climate_Zone = as.factor(climate_regions_table$Climate_Zone)





ens  = ensembles[1]
var  = variable[1]
scen = Scenario[1]

for (ens in ensembles) {

  for (scen in Scenario) {
  
      
      for (var in variable) {
        
        variable_stat_names = c(str_c(var,".mean",   sep=""),
                                str_c(var,".median", sep=""),
                                str_c(var,".stdev",  sep=""))
        
        
        URL_File = str_c(URL_Root_Directory,
                         periods[which(Scenario == scen)],
                         "/MONTHLY/",
                         var,
                         "/LOCA_NGP_",
                         var,
                         "_",
                         ens,
                         "_",
                         scen_fn[which(Scenario == scen)],
                         "_",
                         periods[which(Scenario == scen)],
                         "_",
                         stat_fn[which(variable == var)],
                         ".nc",
                         sep = "")
        
        variable_name = str_c(var,
                              "_",
                              ens,
                              "_",
                              scen_fn[which(Scenario == scen)],
                              sep = "")
        
        print(str_c("  - ",
                    variable_name,
                    sep = " "))
        
        nc.file = nc_open(filename = URL_File)
        
            if ((var  == variable[1])) {
              
              time = nc.get.time.series(f             =  nc.file, # netCDF file handle
                                        time.dim.name =   "time") # netCDF time coordinate name
          
              time = as.POSIXct(time)
    
              year = year(time)
        
              lat = ncvar_get(nc    = nc.file,
                              varid =   "lat")
              lon = ncvar_get(nc    = nc.file,
                              varid =   "lon")
              
              zones_3d = array(data = 0,
                               dim  = c(length(lon),
                                        length(lat),
                                        length(time)),
                               dimnames = list("lon"  = lon,
                                               "lat"  = lat,
                                               "Time" = time))
              

            
              
            }  # close first time around
    
            in_vars           = ncvar_get(nc    = nc.file, 
                                          varid = variable_name)
            
            dimnames(in_vars) = list("lon"  = lon,
                                     "lat"  = lat,
                                     "Time" = time)
  
            statistics_array = array(data = NA,
                                       dim  = c(length(time),
                                                length(Climate_Zones),
                                                3),
                                       dimnames = list("Time"          = time,
                                                       "Climate_Zones" = Climate_Zones,
                                                       "Statistic"     = variable_stat_names)
                                       )    

                      
        nc_close(nc = nc.file)
        
        for (z in Climate_Zones) { 
          
          mask_2d                       = climate_regions * NA
          mask_2d[climate_regions == z] = 1.0
          
          print(str_c("  -- ", variable_name," (",z,") [",(which(Climate_Zones == z)/n_zones*100.0),"%]", sep = ""))

          
          for (t in seq(from = 1,
                            to   = length(time),
                            by   = 1)) {
          

            
              subset = in_vars[,,t] * mask_2d
              
              statistics_array[t,which(Climate_Zones == z),1] = mean(  subset, na.rm = TRUE)
              statistics_array[t,which(Climate_Zones == z),2] = median(subset, na.rm = TRUE)
              statistics_array[t,which(Climate_Zones == z),3] = sd(    subset, na.rm = TRUE)

              
          } # close (time)
          
        } #  close for (z in Climate_Zones)
        
        temp_frame = adply(statistics_array,
                           1:2)
        
        temp_frame$Scenario = scen
        temp_frame$Ensemble = ens

        if (!exists("loca_climate_zone_stats")) {
          
              loca_climate_zone_stats = temp_frame
              
              remove(temp_frame)
          
        } else {
          
          loca_climate_zone_stats = left_join(x  = loca_climate_zone_stats,
                                              y  = temp_frame,
                                              by = c("Time",
                                                     "Climate_Zones",
                                                     "Scenario",
                                                     "Ensemble"))
          remove(temp_frame)

        }
        
        statistics_array[,,] = 0
        in_vars[,,]          = 0

        
        
     
    
      }  # close for (var in variable)
      
    
    
    loca_climate_zone_stats$Time = as.numeric(levels(loca_climate_zone_stats$Time))[loca_climate_zone_stats$Time]
    loca_climate_zone_stats$Time = as.POSIXct(loca_climate_zone_stats$Time,origin="1970-01-01 00:00:00 UTC", tz = "UTC")
    outputfile =  URL_File = str_c(URL_Root_Directory,
                                   "/DERIVED/MONTHLY/LOCA_NGP_CLIMATEZONES_",
                                   "allvar",
                                   "_",
                                   ens,
                                   "_",
                                   scen_fn[which(Scenario == scen)],
                                   ".Rdata",
                                   sep = "")
    
    save(loca_climate_zone_stats,
         file=outputfile)
    
    remove(loca_climate_zone_stats)
            
    
    } # close for (ens in scenarios)


} # close for ensembles


