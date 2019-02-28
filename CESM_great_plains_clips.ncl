



begin

   max_lat =  52.78125 ; degrees north
   min_lat =  33.96875 ; degrees north

   min_lon = -114.28125 + 360 ; degrees east
   max_lon =  -86.21875 + 360 ; degrees east

   RCP45_URL_RAIN = "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/CESM_CONUS/RAIN_RCP45_CONUS_1920-2080.nc4"
   RCP85_URL_RAIN = "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/CESM_CONUS/RAIN_RCP85_CONUS_1920-2100.nc4"

   RCP45_URL_SNOW = "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/CESM_CONUS/SNOW_RCP45_CONUS_1920-2080.nc4"
   RCP85_URL_SNOW = "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/CESM_CONUS/SNOW_RCP85_CONUS_1920-2100.nc4"

   RCP45_URL_TAVG = "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/CESM_CONUS/TREFHT_RCP45_CONUS_1920-2080.nc4"
   RCP85_URL_TAVG = "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/CESM_CONUS/TREFHT_RCP85_CONUS_1920-2100.nc4"



   f = addfile(RCP45_URL_RAIN, "r")

      lon                  = f->lon({min_lon:max_lon})
         lon&lon           = lon
         lon@description   = lon@long_name
         lon@standard_name = "longitude"
         lon@axis          = "X"

      lat        = f->lat({min_lat:max_lat})
         lat&lat = lat
         lat@description   = lat@long_name
         lat@standard_name = "latitude"
         lat@axis          = "Y"

      time         = f->time(0:1931)
         time&time = time
         time@description   = time@long_name
         time@standard_name = "time"
         time@axis          = "T"

      time_bounds         = f->time_bounds(0:1931,:)

      ensemble             = ispan(1,15,1)
         ensemble@long_name   = "ensemble member"
         ensemble@description = "ensemble member"
         ensemble!0           = "ensemble"
         ensemble&ensemble    =  ensemble

      rain_45    = f->RAIN(  0:1931, 0:14, {min_lat:max_lat}, {min_lon:max_lon})
         rain_45&time     = time
         rain_45&ensemble = ensemble
         rain_45&lon      = lon
         rain_45&lat      = lat




   f = addfile(RCP85_URL_RAIN, "r")
      rain_85 = f->RAIN( 0:1931, 0:14, {min_lat:max_lat}, {min_lon:max_lon})
         rain_85&time     = time
         rain_85&ensemble = ensemble
         rain_85&lon      = lon
         rain_85&lat      = lat

   f = addfile(RCP45_URL_SNOW, "r")
      snow_45 = f->SNOW( 0:1931, 0:14, {min_lat:max_lat}, {min_lon:max_lon})
         snow_45&time     = time
         snow_45&ensemble = ensemble
         snow_45&lon      = lon
         snow_45&lat      = lat

   f = addfile(RCP85_URL_SNOW, "r")
      snow_85 = f->SNOW( 0:1931, 0:14, {min_lat:max_lat}, {min_lon:max_lon})
         snow_85&time     = time
         snow_85&ensemble = ensemble
         snow_85&lon      = lon
         snow_85&lat      = lat

   f = addfile(RCP45_URL_TAVG, "r")
      tavg_45 = f->TREFHT( 0:1931, 0:14, {min_lat:max_lat}, {min_lon:max_lon})
         tavg_45&time     = time
         tavg_45&ensemble = ensemble
         tavg_45&lon      = lon
         tavg_45&lat      = lat
         tavg_45@description    = "monthly mean temperature"
         tavg_45@long_name      = "monthly mean temperature"
         tavg_45@units          = "K"
         tavg_45@standard_name  =  "air_temperature"
         tavg_45@rcp            = 4.5

   f = addfile(RCP85_URL_TAVG, "r")
      tavg_85 = f->TREFHT( 0:1931, 0:14, {min_lat:max_lat}, {min_lon:max_lon})
         tavg_85&time     = time
         tavg_85&ensemble = ensemble
         tavg_85&lon      = lon
         tavg_85&lat      = lat
         tavg_85@description    = "monthly mean temperature"
         tavg_85@long_name      = "monthly mean temperature"
         tavg_85@units          = "K"
         tavg_85@standard_name  =  "air_temperature"
         tavg_85@rcp            = 8.5



   prec_85 = rain_85
      prec_85 = (/ rain_85 + snow_85 /) * 86400.
      prec_85@description    = "monthly precipitaation"
      prec_85@long_name      = "monthly precipitaation"
      prec_85@units          = "kg m-2"
      prec_85@standard_name  =  "precipitation_amount"
      prec_85@rcp            = 8.5

   prec_45 = rain_45
      prec_45 = (/ rain_45 + snow_45 /) * 86400.
      prec_45@description    = "monthly precipitaation"
      prec_45@long_name      = "monthly precipitaation"
      prec_45@units          = "kg m-2"
      prec_45@standard_name  =  "precipitation_amount"
      prec_45@rcp            = 4.5
      prec_45@cell_methods   = "time: sum"

      print(lon)

   lon(:) = lon - 360.
   lon&lon = lon

   print(lon)
   tavg_85&lon      = lon
   tavg_45&lon      = lon
   prec_85&lon      = lon
   prec_45&lon      = lon


   nyr = dimsizes(time)/12




   system("rm ./CESM_FEW_TAVG_PREC_MONTHLY.nc ./CESM_FEW_TAVG_PREC_ANNUAL.nc")
   f = addfile("./CESM_FEW_TAVG_PREC_MONTHLY.nc","c")
   f->time = time
   f->time_bounds = time_bounds
   f->lon  = lon
   f->lat  = lat
   f->ensemble = ensemble
   f->prec_45 = prec_45
   f->prec_85 = prec_85
   f->tavg_45 = tavg_45
   f->tavg_85 = tavg_85
   delete(f)
   old_time_bounds = time_bounds
   delete(prec_45)
   delete(prec_85)
   delete(tavg_45)
   delete(tavg_85)
   delete(time_bounds)
   delete(time)
   delete(ensemble)



  system("ncks -O -h --mk_rec_dmn time ./CESM_FEW_TAVG_PREC_MONTHLY.nc delete.me.nc")
  system("mv -v delete.me.nc ./CESM_FEW_TAVG_PREC_MONTHLY.nc")

print("----- Cleaning CESM_FEW_TAVG_PREC_MONTHLY -----")
     system("ncatted -O -h -a _ChunkSizes,time,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a _ChunkSizes,lon,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a _ChunkSizes,lat,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a _ChunkSizes,time,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a _ChunkSizes,tavg_45,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a _ChunkSizes,tavg_85,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a _ChunkSizes,prec_45,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a _ChunkSizes,prec_85,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a _ChunkSizes,time_bounds,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a _FillValue,lon,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a _FillValue,lat,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a _FillValue,time,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a missing_value,lon,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a missing_value,lat,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")
     system("ncatted -O -h -a missing_value,time,d,, ./CESM_FEW_TAVG_PREC_MONTHLY.nc")


     print("----- Done Cleaning CESM_FEW_TAVG_PREC_MONTHLY -----")

   new_time_bounds = new((/ nyr,2 /), "double")

   do i = 0, nyr-1
      end_t = (i+1)*12-1
      start_t = end_t -11
       new_time_bounds(i,0) = round(old_time_bounds(start_t,0),0)
       new_time_bounds(i,1) = round(old_time_bounds(  end_t,1),0)
       system("ncra -h  -d time,"+sprinti("%0.4d", start_t) + ","  + \
                             sprinti("%0.4d",   end_t) + " "  + \
                             "./CESM_FEW_TAVG_PREC_MONTHLY.nc " + \
                             "temp_"+sprinti("%0.4d",(i+1920))+".nc")
   end do

   print(new_time_bounds)

   system("ncrcat -h ./temp_*.nc ./CESM_FEW_TAVG_PREC_ANNUAL.nc")
   system("rm ./temp_????.nc")
   system("ncatted -O -h -a cell_methods,time_bounds,d,, ./CESM_FEW_TAVG_PREC_ANNUAL.nc")

   f = addfile("./CESM_FEW_TAVG_PREC_ANNUAL.nc","w")
      tavg_45 = f->tavg_45
      tavg_85 = f->tavg_85
      prec_45 = f->prec_45
      prec_85 = f->prec_85
      time_bounds = f->time_bounds

      time_bounds(:,:) = (/ new_time_bounds  /)

      prec_45(:,:,:,:) = (/ prec_45(:,:,:,:) * 365. /)
      prec_85(:,:,:,:) = (/ prec_85(:,:,:,:) * 365. /)

      prec_45@description = "annual total precipitation (rcp4.5)"
      prec_85@description = "annual total precipitation (rcp8.5)"
      tavg_45@description = "annual mean temperature (rcp4.5)"
      tavg_85@description = "annual mean temperature (rcp8.5)"

      prec_45@long_name = "annual total precipitation (rcp4.5)"
      prec_85@long_name = "annual total precipitation (rcp8.5)"
      tavg_45@long_name = "annual mean temperature (rcp4.5)"
      tavg_85@long_name = "annual mean temperature (rcp8.5)"

      prec_45@cell_methods = "time: sum"
      prec_85@cell_methods = "time: sum"

      f->time_bounds = time_bounds
      f->prec_45 = prec_45
      f->prec_85 = prec_85
      f->tavg_45 = tavg_45
      f->tavg_85 = tavg_85

   delete(f)


end
