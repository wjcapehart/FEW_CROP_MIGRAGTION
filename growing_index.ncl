


begin

setfileoption("nc","Format","NetCDF4")

  period     = 30
  start_year = 1950
  end_year   = 2099
  interval   = 5

  print(2005 - period + 1 - start_year + 1)


  outfile = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/ANNUAL/tasmin/" + \
            "LOCA_NGP_tasmin_allensembles_allscenarios_2006-2099_" + sprinti("%0.2i", period) + \
            "Y_MEAN_ANNUAL_MIN"+ ".nc"


    ensemble = (/ "ACCESS1-0_r1i1p1",  \
                   "ACCESS1-3_r1i1p1",  \
                   "CCSM4_r6i1p1",  \
                   "CESM1-BGC_r1i1p1",  \
                   "CESM1-CAM5_r1i1p1",  \
                   "CMCC-CMS_r1i1p1",  \
                   "CMCC-CM_r1i1p1",  \
                   "CNRM-CM5_r1i1p1",  \
                   "CSIRO-Mk3-6-0_r1i1p1",  \
                   "CanESM2_r1i1p1",  \
                   "FGOALS-g2_r1i1p1",  \
                   "GFDL-CM3_r1i1p1",  \
                   "GFDL-ESM2G_r1i1p1",  \
                   "GFDL-ESM2M_r1i1p1",  \
                   "HadGEM2-AO_r1i1p1",  \
                   "HadGEM2-CC_r1i1p1",  \
                   "HadGEM2-ES_r1i1p1",  \
                   "IPSL-CM5A-LR_r1i1p1",  \
                   "IPSL-CM5A-MR_r1i1p1",  \
                   "MIROC-ESM_r1i1p1",  \
                   "MIROC-ESM-CHEM_r1i1p1",  \
                   "MIROC5_r1i1p1",  \
                   "MPI-ESM-LR_r1i1p1",  \
                   "MPI-ESM-MR_r1i1p1",  \
                   "MRI-CGCM3_r1i1p1",  \
                   "NorESM1-M_r1i1p1",  \
                   "bcc-csm1-1-m_r1i1p1" /)


  n_ens = dimsizes(ensemble)
  ensemble!0 = "ensemble"
  ensemble&ensemble = ensemble
  ensemble@description = "CMIP5 Ensemble Members"
  ensemble@long_name = "CMIP5 Ensemble Members"

  scenario = (/ "hist/rcp45", "rcp85" /)
  scenario!0 = "scenario"
  scenario&scenario = scenario
  scenario@description = "Representative Concentration Pathway"
  scenario@long_name   = "Representative Concentration Pathway"
  n_scen = dimsizes(scenario)



  f_hist = addfile("/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/1950-2005/ANNUAL/tasmin/LOCA_NGP_tasmin_ACCESS1-0_r1i1p1_historical_1950-2005_CDO_YEARLY_MIN.nc","r")
  f_rcp4 = addfile("/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/2006-2099/ANNUAL/tasmin/LOCA_NGP_tasmin_ACCESS1-0_r1i1p1_rcp45_2006-2099_CDO_YEARLY_MIN.nc","r")
  f_rcp8 = addfile("/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/2006-2099/ANNUAL/tasmin/LOCA_NGP_tasmin_ACCESS1-0_r1i1p1_rcp48_2006-2099_CDO_YEARLY_MIN.nc","r")

  lon      = f_hist->lon
  lat      = f_hist->lat
  lon_bnds = f_hist->lon_bnds
  lat_bnds = f_hist->lat_bnds

  time_hist = f_hist->time
  time_futr = f_rcp4->time

  time_units = time_hist@units


  n_lon = dimsizes(lon)
  n_lat = dimsizes(lat)

  year_start = ispan(1951, 2066, interval)
  year_end   = ispan(1980, 2095, interval)



  n_time = dimsizes(year_start)


  time          = new(n_time, "integer")
  time@units    = time_units
  time@calendar = "standard"
  time@bounds    = "time_bnds"
  time@standard_name = "time"
  time@axis          = "T"

  time_bnds          = new((/ n_time, 2 /),"integer")
  time_bnds@units    = time_units
  time_bnds@calendar = "standard"
  time_bnds@long_name = "time bounds"
  time_bnds@description = "time bounds"




  hist_futr_break_time_start = ind( year_end  .eq. 2005)
  hist_futr_break_time_end   = ind(year_start .eq. 2006)
  print("hist_futr_break_time_start = "+hist_futr_break_time_start + " " + year_start(hist_futr_break_time_start) +"-"+year_end(hist_futr_break_time_start))
  print("  hist_futr_break_time_end = "+hist_futr_break_time_end   + " " + year_start(hist_futr_break_time_end) +"-"+year_end(hist_futr_break_time_end))
  print(" ")

  mode = time * 0
  mode@description = "mode for file access"

  do i = 0, n_time-1

    if ( year_end(i)  .le. 2005 ) then
      mode(i) = -1
      print( sprinti("%0.3i", i)+ " - " + sprinti("%2i", mode(i)) + " - " + year_start(i) + "-" + year_end(i) )
    elseif (year_start(i)  .ge. 2006 ) then
      mode(i) = 1
      print(sprinti("%0.3i", i)+ " | " + sprinti("%2i", mode(i)) + " | " + year_start(i) + "-" + year_end(i))
    else
      mode(i) = 0
      print(sprinti("%0.3i", i)+ " * " + sprinti("%2i", mode(i)) + " * " + year_start(i) + "-" + year_end(i))
    end if

    time_bnds(i,0) = toint(cd_inv_calendar(year_start(i),  1, 1, 0, 0, 0, time_units, 0))
    time_bnds(i,1) = toint(cd_inv_calendar(  year_end(i), 12,31,24,59,59, time_units, 0))
    time(i) = ( time_bnds(i,0)  + time_bnds(i,1) )/2

  end do

  time!0    = "time"
  time&time = time
  delete(time@_FillValue)

  time_bnds!0 = "time"
  time_bnds&time = time
  delete(time_bnds@_FillValue)




  t_min = new((/ n_time, n_ens, n_scen, n_lat, n_lon /), "short")
  t_min!0 = "time"
  t_min!1 = "ensemble"
  t_min!2 = "scenario"
  t_min!3 = "lat"
  t_min!4 = "lon"

  t_min&time      = time
  t_min&ensemble  = ensemble
  t_min&scenario  = scenario
  t_min&lat  = lat
  t_min&lon = lon

  t_min@description   = "Average Yearly Minimum Temperature"
  t_min@long_name     = t_min@description
  t_min@standard_name = "air_temperature"
  t_min@cell_methods  = "min: time(by calendar year)   mean:time"
  t_min@units         = "degC"
  t_min@scale_factor  = 0.1
  t_min@add_offset    = 0.0

print(" ")


delete(f_hist)
delete(f_rcp8)
delete(f_rcp4)

print(ensemble)

do ens = 0, n_ens-1

  print(" ")
  print(" "+ens+":"+ (n_ens-1)  +" "+ ensemble(ens))
  print(" ")


  file_hist = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/1950-2005/ANNUAL/tasmin/LOCA_NGP_tasmin_" + ensemble(ens) + "_historical_1950-2005_CDO_MONTLY_MIN.nc"
  file_rcp4 = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/2006-2099/ANNUAL/tasmin/LOCA_NGP_tasmin_" + ensemble(ens) +      "_rcp45_2006-2099_CDO_MONTLY_MIN.nc"
  file_rcp8 = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/2006-2099/ANNUAL/tasmin/LOCA_NGP_tasmin_" + ensemble(ens) +      "_rcp85_2006-2099_CDO_MONTLY_MIN.nc"

  f_hist = addfile(file_hist,"r")
  f_rcp4 = addfile(file_rcp4,"r")
  f_rcp8 = addfile(file_rcp8,"r")

  v_hist = "tasmin_" + ensemble(ens) + "_historical"
  v_rcp4 = "tasmin_" + ensemble(ens) + "_rcp45"
  v_rcp8 = "tasmin_" + ensemble(ens) + "_rcp85"

  packopt = True
    packopt@scale_factor = 0.1
    packopt@add_offset   = 0.0

  do i = 0,  n_time-1


      if (mode(i) .eq. -1) then

        capture_indexes = ind( (time_hist .ge. time_bnds(i,0))  .and.   (time_hist .le. time_bnds(i,1)) )

        print(ensemble(ens) + " :: " +   sprinti("%0.3i", i)+ " - " + sprinti("%2i", mode(i)) + " - (" + year_start(i) +"-"+ year_end(i)+  ") " + min(capture_indexes) + "-" + max(capture_indexes) )


        inarr = short2flt(f_hist->$v_hist$(capture_indexes,:,:))

        inarr2 = pack_values(dim_avg_n_Wrap(inarr,0), \
                              "short", \
                              packopt)




        t_min(i, ens, 0, :, :) = (/ inarr2 /)

        delete(inarr)
        delete(inarr2)

      elseif (mode(i) .eq. 1)   then


        capture_indexes = ind( (time_futr .ge. time_bnds(i,0))  .and.   (time_futr .le. time_bnds(i,1)) )

        print(ensemble(ens) + " :: " +  sprinti("%0.3i", i)+ " | " + sprinti("%2i", mode(i)) + " - (" + year_start(i) +"-"+ year_end(i)+  ") " + min(capture_indexes) + "-" + max(capture_indexes) )


        inarr = short2flt(f_rcp4->$v_rcp4$(capture_indexes,:,:))

        inarr2 = pack_values(dim_avg_n_Wrap(inarr,0), \
                              "short", \
                              packopt)




        t_min(i, ens, 0, :, :) = (/  inarr2 /)

        delete(inarr)
        delete(inarr2)

        inarr = short2flt(f_rcp8->$v_rcp8$(capture_indexes,:,:))

        inarr2 = pack_values(dim_avg_n_Wrap(inarr,0), \
                              "short", \
                              packopt)



        t_min(i, ens, 1, :, :) = (/ inarr2 /)

        delete(inarr)
        delete(inarr2)


      else


        capture_indexes_hist = ind( (time_hist .ge. time_bnds(i,0))  )
        capture_indexes_futr = ind( (time_futr .le. time_bnds(i,1)) )

        print(ensemble(ens) + " :: " +   sprinti("%0.3i", i)+ " * " + sprinti("%2i", mode(i)) + " - (" + year_start(i) +"-"+ year_end(i)+  ") " + min(capture_indexes_hist) + "-" + max(capture_indexes_hist) +"  ::  " + min(capture_indexes_futr) + "-" + max(capture_indexes_futr) )


        inarr_hist = short2flt(f_hist->$v_hist$(capture_indexes_hist,:,:))
        inarr_rcp4 = short2flt(f_rcp4->$v_rcp4$(capture_indexes_futr,:,:))
        inarr_rcp8 = short2flt(f_rcp8->$v_rcp8$(capture_indexes_futr,:,:))

        inarr = array_append_record (inarr_hist, inarr_rcp4, 0)

        inarr2 = pack_values(dim_avg_n_Wrap(inarr,0), \
                              "short", \
                              packopt)



        t_min(i, ens, 0, :, :) = (/ inarr2 /)

        intarr = array_append_record (inarr_hist, inarr_rcp8, 0)

        inarr2 = pack_values(dim_avg_n_Wrap(inarr,0), \
                              "short", \
                              packopt)

        t_min(i, ens, 1, :, :) = (/ inarr2 /)





        delete(inarr)
        delete(inarr2)
        delete(inarr_hist)
        delete(inarr_rcp4)
        delete(inarr_rcp8)

        delete(capture_indexes_hist)
        delete(capture_indexes_futr)

      end if

  end do

  delete(f_hist)
  delete(f_rcp8)
  delete(f_rcp4)

end do

system("rm -frv " + outfile)
f_new = addfile(outfile,"c")
f_new->tasmin_running_avg = t_min
f_new->lon_bnds = lon_bnds
f_new->lat_bnds = lat_bnds
f_new->time_bnds = time_bnds


delete(f_new)




end
