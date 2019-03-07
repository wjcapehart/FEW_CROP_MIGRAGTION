


begin

setfileoption("nc","Format","NetCDF4")

  period     = 30
  start_year = 1950
  end_year   = 2099
  interval   = 1



    variable               = "tasavg"
    variable@description   = "Average Monthly Mean Temperature"
    variable@units         = "degC"
    variable@standard_name = "air_temperature"
    variable@cell_methods  = "ensemble: mean   time: mean within month   time: mean by calendar months"
    first_ag               = "CDO_MONTHLY_AVERAGES"


      variable               = "pr"
      variable@description   = "Average Monthly Total Rainfall"
      variable@units         = "kg m-2"
      variable@standard_name = "precipitation_amount"
      variable@cell_methods  = "ensemble: mean   time: sum within month   time: mean by calendar months"
      first_ag               = "CDO_MONTLY_TOTAL"



  n_years = (end_year-period-1 )- start_year+1

  year_start := ispan(start_year , end_year-period+1,interval)
  year_end   := year_start+ period-1

  n_years = dimsizes(year_start)


  year_start!0    = "time"
  year_start&time = time
  year_start@description = "start year for climate period"
  year_start@long_name   = "start year for climate period"
  year_start@units       = "years since 0000-01-01 00:00:00"

  year_end!0    = "time"
  year_end&time = time
  year_end@description = "start year for climate period"
  year_end@long_name   = "start year for climate period"
  year_end@units       = "years since 0000-01-01 00:00:00"



  outfile = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/MONTHLY/"+variable+"/" + \
            "LOCA_NGP_"+variable+"_allensembles_allscenarios_2006-2099_" + sprinti("%0.2i", period) + \
            "Y_MEAN_MONTHLY_MEAN_"+sprinti("%0.2i", interval)+"Y_PRODUCT_INTERVAL"+ ".nc"




                   ensemble := (/ "allensembles" /)



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



  f_hist = addfile("/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/1950-2005/MONTHLY/"+variable+"/LOCA_NGP_"+variable+"_allensembles_historical_1950-2005_"+first_ag+"_MEAN_ACROSS_ALL_MEMBERS.nc","r")
  f_rcp4 = addfile("/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/2006-2099/MONTHLY/"+variable+"/LOCA_NGP_"+variable+     "_allensembles_rcp45_2006-2099_"+first_ag+"_MEAN_ACROSS_ALL_MEMBERS.nc","r")
  f_rcp8 = addfile("/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/2006-2099/MONTHLY/"+variable+"/LOCA_NGP_"+variable+     "_allensembles_rcp85_2006-2099_"+first_ag+"_MEAN_ACROSS_ALL_MEMBERS.nc","r")

  lon      = f_hist->lon
  lat      = f_hist->lat
  lon_bnds = f_hist->lon_bnds
  lat_bnds = f_hist->lat_bnds

  time_hist = f_hist->time
  time_futr = f_rcp4->time

  datevals_hist = cd_calendar(time_hist, -5)
  datevals_futr = cd_calendar(time_futr, -5)

  time_units = time_hist@units


  n_lon = dimsizes(lon)
  n_lat = dimsizes(lat)

  time               = new(n_years, "integer")
  time@units         = time_units
  time@calendar      = "standard"
  time@climatology   = "climatology_bounds"
  time@standard_name = "time"
  time@axis          = "T"


  climatology_bounds             = new((/ n_years, 2 /),"integer")
  climatology_bounds!0           = "time"
  climatology_bounds&time
  climatology_bounds!1           = "bnds"

  climatology_bounds@units       = time_units
  climatology_bounds@calendar    = "standard"
  climatology_bounds@long_name   = "climatology bounds"
  climatology_bounds@description = "climatology bounds"

  hist_futr_break_time_start = ind( year_end  .eq. 2005)
  hist_futr_break_time_end   = ind(year_start .eq. 2006)
  print("hist_futr_break_time_start = "+hist_futr_break_time_start + " " + year_start(hist_futr_break_time_start) +"-"+year_end(hist_futr_break_time_start))
  print("  hist_futr_break_time_end = "+hist_futr_break_time_end   + " " + year_start(hist_futr_break_time_end) +"-"+year_end(hist_futr_break_time_end))
  print(" ")

  mode = time * 0
  mode@description = "mode for file access"

  do t = 0, dimsizes(time)-1


    if ( year_end(t)  .le. 2005 ) then
      mode(t) = -1
      print( sprinti("%0.3i", t)+ " - " + sprinti("%2i", mode(t)) + " - " + year_start(t) + "-" + year_end(t) )
    elseif (year_start(t)  .ge. 2006 ) then
      mode(t) = 1
      print(sprinti("%0.3i", t)+ " | " + sprinti("%2i", mode(t)) + " | " + year_start(t) + "-" + year_end(t))
    else
      mode(t) = 0
      print(sprinti("%0.3i", t)+ " * " + sprinti("%2i", mode(t)) + " * " + year_start(t) + "-" + year_end(t))
    end if

    climatology_bounds(t,0) = (/ toint(cd_inv_calendar(year_start(t),  1, 1, 0, 0, 0, time_units, 0)) /)
    climatology_bounds(t,1) = (/ toint(cd_inv_calendar(  year_end(t), 12,31,24,59,59, time_units, 0)) /)
    time(t)                 = toint( (climatology_bounds(t,1) + climatology_bounds(t,0)) / 2.0 )

  end do

  month = ispan(1,12,1)
  month@description = "calendar_month"
  month@long_name   = "calendar_month"
  n_months = dimsizes(month)
  month!0 = "month"
  month&month = month

  time!0    = "time"
  time&time = time
  delete(time@_FillValue)

  climatology_bounds!0 = "time"
  climatology_bounds&time = time
  delete(climatology_bounds@_FillValue)




  t_min = new((/ n_years, n_months, n_ens, n_scen, n_lat, n_lon /), "short")
  t_min!0 = "time"
  t_min!1 = "month"
  t_min!2 = "ensemble"
  t_min!3 = "scenario"
  t_min!4 = "lat"
  t_min!5 = "lon"

  t_min&time      = time
  t_min&month     = month
  t_min&ensemble  = ensemble
  t_min&scenario  = scenario
  t_min&lat       = lat
  t_min&lon       = lon

  t_min@description   = variable@description
  t_min@long_name     = variable@description
  t_min@units         = variable@units
  t_min@standard_name = variable@standard_name
  t_min@cell_methods  = variable@cell_methods
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


  file_hist = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/1950-2005/MONTHLY/"+variable+"/LOCA_NGP_"+variable+"_" + ensemble(ens) + "_historical_1950-2005_"+first_ag+"_MEAN_ACROSS_ALL_MEMBERS.nc"
  file_rcp4 = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/2006-2099/MONTHLY/"+variable+"/LOCA_NGP_"+variable+"_" + ensemble(ens) +      "_rcp45_2006-2099_"+first_ag+"_MEAN_ACROSS_ALL_MEMBERS.nc"
  file_rcp8 = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/2006-2099/MONTHLY/"+variable+"/LOCA_NGP_"+variable+"_" + ensemble(ens) +      "_rcp85_2006-2099_"+first_ag+"_MEAN_ACROSS_ALL_MEMBERS.nc"

  f_hist = addfile(file_hist,"r")
  f_rcp4 = addfile(file_rcp4,"r")
  f_rcp8 = addfile(file_rcp8,"r")

  v_hist = variable+"_ACCESS1-0_r1i1p1_historical"
  v_rcp4 = variable+"_ACCESS1-0_r1i1p1_rcp45"
  v_rcp8 = variable+"_ACCESS1-0_r1i1p1_rcp85"

  packopt = True
    packopt@scale_factor = 0.1
    packopt@add_offset   = 0.0

  do i = 0,  n_years-1


      if (mode(i) .eq. -1) then

        capture_indexes = ind( (time_hist .ge. climatology_bounds(i,0))  .and.   (time_hist .le. climatology_bounds(i,1)) )

        print(ensemble(ens) + " :: " +   sprinti("%0.3i", i)+ " - " + sprinti("%2i", mode(i)) + " - (" + year_start(i) +"-"+ year_end(i)+  ") " + min(capture_indexes) + "-" + max(capture_indexes) )

        inarr         := f_hist->$v_hist$(capture_indexes,:,:)
        inarr         := int2flt(inarr)
        inarr(:,:,:)  = (/ inarr(:,:,:) * 0.1 + 0.0 /)
        inarr2        := clmMonTLL(inarr)
        inarr2        := pack_values(  inarr2, \
                                       "short", \
                                        packopt)
        t_min(i, :, 0, 0, :, :) = (/ inarr2 /)

        delete(inarr)
        delete(inarr2)

      elseif (mode(i) .eq. 1)   then


        capture_indexes = ind( (time_futr .ge. climatology_bounds(i,0))  .and.   (time_futr .le. climatology_bounds(i,1)) )

        print(ensemble(ens) + " :: " +  sprinti("%0.3i", i)+ " | " + sprinti("%2i", mode(i)) + " - (" + year_start(i) +"-"+ year_end(i)+  ") " + min(capture_indexes) + "-" + max(capture_indexes) )

        inarr             := f_rcp4->$v_rcp4$(capture_indexes,:,:)
        inarr             := int2flt(inarr)
        inarr_rcp4        := (/ inarr(:,:,:) * 0.1 + 0.0 /)

        inarr2        := clmMonTLL(inarr_rcp4)
        inarr2        := pack_values(  inarr2, \
                                       "short", \
                                        packopt)
        t_min(i, :, 0, 0, :, :) = (/ inarr2 /)

        delete(inarr)
        delete(inarr2)
        delete(inarr_rcp4)




        inarr         := f_rcp8->$v_rcp8$(capture_indexes,:,:)
        inarr         := int2flt(inarr)
        inarr_rcp8    := (/ inarr(:,:,:) * 0.1 + 0.0 /)
        inarr2        := clmMonTLL(inarr_rcp8)
        inarr2        := pack_values(  inarr2, \
                                       "short", \
                                        packopt)
        t_min(i, :, 0, 1, :, :) = (/ inarr2 /)

        delete(inarr)
        delete(inarr2)
        delete(inarr_rcp8)


      else


        capture_indexes_hist = ind( (time_hist .ge. climatology_bounds(i,0)) )
        capture_indexes_futr = ind( (time_futr .le. climatology_bounds(i,1)) )

        print(ensemble(ens) + " :: " +   sprinti("%0.3i", i)+ " * " + sprinti("%2i", mode(i)) + " - (" + year_start(i) +"-"+ year_end(i)+  ") " + min(capture_indexes_hist) + "-" + max(capture_indexes_hist) +"  ::  " + min(capture_indexes_futr) + "-" + max(capture_indexes_futr) )

        inarr             := f_hist->$v_hist$(capture_indexes_hist,:,:)
        inarr             := int2flt(inarr)
        inarr_hist        := (/ inarr(:,:,:) * 0.1 + 0.0 /)
        delete(inarr)

        inarr             := f_rcp4->$v_rcp4$(capture_indexes_futr,:,:)
        inarr             := int2flt(inarr)
        inarr_rcp4        := (/ inarr(:,:,:) * 0.1 + 0.0 /)
        delete(inarr)

        inarr             := f_rcp4->$v_rcp4$(capture_indexes_futr,:,:)
        inarr             := int2flt(inarr)
        inarr_rcp8        := (/ inarr(:,:,:) * 0.1 + 0.0 /)
        delete(inarr)


        inarr := array_append_record (inarr_hist, inarr_rcp4, 0)

        inarr2            := clmMonTLL(inarr)
        inarr2            := pack_values(inarr2, \
                                       "short", \
                                        packopt)
        t_min(i, :, 0, 0, :, :) = (/ inarr2 /)

        intarr := array_append_record (inarr_hist, inarr_rcp8, 0)

        inarr2            := clmMonTLL(inarr)
        inarr2            := pack_values(inarr2, \
                                       "short", \
                                        packopt)
        t_min(i, :, 0, 1, :, :) = (/ inarr2 /)

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

outputvar = variable+"_running_avg"

system("rm -frv " + outfile)
f_new = addfile(outfile,"c")
f_new->$outputvar$ = t_min
f_new->lon_bnds    = lon_bnds
f_new->lat_bnds    = lat_bnds
f_new->climatology_bounds = climatology_bounds
f_new->year_start         = year_start
f_new->year_end           = year_end


delete(f_new)




end
