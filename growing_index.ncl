


begin

  period     = 30
  start_year = 1950
  end_year   = 2099

  print(2005 - period + 1 - start_year + 1)



    ensembles = (/ "ACCESS1-0_r1i1p1",  \
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

  n_ens = dimsizes(ensembles)
  ensembles!0 = "ensembles"
  ensembles&ensembles = ensembles


  f_hist = addfile("/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/1950-2005/MONTHLY/tasmin/LOCA_NGP_tasmin_ACCESS1-0_r1i1p1_historical_1950-2005_CDO_MONTLY_MEAN.nc","r")
  f_rcp4 = addfile("/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/2006-2099/MONTHLY/tasmin/LOCA_NGP_tasmin_ACCESS1-0_r1i1p1_rcp45_2006-2099_CDO_MONTLY_MEAN.nc","r")
  f_rcp8 = addfile("/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/2006-2099/MONTHLY/tasmin/LOCA_NGP_tasmin_ACCESS1-0_r1i1p1_rcp48_2006-2099_CDO_MONTLY_MEAN.nc","r")

  lon      = f_hist->lon
  lat      = f_hist->lat
  lon_bnds = f_hist->lon_bnds
  lat_bnds = f_hist->lat_bnds

  n_lon = dimsizes(lon)
  n_lat = dimsizes(lat)

  year_start = ispan(1951, 2066, 5)
  year_end   = ispan(1980, 2095, 5)


  time_units = "days since 1950-01-01 00:00:00"

  nt = dimsizes(year_start)


  time          = new(nt, "integer")
  time@units    = time_units
  time@calendar = "standard"

  time_bnds          = new((/ nt, 2 /),"integer")
  time_bnds@units    = time_units
  time_bnds@calendar = "standard"




  do i = 0, nt-1

    print(i+ "|"+ year_start(i) + "-" + year_end(i))

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

  print(time)
  print(time_bnds)


  t_min_average = new((/  /), "float")



end
