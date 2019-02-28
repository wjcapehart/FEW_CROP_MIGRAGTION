load "./thornthwaite_budget_1d.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"


begin


   setfileoption("nc", "Format",        "NetCDF4")
   setfileoption("nc", "FileStructure", "Advanced")


   historical_url_thredds = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/1950-2005/MONTHLY/"
   future_url_thredds     = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climatology/2006-2099/MONTHLY/"



   ensemble_member = (/ "ACCESS1-0_r1i1p1", \
                        "ACCESS1-3_r1i1p1", \
                        "CCSM4_r6i1p1", \
                        "CESM1-BGC_r1i1p1", \
                        "CESM1-CAM5_r1i1p1", \
                        "CMCC-CMS_r1i1p1", \
                        "CMCC-CM_r1i1p1", \
                        "CNRM-CM5_r1i1p1", \
                        "CSIRO-Mk3-6-0_r1i1p1", \
                        "CanESM2_r1i1p1", \
                        "FGOALS-g2_r1i1p1", \
                        "GFDL-CM3_r1i1p1", \
                        "GFDL-ESM2G_r1i1p1", \
                        "GFDL-ESM2M_r1i1p1", \
                        "HadGEM2-AO_r1i1p1", \
                        "HadGEM2-CC_r1i1p1", \
                        "HadGEM2-ES_r1i1p1", \
                        "IPSL-CM5A-LR_r1i1p1", \
                        "IPSL-CM5A-MR_r1i1p1", \
                        "MIROC-ESM_r1i1p1", \
                        "MIROC-ESM-CHEM_r1i1p1", \
                        "MIROC5_r1i1p1", \
                        "MPI-ESM-LR_r1i1p1", \
                        "MPI-ESM-MR_r1i1p1", \
                        "MRI-CGCM3_r1i1p1", \
                        "NorESM1-M_r1i1p1", \
                        "bcc-csm1-1-m_r1i1p1" /)

  ensemble_member!0 = "ensemble_member"
  ensemble_member&ensemble_member = ensemble_member
  ensemble_member@long_name   = "Ensemble Member"
  ensemble_member@description = "Ensemble Member"

   baseline_storage = 150.

  n_ens = dimsizes(ensemble_member)

  e = 0
  i = 0
  j = 0

  do e = 0, n_ens-1

    precip_variable = "pr_" + ensemble_member(e) + "_historical"

    filename_pr = historical_url_thredds           + \
                  "pr/LOCA_NGP_"                   + \
                  precip_variable                  + \
                  "_1950-2005_CDO_MONTLY_TOTAL.nc"

    tasmax_variable = "tasmax_" + ensemble_member(e) + "_historical"

    filename_tx = historical_url_thredds           + \
                  "tasmax/LOCA_NGP_"                   + \
                  tasmax_variable                  + \
                  "_1950-2005_CDO_MONTLY_MEAN.nc"

    tasmin_variable = "tasmin_" + ensemble_member(e) + "_historical"

    filename_tn = historical_url_thredds           + \
                  "tasmin/LOCA_NGP_"                   + \
                  tasmin_variable                  + \
                  "_1950-2005_CDO_MONTLY_MEAN.nc"


    fpr = addfile(filename_pr,"r")
    ftx = addfile(filename_tx,"r")
    ftn = addfile(filename_tn,"r")

    mean_temperature = int2flt(ftx->$tasmax_variable$) * ftx->$tasmax_variable$@scale_factor +  ftx->$tasmax_variable$@add_offset
    tmin             = int2flt(ftn->$tasmin_variable$) * ftn->$tasmin_variable$@scale_factor +  ftn->$tasmin_variable$@add_offset
    precipitation    = int2flt(fpr->$precip_variable$) * fpr->$precip_variable$@scale_factor +  fpr->$precip_variable$@add_offset

    mean_temperature(:,:,:) = (/ mean_temperature + tmin /) / 2
      mean_temperature@cell_method   = "time: mean"
      mean_temperature@long_name     = "Mean Monthly Temperature"
      mean_temperature@description   = "Mean Monthly Temperature"
    delete(tmin)

    if (e .eq. 0) then

      lon  = fpr->lon
      lat  = fpr->lon
      time = fpr->time

      n_time = dimsizes(time)
      n_lon  = dimsizes(lon)
      n_lat  = dimsizes(lat)

      lat_bnds  = fpr->lat_bnds
      lon_bnds  = fpr->lon_bnds
      time_bnds = fpr->time_bnds



      tm_potential_evaporation   = new((/ 1, n_time, n_lat, n_lon /),float)
        tm_potential_evaporation!0 = "ensemble_member"
        tm_potential_evaporation!1 = "time"
        tm_potential_evaporation!2 = "lat"
        tm_potential_evaporation!3 = "lon"
        tm_potential_evaporation&ensemble_member = ensemble_member(e)
        tm_potential_evaporation&time            = time
        tm_potential_evaporation&lat             = lat
        tm_potential_evaporation&lon             = lon
        tm_potential_evaporation@standard_name = "water_potential_evaporation_amount"
        tm_potential_evaporation@description   = "Thorntwaite-Mather Potential Evaporation"
        tm_potential_evaporation@long_name     = "Thorntwaite-Mather Potential Evaporation"
        tm_potential_evaporation@units         = "kg m-2"
        tm_potential_evaporation@comment1      = "Thornthwaite, C.W., and Mather, J.R., 1957, Instructions and tables for computing potential evapotranspiration and the water balance: Centerton, N.J., Laboratory of Climatology, Publications in Climatology, v. 10, no. 3, p. 185–311."

      tm_precipitation = tm_potential_evaporation
          tm_precipitation@standard_name = "precipitation_amount"
          tm_precipitation@description   = "Thorntwaite-Mather Budget Precipitation"
          tm_precipitation@long_name     = "Thorntwaite-Mather Budget Precipitation"

      tm_temperature = tm_potential_evaporation
          tm_temperature@standard_name = "air_temperature"
          tm_temperature@description   = "Thorntwaite-Mather Budget Mean Temperature"
          tm_temperature@long_name     = "Thorntwaite-Mather Budget Mean Temperature"
          tm_temperature@units         = "degC"

      tm_evaporation = tm_potential_evaporation
        tm_evaporation@standard_name = "water_evaporation_amount"
        tm_evaporation@description   = "Thorntwaite-Mather Budget Evaporation"
        tm_evaporation@long_name     = "Thorntwaite-Mather Budget Evaporation"

      tm_storage = tm_potential_evaporation
        delete(tm_storage@standard_name)
        tm_storage@description  = "Thorntwaite-Mather Budget Storage"
        tm_storage@long_name    = "Thorntwaite-Mather Budget Storage"

      tm_surplus = tm_potential_evaporation
        delete(tm_surplus@standard_name)
        tm_surplus@description = "Thorntwaite-Mather Budget Surplus"
        tm_surplus@long_name   = "Thorntwaite-Mather Budget Surplus"

      tm_deficit = tm_potential_evaporation
        delete(tm_deficit@standard_name)
        tm_deficit@description = "Thorntwaite-Mather Budget Deficit"
        tm_deficit@long_name   = "Thorntwaite-Mather Budget Deficit"

      tm_recharge  = tm_potential_evaporation
        delete(tm_recharge@standard_name)
        tm_recharge@description   = "Thorntwaite-Mather Recharge"
        tm_recharge@long_name     = "Thorntwaite-Mather Recharge"

      tm_storage_change  = tm_evaporation
        delete(tm_storage_change@standard_name)
        tm_storage_change@description   = "Thorntwaite-Mather Monthly Storage Change"
        tm_storage_change@long_name     = "Thorntwaite-Mather Monthly Storage Change"
        tm_storage_change@units         = "kg m-2 month-1"

    end if

    tm_precipitation(e,:,:,:) = precipitation
    tm_temperature(e,:,:,:)   = mean_temperature

    delete(precipitation)
    delete(mean_temperature)


    do i = 0, n_lon-1

      print("historical - " + ensemble_member(e) + " row " + (i+1) + ":" + (n_lat))

      do j = 0, n_lat-1

        pcp1d  = ndtooned(tm_precipitation(e,:,j,i))
        tmp1d  = ndtooned(tm_temperature(e,:,j,i))
        sto1d  = ndtooned(tm_storage(e,:,j,i))
        dsto1d = ndtooned(tm_storage_change(e,:,j,i))
        def1d  = ndtooned(tm_deficit(e,:,j,i))
        rec1d  = ndtooned(tm_recharge(e,:,j,i))
        sur1d  = ndtooned(tm_surplus(e,:,j,i))
        evp1d  = ndtooned(tm_evaporation(e,:,j,i))
        pe1d   = ndtooned(tm_potential_evaporation(e,:,j,i))
        lat0d  = lat(i,j)

        thornthwaite_budget_1d(lat0d,            \  ; degrees_north
                               baseline_storage, \  ; kg m-1
                               tmp1d,            \  ; degC
                               pcp1d,            \  ; kg m-1
                               pe1d,             \  ; kg m-1
                               evp1d,            \  ; kg m-1
                               sto1d,            \  ; kg m-1
                               dsto1d,           \  ; kg m-1
                               def1d,            \  ; kg m-1
                               rec1d,            \  ; kg m-1
                               sur1d             )  ; kg m-1

         tm_storage(e,:,j,i)               = (/ sto1d  /)
         tm_storage_change(e,:,j,i)        = (/ dsto1d /)
         tm_deficit(e,:,j,i)               = (/ def1d  /)
         tm_recharge(e,:,j,i)              = (/ rec1d  /)
         tm_surplus(e,:,j,i)               = (/ sur1d  /)
         tm_evaporation(e,:,j,i)           = (/ evp1d  /)
         tm_potential_evaporation(e,:,j,i) = (/ pe1d   /)

      end do


    end do

    output_file = "./LOCA_NGP_THORNTHWAITE_BUDGETS_" + \
                   ensemble_member(e)                + \
                   "_1950-2005_CDO_MONTLY_VALUES.nc"

    fo = addfile(output_file,"r")

    fo->ensemble_member          = ensemble_member(e)
    fo->time                     = time
    fo->lat                      = lat
    fo->lon                      = lon
    fo->tm_temperature           = tm_temperature
    fo->tm_precipitation         = tm_precipitation
    fo->tm_potential_evaporation = tm_potential_evaporation
    fo->tm_evaporation           = tm_evaporation
    fo->tm_surplus               = tm_surplus
    fo->tm_recharge              = tm_recharge
    fo->tm_storage               = tm_storage
    fo->tm_storage_change        = tm_storage_change
    fo->tm_deficit               = tm_deficit

    delete(fo)


    last_storage = tm_storage(e,nt-1,:,:)

    delete(pcp1d)
    delete(tmp1d)
    delete(sto1d)
    delete(dsto1d)
    delete(def1d)
    delete(rec1d)
    delete(sur1d)
    delete(evp1d)
    delete(pe1d)
    delete(lat0d)

    delete(time)
    delete(lon)
    delete(lat)
    delete(time_bnds)
    delete(lon_bnds)
    delete(lat_bnds)

    delete(tm_precipitation)
    delete(tm_temperature)
    delete(tm_potential_evaporation)
    delete(tm_evaporation)
    delete(tm_surplus)
    delete(tm_recharge)
    delete(tm_storage)
    delete(tm_storage_change)
    delete(tm_deficit)

    print(" ")

end do



end
