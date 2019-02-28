
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################

# Libraries

# Tidyverse Packates




library(package = "tidyverse") # bulk frequently-used tidyverse packages
library(package = "lubridate") # tidyverse date-time support

# NetCDF/OPeNDAP support

require("ncdf4")          # netCDF file access
require("ncdf4.helpers")  # netCDF support files

# Additional Time Date Support

library(package = "PCICt") # Implementation of POSIXct Work-Alike for 365 and 360 Day Calendars



# array -> dataframe
library(package = "reshape2")



# Region

region = "LOCA_NGP"

# Ensemble Members

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



# Variables

variables      = c(               "pr",          "tasmax",          "tasmin")
variable_names = c(          "Precip",         "Max_Temp",        "Min_Temp")
statistics     = c("CDO_YEARLY_TOTAL",  "CDO_YEARLY_MEAN", "CDO_YEARLY_MEAN")

# Scenarios


scenarios  = c("historical",     "rcp45",     "rcp85")

period     = c( "1950-2005", "2006-2099", "2006-2099")


# directories

root_loc = "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/"
root_loc = "/projects/ECEP/LOCA_MACA_Ensembles/LOCA/"


#######################################################
#######################################################
#######################################################
#######################################################
#######################################################



# get histporical time coordinates

v = 2
s = 1
e = 2

variable = str_c(variables[v],
                 "_",
                 "ACCESS1-0_r1i1p1",
                 "_",
                 scenarios[s],
                 sep = "")

file_loc = str_c(root_loc,
                 region,
                 "/",
                 "climatology",
                 "/",
                 period[s],
                 "/ANNUAL/",
                 variables[v],
                 "/",
                 region,
                 "_",
                 variable,
                 "_",
                 period[s],
                 "_",
                 statistics[v],
                 ".nc",
                 sep = "")



nc.file = nc_open(filename = file_loc)  # name of the file

time_histo = nc.get.time.series(f             =  nc.file, # netCDF file handle
                                time.dim.name =   "time") # netCDF time coordinate name

year_histo = year(time_histo)

lat = ncvar_get(nc    = nc.file,
                varid =   "lat")
lon = ncvar_get(nc    = nc.file,
                varid =   "lon")

nc_close(nc = nc.file)




# get future time coordinates

v = 2
s = 2
e = 2

variable = str_c(variables[v],
                 "_",
                 "ACCESS1-0_r1i1p1",
                 "_",
                 scenarios[s],
                 sep = "")

file_loc = str_c(root_loc,
                 region,
                 "/",
                 "climatology",
                 "/",
                 period[s],
                 "/ANNUAL/",
                 variables[v],
                 "/",
                 region,
                 "_",
                 variable,
                 "_",
                 period[s],
                 "_",
                 statistics[v],
                 ".nc",
                 sep = "")

nc.file = nc_open(filename = file_loc)  # name of the file

time_rcpXX = nc.get.time.series(f             =  nc.file, # netCDF file handle
                                time.dim.name =   "time") # netCDF time coordinate name

year_rcpXX = year(time_rcpXX)


nc_close(nc = nc.file)

nt_histo = length(year_histo)
nt_rcpXX = length(year_rcpXX)
ny       = length(lat)
nx       = length(lon)
ne       = length(ensembles)


remove(time_rcpXX)
remove(time_histo)
remove(nc.file)
remove(v)
remove(s)
remove(e)

if (FALSE) {

  for (e in seq(from = 1,
                to   = ne))  {

    histo_hypercube         = array(data     = NA,
                                    dim      = c(nx,ny,nt_histo,3,1,1),
                                    dimnames = list("lon"       = lon,
                                                    "lat"       = lat,
                                                    "time"      = year_histo,
                                                    "variable"  = variable_names,
                                                    "ensemble"  = ensembles[e],
                                                    "scenario"  = c(scenarios[1]))
                                    )

    v = 1

    for (s in seq(from = 1,
                  to   = 1) ) {

      variable = str_c(variables[v],
                       "_",
                       ensembles[e],
                       "_",
                       scenarios[s],
                       sep = "")

      print(variable)

      file_loc = str_c(root_loc,
                       region,
                       "/",
                       "climatology",
                       "/",
                       period[s],
                       "/ANNUAL/",
                       variables[v],
                       "/",
                       region,
                       "_",
                       variable,
                       "_",
                       period[s],
                       "_",
                       statistics[v],
                       ".nc",
                       sep = "")

      if (file.exists(file_loc)) {

        nc.file = nc_open(filename = file_loc)  # name of the file

        histo_hypercube[ , , ,v,1,s] = ncvar_get(nc    = nc.file,
                                                 varid =  variable)

        nc_close(nc = nc.file)

      } else {
        print("--- File Missing ---")
      }

    }


    v = 2

    for (s in seq(from = 1,
                  to   = 1) ) {

      variable = str_c(variables[v],
                       "_",
                       ensembles[e],
                       "_",
                       scenarios[s],
                       sep = "")

      print(variable)

      file_loc = str_c(root_loc,
                       region,
                       "/",
                       "climatology",
                       "/",
                       period[s],
                       "/ANNUAL/",
                       variables[v],
                       "/",
                       region,
                       "_",
                       variable,
                       "_",
                       period[s],
                       "_",
                       statistics[v],
                       ".nc",
                       sep = "")

      if (file.exists(file_loc)) {

        nc.file = nc_open(filename = file_loc)  # name of the file

        histo_hypercube[ , , ,v,1,s] = ncvar_get(nc    = nc.file,
                                                 varid =  variable)

        nc_close(nc = nc.file)

      } else {
        print("--- File Missing ---")
      }

    }

    v = 3

    for (s in seq(from = 1,
                  to   = 1) ) {

      variable = str_c(variables[v],
                       "_",
                       ensembles[e],
                       "_",
                       scenarios[s],
                       sep = "")

      print(variable)

      file_loc = str_c(root_loc,
                       region,
                       "/",
                       "climatology",
                       "/",
                       period[s],
                       "/ANNUAL/",
                       variables[v],
                       "/",
                       region,
                       "_",
                       variable,
                       "_",
                       period[s],
                       "_",
                       statistics[v],
                       ".nc",
                       sep = "")

      if (file.exists(file_loc)) {

        nc.file = nc_open(filename = file_loc)  # name of the file

        histo_hypercube[ , , ,v,1,s] = ncvar_get(nc    = nc.file,
                                                 varid =  variable)

        nc_close(nc = nc.file)

      } else {
        print("--- File Missing ---")
      }

    }

    # Melt Historical HyperCube

    loca_histo   = melt(data      =  histo_hypercube,
                        id.vars   = c("lon",
                                      "lat",
                                      "time",
                                      "variable",
                                      "ensemble",
                                      "scenario"),
                        na.rm     = FALSE)

    loca_histo = spread(data =loca_histo,
                        key = c("variable"),
                        value = "value")

    remove(histo_hypercube)

    print(" ")
    print(" writing file")
    print(" ")

    save(loca_annual_histo = loca_histo,
         compress          = "gzip" ,
         compression_level = 6,
         file              = str_c(root_loc,
                                   region,
                                   "/",
                                   "climatology",
                                   "/",
                                   period[s],
                                   "/ANNUAL/Rdata/",
                                   "LOCA_NGP_ANNUAL_HISTO_",
                                   ensembles[e],
                                   ".Rdata",
                                   sep="")
         )

  }
}

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################


print("")
print("==================================")
print("")
print("")


if (TRUE) {
    for (e in seq(from = 1,
                  to   = ne))  {

      rcpXX_hypercube         = array(data     = NA,
                                      dim      = c(nx,ny,nt_rcpXX,3,1,2),
                                      dimnames = list("lon"       = lon,
                                                      "lat"       = lat,
                                                      "time"      = year_rcpXX,
                                                      "variable"  = variable_names,
                                                      "ensemble"  = ensembles[e],
                                                      "scenario"  = c(scenarios[2:3]))
                                      )

      v = 1

      for (s in seq(from = 2,
                    to   = 3) ) {

        variable = str_c(variables[v],
                         "_",
                         ensembles[e],
                         "_",
                         scenarios[s],
                         sep = "")

        print(variable)

        file_loc = str_c(root_loc,
                         region,
                         "/",
                         "climatology",
                         "/",
                         period[s],
                         "/ANNUAL/",
                         variables[v],
                         "/",
                         region,
                         "_",
                         variable,
                         "_",
                         period[s],
                         "_",
                         statistics[v],
                         ".nc",
                         sep = "")

        if (file.exists(file_loc)) {

          nc.file = nc_open(filename = file_loc)  # name of the file

          rcpXX_hypercube[ , , ,v,1,s-1]   = ncvar_get(nc    = nc.file,
                                                     varid =  variable)

          nc_close(nc = nc.file)

        } else {
          print("--- File Missing ---")
        }

      }

      v = 2

      for (s in seq(from = 2,
                    to   = 3) ) {

        variable = str_c(variables[v],
                         "_",
                         ensembles[e],
                         "_",
                         scenarios[s],
                         sep = "")

         print(variable)
        file_loc = str_c(root_loc,
                         region,
                         "/",
                         "climatology",
                         "/",
                         period[s],
                         "/ANNUAL/",
                         variables[v],
                         "/",
                         region,
                         "_",
                         variable,
                         "_",
                         period[s],
                         "_",
                         statistics[v],
                         ".nc",
                         sep = "")


        if (file.exists(file_loc)) {

          nc.file = nc_open(filename = file_loc)  # name of the file

          rcpXX_hypercube[ , , ,v,1,s-1]   = ncvar_get(nc    = nc.file,
                                                     varid =  variable)

          nc_close(nc = nc.file)

        } else {
          print("--- File Missing ---")
        }


      }

      v = 3

      for (s in seq(from = 2,
                    to   = 3) ) {

        variable = str_c(variables[v],
                         "_",
                         ensembles[e],
                         "_",
                         scenarios[s],
                         sep = "")

        print(variable)

        file_loc = str_c(root_loc,
                         region,
                         "/",
                         "climatology",
                         "/",
                         period[s],
                         "/ANNUAL/",
                         variables[v],
                         "/",
                         region,
                         "_",
                         variable,
                         "_",
                         period[s],
                         "_",
                         statistics[v],
                         ".nc",
                         sep = "")

        if (file.exists(file_loc)) {

          nc.file = nc_open(filename = file_loc)  # name of the file

          rcpXX_hypercube[ , , ,v,1,s-1] = ncvar_get(nc    = nc.file,
                                                   varid =  variable)

          nc_close(nc = nc.file)

        } else {
          print("--- File Missing ---")
        }

      }

      # Melt Fiture HyperCube

      loca_rcpXX   = melt(data      =  rcpXX_hypercube,
                          id.vars   = c("lon",
                                        "lat",
                                        "time",
                                        "variable",
                                        "ensemble",
                                        "scenario"),
                          na.rm     = FALSE)

      loca_rcpXX = spread(data =loca_rcpXX,
                          key = c("variable"),
                          value = "value")

      remove(rcpXX_hypercube)

      print(" ")
      print(" writing file")
      print(" ")
      save(loca_annual_rcpXX = loca_rcpXX,
           compress          = "gzip" ,
           compression_level = 6,
           file              = str_c(root_loc,
                                     region,
                                     "/",
                                     "climatology",
                                     "/",
                                     period[s],
                                     "/ANNUAL/Rdata/",
                                     "LOCA_NGP_ANNUAL_RCPxx_",
                                     ensembles[e],
                                     ".Rdata",
                                     sep="")
           )

    }
}

print("--")
print("DONE")

remove(v)
remove(s)
remove(e)
