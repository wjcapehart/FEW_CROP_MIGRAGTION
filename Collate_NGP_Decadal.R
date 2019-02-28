# Libraries

  # Tidyverse Packates

  library(package = "tidyverse") # bulk frequently-used tidyverse packages
  library(package = "lubridate") # tidyverse date-time support

  # array -> dataframe
  library(package = "reshape2")

# 3 Scenerio Information


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
                 "MIROC-ESM-CHEM_r1i1p1",
                 "MIROC5_r1i1p1",
                 "MPI-ESM-LR_r1i1p1",
                 "MPI-ESM-MR_r1i1p1",
                 "MRI-CGCM3_r1i1p1",
                 "NorESM1-M_r1i1p1",
                 "bcc-csm1-1-m_r1i1p1" )


# 4 LOCA Files, Directory and OPeNDAP URLS (includes coordinate extraction)

  # directories

  root_loc = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/"
  root_loc = "/projects/ECEP/LOCA_MACA_Ensembles/LOCA/"
  
  interval = 10

for (decade in seq(from =     1950,
                   to   =     2090,
                   by   = interval)) {
  
    decade_name = str_c(decade,
                        "-",
                        (decade + interval-1),
                        seq = "")
  
    print(decade_name)
 



    for (ensemble in ensembles) {

      print(str_c(".. ",ensemble))
      
      if (decade < 2000) {
        
          ####  Loading data for Historical Cases
      
          print(".... historical")
        
          loca_file_name = str_c(root_loc,
                                 region,
                                 "/",
                                 "climatology",
                                 "/",
                                 "1950-2005",
                                 "/ANNUAL/Rdata/",
                                 "LOCA_NGP_ANNUAL_HISTO_",
                                 ensemble,
                                 ".Rdata",
                                 sep="")

          load(file = (loca_file_name))
          
          loca_histo = as.tibble(loca_histo)
          

          loca_full = filter(loca_histo, (time >= decade) & 
                                         (time <  decade+interval) )
          

          
          remove(loca_histo)
          

          
          loca_full$scenario = "Historical"
          
          loca_full$scenario = as.character(loca_full$scenario)
          loca_full$ensemble = as.character(loca_full$ensemble)         

        } else if (decade > 2000 )  {
        
          ####  Loading data for Future Cases
          
          print(".... future")
          
          loca_file_name = str_c(root_loc,
                                 region,
                                 "/",
                                 "climatology",
                                 "/",
                                 "2006-2099",
                                 "/ANNUAL/Rdata/",
                                 "LOCA_NGP_ANNUAL_RCPxx_",
                                 ensemble,
                                 ".Rdata",
                                 sep = "")
          
          load(file = (loca_file_name))
          
          loca_rcpXX = as.tibble(loca_rcpXX)
 
          loca_full = loca_rcpXX %>% filter( (time >= decade) & 
                                             (time <  decade+interval) )
          
          loca_full$scenario = as.character(loca_full$scenario)
          loca_full$ensemble = as.character(loca_full$ensemble)
          
          loca_full$scenario[loca_full$scenario == "rcp85"] = "RCP 8.5"
          loca_full$scenario[loca_full$scenario == "rcp45"] = "RCP 4.5"
          
          remove(loca_rcpXX)
          
        }  else {
          
          ####  Loading data for Historical Cases
        
          print(".... historical")
          loca_file_name = str_c(root_loc,
                                 region,
                                 "/",
                                 "climatology",
                                 "/",
                                 "1950-2005",
                                 "/ANNUAL/Rdata/",
                                 "LOCA_NGP_ANNUAL_HISTO_",
                                 ensemble,
                                 ".Rdata",
                                 sep="")
          
          load(file = (loca_file_name))
          
          loca_histo = as.tibble(loca_histo)
          
          loca_histo$scenario = as.character(loca_histo$scenario)
          loca_histo$ensemble = as.character(loca_histo$ensemble)
          
          loca_histo = loca_histo %>% filter( (time >= decade) & 
                                              (time <  decade+interval) )  
          
          loca_hist2 = loca_histo
          
          loca_hist2$scenario  = "RCP 8.5"
          loca_histo$scenario  = "RCP 4.5"
          
          ####  Loading data for Future Cases
          
          print(".... future")
          
          loca_file_name = str_c(root_loc,
                                 region,
                                 "/",
                                 "climatology",
                                 "/",
                                 "2006-2099",
                                 "/ANNUAL/Rdata/",
                                 "LOCA_NGP_ANNUAL_RCPxx_",
                                 ensemble,
                                 ".Rdata",
                                 sep="")
          
          load(file = (loca_file_name))
          
          loca_rcpXX$scenario = as.character(loca_rcpXX$scenario)
          loca_rcpXX$ensemble = as.character(loca_rcpXX$ensemble)
          
          loca_rcpXX = loca_rcpXX %>% filter( (time >= decade) &
                                              (time <  decade+interval) )   
          
          loca_rcpXX$scenario[loca_rcpXX$scenario == "rcp85"] = "RCP 8.5"
          loca_rcpXX$scenario[loca_rcpXX$scenario == "rcp45"] = "RCP 4.5"

          loca_full = bind_rows(loca_histo,
                                loca_hist2,
                                loca_rcpXX)
          
          remove(loca_histo)
          remove(loca_hist2)
          remove(loca_rcpXX)
          
          
                  
        }
      
        ####

        loca_full$time     = floor(loca_full$time/10)*10
        loca_full$Avg_Temp = (loca_full$Min_Temp + loca_full$Max_Temp) / 2.0

        print(".... aggregating current ensemble")

        loca_agg  =  loca_full %>%
                     group_by(lon,
                              lat,
                              time,
                              scenario,
                              ensemble) %>%
                     summarise(Precip   = mean(Precip,   na.rm=FALSE),
                               Max_Temp = mean(Max_Temp, na.rm=FALSE),
                               Min_Temp = mean(Min_Temp, na.rm=FALSE),
                               Avg_Temp = mean(Avg_Temp, na.rm=FALSE))

        remove(loca_full)

        if (ensemble == ensembles[1]) {
          print(".... full member group field 'loca'")
          loca = loca_agg
        } else {
          print(".... adding current ensemble to rest of members")
          loca = bind_rows(loca, loca_agg)
        }

        remove(loca_agg)

    }
    
    colnames(loca)[colnames(loca) == "time"] = "decade"
    loca$decade = decade_name
    
    loca$decade   = as.factor(loca$decade)
    loca$scenario = as.factor(loca$scenario)
    loca$ensemble = as.factor(loca$ensemble)
    
    output_file = str_c(root_loc,
                        region,
                        "/climatology/",
                        "DECADALS/Rdata/",
                        "LOCA_NGP_DECADAL_ALL_MEMBERS_",
                        decade_name,
                        ".Rdata",
                        sep="")

    save(loca_decadal    = loca,
       compress          = "gzip" ,
       compression_level = 6,
       file              = output_file)
    
    remove(loca)
    
  }
